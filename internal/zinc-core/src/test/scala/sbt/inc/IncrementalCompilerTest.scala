package sbt
package internal
package inc

import java.io.File
import scala.compat.Platform.EOL

import sbt.io.{ IO, Path }
import xsbt.{ CompilationFailedException, TestAnalyzingCompiler }

import org.scalatest.exceptions.TestPendingException

object IncrementalCompilerTest {
  implicit class withPending(result: => Unit) {
    def pending(): Unit =
      try {
        result
        throw PassingPendingScenarioException
      } catch {
        case _: FailedStepException => throw new TestPendingException
      }
  }
  implicit class FileOP(val content: String)
  case object delete extends FileOP("")

  object PassingPendingScenarioException extends Exception("""This scenario is marked as pending but has been successfully executed.
                                                             |Mark this scenario as passing to clear this failure.""".stripMargin)

  /** Exception thrown when a step has failed. */
  case class FailedStepException(override val getMessage: String) extends Exception

  object FailedStepException {
    private def show(pairs: Seq[(String, String)]): String = {
      pairs map {
        case (name, content) =>
          val startLength = 5
          val formattedContent = content.lines.toList match {
            case Nil      => ""
            case x :: Nil => x
            case x :: xs  => x + EOL + (xs map (l => " " * ((name.length + startLength + 4)) + l)).mkString(EOL)
          }
          " " * startLength + s"$name -> $formattedContent"
      } mkString EOL
    }

    private def showFiles(state: ScenarioState): String = show(state.files.toSeq)
    private def showChanges(state: ScenarioState): String = {
      val changes = state.lastChanges map {
        case (name, `delete`) => (name, "deleted")
        case (name, content)  => (name, content.content)
      }
      show(changes)
    }

    def apply(state: ScenarioState, step: Step, cause: String): FailedStepException = FailedStepException(
      s"""Scenario failed at step '${step.description}':
        |  $cause
        |State at the time of failure:
        |  Directory: ${state.directory.getAbsolutePath}
        |  Files:
        |${showFiles(state)}
        |  lastChanges:
        |${showChanges(state)}
        """.stripMargin
    )
  }

  /**
   * The current state of the scenario.
   */
  case class ScenarioState private (
    compiler: TestAnalyzingCompiler,
    directory: File,
    files: Map[String, String],
    lastChanges: Seq[(String, FileOP)],
    previous: Option[ScenarioState],
    previousIsFailing: Boolean
  ) {

    /**
     * Modifies the current state by applying the different file changes described
     * in `fileChanges`
     */
    def performFileChanges(fileChanges: (String, FileOP)*): ScenarioState = {
      val newFiles =
        (fileChanges foldLeft files) {
          case (files, (fileName, `delete`)) =>
            IO.delete(new File(directory, fileName))
            files - fileName
          case (files, (fileName, content)) =>
            IO.write(new File(directory, fileName), content.content)
            files + (fileName -> content.content)
        }

      val previousChanges =
        if (previousIsFailing) previous map (_.lastChanges) getOrElse Nil filterNot (f => fileChanges exists (c => c._1 == f._1))
        else Nil

      if (fileChanges.nonEmpty || previousIsFailing)
        this.copy(files = newFiles, lastChanges = fileChanges ++ previousChanges)
      else this
    }

    /** Marks the files `fs` as changed. */
    def markChanged(fs: String*): ScenarioState = {
      val changes = fs map (f => (f, new FileOP(files(f))))
      this performFileChanges (changes: _*)
    }

    /** The same state with no file marked as changed. */
    def noChanges: ScenarioState = copy(lastChanges = Nil)

    def markFailing: ScenarioState = copy(previousIsFailing = true)

    private[this] def copy(
      compiler: TestAnalyzingCompiler = compiler,
      directory: File = directory,
      files: Map[String, String] = files,
      lastChanges: Seq[(String, FileOP)] = lastChanges,
      previousIsFailing: Boolean = false
    ): ScenarioState =
      new ScenarioState(compiler, directory, files, lastChanges, Some(this), previousIsFailing)
  }
  object ScenarioState {
    /** Creates an initial state. */
    def apply(compiler: TestAnalyzingCompiler, directory: File, files: Map[String, String]): ScenarioState =
      new ScenarioState(compiler, directory, files, Nil, None, false)
  }

  /**
   * Represent a scenario, whose first step is `step`.
   */
  class Scenario(step: Step) {

    /**
     * Marks this scenario as pending. If the scenario succeeds, this will be
     * reported as a failure.
     */
    def pending: Scenario = new Scenario(step) {
      override def execute(compiler: TestAnalyzingCompiler): Unit =
        try {
          super.execute(compiler)
          throw PassingPendingScenarioException
        } catch {
          case _: FailedStepException => throw new TestPendingException
        }
    }

    /** Executes all the steps of this scenario using `compiler`. */
    def execute(compiler: TestAnalyzingCompiler): Unit =
      IO.withTemporaryDirectory { dir =>
        val initial = ScenarioState(compiler, dir, Map.empty)
        step.execute(initial)
        ()
      }
  }
  object Scenario {
    /** Creates a scenario composed of the given steps. */
    def apply(steps: Step*): Scenario = {
      if (steps.isEmpty) EmptyScenario
      else new Scenario(steps reduce (_ andThen _))
    }
  }

  /** A scenario without any step. */
  object EmptyScenario extends Scenario(EmptyStep)

  /** Represents a step of the scenario. */
  abstract class Step(val description: String) {
    /**
     * Composes this step with `second`, so that this step is executed and then
     * `second` is executed.
     */
    final def andThen(second: Step): CombinedStep = CombinedStep(this, second)

    /** Executes this step */
    def execute(state: ScenarioState): ScenarioState
  }

  /** A step that performs no action. */
  object EmptyStep extends Step("Empty step") {
    override def execute(state: ScenarioState): ScenarioState = state
  }

  /**
   * A step composed of two substeps. `first` is executed, and then `second` is executed.
   */
  case class CombinedStep(first: Step, second: Step) extends Step(s"${first.description}, and then ${second.description}") {
    override def execute(state: ScenarioState): ScenarioState =
      second execute (first execute state)
  }

  /**
   * A step that runs incremental compilation steps until nothing more needs to be compiled,
   * and succeeds iff this operation took `expectedSteps` steps.
   */
  case class FullCompilation(expectedSteps: Int, fileChanges: (String, FileOP)*)
    extends Step(s"Full compilation in $expectedSteps expected steps (${fileChanges.length} changes)") {

    override def execute(state: ScenarioState): ScenarioState = {
      val newState = state.performFileChanges(fileChanges: _*)

      def compileUntilFinished(state: ScenarioState, stepsCount: Int): Unit = {
        val invalidated =
          try state.compiler.incrementalStep(state)
          catch { case e: CompilationFailedException => throw FailedStepException(state, this, "Compilation failed: " + e) }
        if (invalidated.nonEmpty && stepsCount > 0)
          compileUntilFinished(state.markChanged((invalidated.toSeq map (_.getName)): _*), stepsCount - 1)
        else {
          if (stepsCount != 1) {
            val message =
              if (stepsCount == 0) s"Compilation didn't finish after the expected number of steps ($expectedSteps)."
              else s"Compilation finished before expected number of steps (took ${expectedSteps - stepsCount} instead of $expectedSteps)."
            throw FailedStepException(state, this, message)
          }
        }
      }

      compileUntilFinished(newState, expectedSteps)
      newState
    }

  }

  /**
   * A step that performs one step of the incremental compiler.
   */
  case class IncrementalStep(fileChanges: (String, FileOP)*)
    extends Step(s"Single incremental compilation step (${fileChanges.length} changes)") {

    override def execute(state: ScenarioState): ScenarioState = {
      val newState = state.performFileChanges(fileChanges: _*)

      try newState.compiler.incrementalStep(newState)
      catch {
        case e: CompilationFailedException =>
          throw FailedStepException(newState, this, "Compilation failed: " + e)
      }

      newState
    }

    /**
     * Verifies that after running this incremental step, all the files in `invalidated` have been
     * invalidated.
     */
    def invalidates(invalidated: String*): IncrementalStep = {
      val parent = this
      new IncrementalStep(fileChanges: _*) {
        override def execute(state: ScenarioState): ScenarioState = {
          val stateAfterCompilation = parent.execute(state)
          val invalidatedClasses = stateAfterCompilation.compiler.computeInvalidations(stateAfterCompilation)
          val analysisAfterCompilation = stateAfterCompilation.compiler.getAnalyses.head
          val invalidatedFiles = invalidatedClasses.flatMap(analysisAfterCompilation.relations.definesClass) map (_.getName)

          if (invalidatedFiles != invalidated.toSet) {
            val message = s"""Invalidated files didn't match expected invalidations.
                             |Expected:    ${invalidated.toSet mkString ", "}
                             |Invalidated: ${invalidatedFiles mkString ", "}""".stripMargin
            throw FailedStepException(stateAfterCompilation, this, message)
          } else {
            stateAfterCompilation
          }
        }
      }
    }
  }

  /**
   * Runs the incremental compiler until the compilation fails or nothing else needs recompilation.
   * This step fails if the compilation succeeds.
   */
  case class FailedCompile(fileChanges: (String, FileOP)*) extends Step(s"Failing compilation step (${fileChanges.length} changes)") {
    override def execute(state: ScenarioState): ScenarioState = {
      val newState = state.performFileChanges(fileChanges: _*)

      def compileUntilFinished(state: ScenarioState): Unit = {
        val invalidated = state.compiler.incrementalStep(state)
        if (invalidated.nonEmpty)
          compileUntilFinished(state.markChanged((invalidated.toSeq map (_.getName)): _*))
      }

      try {
        compileUntilFinished(newState)
        throw FailedStepException(newState, this, "Compilation succeeded, but failure was expected.")
      } catch {
        case _: CompilationFailedException => newState.markFailing
      }

    }
  }

  /** A step that can be used to perform logging. */
  case class LoggingStep(op: ScenarioState => Unit) extends Step("Logging step") {
    override def execute(state: ScenarioState): ScenarioState = {
      op(state)
      state
    }
  }

  /**
   * Cleans the generated classfiles and resets the incremental compiler.
   */
  object Clean extends Step("Cleaning step") {
    override def execute(state: ScenarioState): ScenarioState = {
      state.compiler.clean(state)
      state.markChanged(state.files.keys.toSeq: _*)
    }
  }
}
