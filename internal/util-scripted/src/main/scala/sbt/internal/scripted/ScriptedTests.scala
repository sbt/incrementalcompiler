package sbt
package internal
package scripted

import java.io.File
import sbt.util.Logger
import sbt.internal.util.{ ConsoleLogger, BufferedLogger, FullLogger }
import sbt.io.IO.wrapNull
import sbt.io.{ DirectoryFilter, HiddenFileFilter, Path, GlobFilter }
import sbt.internal.io.Resources

object ScriptedRunnerImpl {
  def run(resourceBaseDirectory: File, bufferLog: Boolean, tests: Array[String], handlersProvider: HandlersProvider): Unit = {
    val runner = new ScriptedTests(resourceBaseDirectory, bufferLog, handlersProvider)
    val logger = ConsoleLogger()
    val allTests = get(tests, resourceBaseDirectory, logger) flatMap {
      case ScriptedTest(group, name) =>
        runner.scriptedTest(group, name, logger)
    }
    runAll(allTests)
  }
  def runAll(tests: Seq[() => Option[String]]): Unit = {
    val errors = for (test <- tests; err <- test()) yield err
    if (errors.nonEmpty)
      sys.error(errors.mkString("Failed tests:\n\t", "\n\t", "\n"))
  }
  def get(tests: Seq[String], baseDirectory: File, log: Logger): Seq[ScriptedTest] =
    if (tests.isEmpty) listTests(baseDirectory, log) else parseTests(tests)
  def listTests(baseDirectory: File, log: Logger): Seq[ScriptedTest] =
    (new ListTests(baseDirectory, _ => true, log)).listTests
  def parseTests(in: Seq[String]): Seq[ScriptedTest] =
    for (testString <- in) yield {
      val Array(group, name) = testString.split("/").map(_.trim)
      ScriptedTest(group, name)
    }
}

final class ScriptedTests(resourceBaseDirectory: File, bufferLog: Boolean, handlersProvider: HandlersProvider) {
  // import ScriptedTests._
  private val testResources = new Resources(resourceBaseDirectory)

  val ScriptFilename = "test"
  val PendingScriptFilename = "pending"

  def scriptedTest(group: String, name: String, log: xsbti.Logger): Seq[() => Option[String]] =
    scriptedTest(group, name, Logger.xlog2Log(log))
  def scriptedTest(group: String, name: String, log: Logger): Seq[() => Option[String]] =
    scriptedTest(group, name, { _ => () }, log)
  def scriptedTest(group: String, name: String, prescripted: File => Unit, log: Logger): Seq[() => Option[String]] = {
    import Path._
    import GlobFilter._
    var failed = false
    for (groupDir <- (resourceBaseDirectory * group).get; nme <- (groupDir * name).get) yield {
      val g = groupDir.getName
      val n = nme.getName
      val str = s"$g / $n"
      () => {
        println("Running " + str)
        testResources.readWriteResourceDirectory(g, n) { testDirectory =>
          val disabled = new File(testDirectory, "disabled").isFile
          if (disabled) {
            log.info("D " + str + " [DISABLED]")
            None
          } else {
            try { scriptedTest(str, testDirectory, prescripted, log); None }
            catch { case _: TestException | _: PendingTestSuccessException => Some(str) }
          }
        }
      }
    }
  }

  private def scriptedTest(label: String, testDirectory: File, prescripted: File => Unit, log: Logger): Unit =
    {
      val buffered = new BufferedLogger(new FullLogger(log))
      if (bufferLog)
        buffered.record()

      def createParser() =
        {
          // val fileHandler = new FileCommands(testDirectory)
          // // val sbtHandler = new SbtHandler(testDirectory, launcher, buffered, launchOpts)
          // new TestScriptParser(Map('$' -> fileHandler, /* '>' -> sbtHandler, */ '#' -> CommentHandler))
          val scriptConfig = new ScriptConfig(label, testDirectory, buffered)
          new TestScriptParser(handlersProvider getHandlers scriptConfig)
        }
      val (file, pending) = {
        val normal = new File(testDirectory, ScriptFilename)
        val pending = new File(testDirectory, PendingScriptFilename)
        if (pending.isFile) (pending, true) else (normal, false)
      }
      val pendingString = if (pending) " [PENDING]" else ""

      def runTest() =
        {
          val run = new ScriptRunner
          val parser = createParser()
          run(parser.parse(file))
        }
      def testFailed(): Unit = {
        if (pending) buffered.clear() else buffered.stop()
        buffered.error("x " + label + pendingString)
      }

      try {
        prescripted(testDirectory)
        runTest()
        buffered.info("+ " + label + pendingString)
        if (pending) throw new PendingTestSuccessException(label)
      } catch {
        case e: TestException =>
          testFailed()
          e.getCause match {
            case null | _: java.net.SocketException => buffered.error("   " + e.getMessage)
            case _                                  => if (!pending) e.printStackTrace
          }
          if (!pending) throw e
        case e: PendingTestSuccessException =>
          testFailed()
          buffered.error("  Mark as passing to remove this failure.")
          throw e
        case e: Exception =>
          testFailed()
          if (!pending) throw e
      } finally { buffered.clear() }
    }
}

// object ScriptedTests extends ScriptedRunner {
//   val emptyCallback: File => Unit = { _ => () }
// }

final case class ScriptedTest(group: String, name: String) {
  override def toString = group + "/" + name
}

object ListTests {
  def list(directory: File, filter: java.io.FileFilter) = wrapNull(directory.listFiles(filter))
}
import ListTests._
final class ListTests(baseDirectory: File, accept: ScriptedTest => Boolean, log: Logger) {
  def filter = DirectoryFilter -- HiddenFileFilter
  def listTests: Seq[ScriptedTest] =
    {
      list(baseDirectory, filter) flatMap { group =>
        val groupName = group.getName
        listTests(group).map(ScriptedTest(groupName, _))
      }
    }
  private[this] def listTests(group: File): Seq[String] =
    {
      val groupName = group.getName
      val allTests = list(group, filter).sortBy(_.getName)
      if (allTests.isEmpty) {
        log.warn("No tests in test group " + groupName)
        Seq.empty
      } else {
        val (included, skipped) = allTests.toList.partition(test => accept(ScriptedTest(groupName, test.getName)))
        if (included.isEmpty)
          log.warn("Test group " + groupName + " skipped.")
        else if (skipped.nonEmpty) {
          log.warn("Tests skipped in group " + group.getName + ":")
          skipped.foreach(testName => log.warn(" " + testName.getName))
        }
        Seq(included.map(_.getName): _*)
      }
    }
}

class PendingTestSuccessException(label: String) extends Exception {
  override def getMessage: String =
    s"The pending test $label succeeded. Mark this test as passing to remove this failure."
}