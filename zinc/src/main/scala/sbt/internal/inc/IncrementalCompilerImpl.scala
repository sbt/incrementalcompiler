package sbt
package internal
package inc

import sbt.internal.inc.javac.JavaTools
import xsbti.{ Position, Logger, Maybe, Reporter, F1, T2 }
import xsbti.compile.{ CompileOrder, GlobalsCache, IncOptions, MiniSetup, CompileAnalysis, CompileResult, CompileOptions }
import xsbti.compile.{ PreviousResult, Setup, Inputs, IncrementalCompiler, PerClasspathEntryLookup, FileWatch }
import xsbti.compile.{ Compilers, CompileProgress, JavaCompiler, JavaTools => XJavaTools, Output, ScalaCompiler, ClasspathOptions => XClasspathOptions }
import java.io.File
import sbt.util.Logger.m2o
import sbt.io.{ IO, Using }
import xsbti.compile.CompileOrder.Mixed

// TODO -
//  1. Move analyzingCompile from MixedAnalyzingCompiler into here
//  2. Create AnalyzingJavaComiler class
//  3. MixedAnalyzingCompiler should just provide the raw 'compile' method used in incremental compiler (and
//     by this class.

class IncrementalCompilerImpl extends IncrementalCompiler {
  override def compile(in: Inputs, log: Logger): CompileResult =
    {
      val cs = in.compilers()
      val config = in.options()
      val setup = in.setup()
      import cs._
      import config._
      import setup._
      val javacChosen = in.compilers.javaTools.javac
      val scalac = in.compilers.scalac
      incrementalCompile(scalac, javacChosen, sources, classpath, CompileOutput(classesDirectory), cache, None, scalacOptions, javacOptions,
        m2o(in.previousResult.analysis),
        m2o(in.previousResult.setup),
        perClasspathEntryLookup, fileWatch,
        reporter, order, skip, incrementalCompilerOptions,
        extra.toList map { x => (x.get1, x.get2) })(log)
    }

  /**
   * This will run a mixed-compilation of Java/Scala sources
   *
   * @param scalac  An instances of the Scalac compiler which can also extract "Analysis" (dependencies)
   * @param javac  An instance of the Javac compiler.
   * @param sources  The set of sources to compile
   * @param classpath  The classpath to use when compiling.
   * @param output  Configuration for where to output .class files.
   * @param cache   The caching mechanism to use instead of insantiating new compiler instances.
   * @param progress  Progress listening for the compilation process.  TODO - Feed this through the Javac Compiler!
   * @param options   Options for the Scala compiler
   * @param javacOptions  Options for the Java compiler
   * @param previousAnalysis  The previous dependency Analysis object/
   * @param previousSetup  The previous compilation setup (if any)
   * @param reporter  Where we sent all compilation error/warning events
   * @param compileOrder  The order we'd like to mix compilation.  JavaThenScala, ScalaThenJava or Mixed.
   * @param skip  IF true, we skip compilation and just return the previous analysis file.
   * @param incrementalCompilerOptions  Options specific to incremental compilation.
   * @param log  The location where we write log messages.
   * @return  The full configuration used to instantiate this mixed-analyzing compiler, the set of extracted dependencies and
   *          whether or not any file were modified.
   */
  def incrementalCompile(
    scalac: xsbti.compile.ScalaCompiler,
    javac: xsbti.compile.JavaCompiler,
    sources: Seq[File],
    classpath: Seq[File],
    output: Output,
    cache: GlobalsCache,
    progress: Option[CompileProgress] = None,
    options: Seq[String] = Nil,
    javacOptions: Seq[String] = Nil,
    previousAnalysis: Option[CompileAnalysis],
    previousSetup: Option[MiniSetup],
    perClasspathEntryLookup: PerClasspathEntryLookup,
    fileWatch: FileWatch,
    reporter: Reporter,
    compileOrder: CompileOrder = Mixed,
    skip: Boolean = false,
    incrementalCompilerOptions: IncOptions,
    extra: List[(String, String)]
  )(implicit log: Logger): CompileResult = {
    val prev = previousAnalysis match {
      case Some(previous) => previous
      case None           => Analysis.empty(incrementalCompilerOptions.nameHashing)
    }
    val config = MixedAnalyzingCompiler.makeConfig(scalac, javac, sources, classpath, output, cache,
      progress, options, javacOptions, prev, previousSetup, perClasspathEntryLookup, fileWatch, reporter,
      compileOrder, skip, incrementalCompilerOptions, extra)
    if (skip) new CompileResult(prev, config.currentSetup, false)
    else {
      val (analysis, changed) = compileInternal(
        MixedAnalyzingCompiler(config)(log),
        MiniSetupUtil.equivCompileSetup, MiniSetupUtil.equivPairs, log
      )
      new CompileResult(analysis, config.currentSetup, changed)
    }
  }

  private class LookupImpl(compileConfiguration: CompileConfiguration) extends Lookup {
    private val entry = MixedAnalyzingCompiler.classPathLookup(compileConfiguration)

    override def lookupOnClasspath(binaryClassName: String): Option[File] =
      entry(binaryClassName)
    override def lookupAnalysis(classFile: File): Option[CompileAnalysis] =
      m2o(compileConfiguration.perClasspathEntryLookup.analysis(classFile))

    override def lookupAnalysis(binaryDependency: File, binaryClassName: String): Option[CompileAnalysis] = {
      lookupOnClasspath(binaryClassName) flatMap { defines =>
        if (binaryDependency != Locate.resolve(defines, binaryClassName))
          None
        else
          lookupAnalysis(defines)
      }
    }

    override def lookupAnalysis(binaryClassName: String): Option[CompileAnalysis] =
      lookupOnClasspath(binaryClassName).flatMap(lookupAnalysis)
  }

  /** Actually runs the incremental compiler using the given mixed compiler.  This will prune the inputs based on the MiniSetup. */
  private def compileInternal(
    mixedCompiler: MixedAnalyzingCompiler,
    equiv: Equiv[MiniSetup],
    equivPairs: Equiv[Array[T2[String, String]]],
    log: Logger
  ): (Analysis, Boolean) = {
    val lookup = new LookupImpl(mixedCompiler.config)
    import mixedCompiler.config._
    import mixedCompiler.config.currentSetup.output
    val sourcesSet = sources.toSet
    val analysis = previousSetup match {
      case Some(previous) if !equivPairs.equiv(previous.extra, currentSetup.extra) =>
        // if the values of extra has changed we have to throw away
        // previous Analysis completely and start with empty Analysis object.
        Analysis.empty(currentSetup.nameHashing)
      case Some(previous) if previous.nameHashing != currentSetup.nameHashing =>
        // if the value of `nameHashing` flag has changed we have to throw away
        // previous Analysis completely and start with empty Analysis object
        // that supports the particular value of the `nameHashing` flag.
        // Otherwise we'll be getting UnsupportedOperationExceptions
        Analysis.empty(currentSetup.nameHashing)
      case Some(previous) if equiv.equiv(previous, currentSetup) => previousAnalysis
      case _ => Incremental.prune(sourcesSet, previousAnalysis)
    }
    // Run the incremental compiler using the mixed compiler we've defined.
    IncrementalCompile(sourcesSet, lookup, fileWatch, mixedCompiler.compile, analysis, output, log, incOptions).swap
  }

  def setup(perClasspathEntryLookup: PerClasspathEntryLookup, fileWatch: FileWatch,
    skip: Boolean, cacheFile: File, cache: GlobalsCache,
    incrementalCompilerOptions: IncOptions, reporter: Reporter, extra: Array[T2[String, String]]): Setup =
    new Setup(perClasspathEntryLookup, fileWatch, skip, cacheFile, cache, incrementalCompilerOptions, reporter, extra)
  def inputs(options: CompileOptions, compilers: Compilers, setup: Setup, pr: PreviousResult): Inputs =
    new Inputs(compilers, options, setup, pr)
  def inputs(classpath: Array[File], sources: Array[File], classesDirectory: File, scalacOptions: Array[String],
    javacOptions: Array[String], maxErrors: Int, sourcePositionMappers: Array[F1[Position, Maybe[Position]]],
    order: CompileOrder,
    compilers: Compilers, setup: Setup, pr: PreviousResult): Inputs =
    inputs(
      new CompileOptions(classpath, sources, classesDirectory, scalacOptions, javacOptions, maxErrors,
        foldMappers(sourcePositionMappers), order),
      compilers, setup, pr
    )
  def previousResult(result: CompileResult): PreviousResult =
    new PreviousResult(Maybe.just[CompileAnalysis](result.analysis), Maybe.just[MiniSetup](result.setup))
  def emptyPreviousResult: PreviousResult = new PreviousResult(
    Maybe.nothing[CompileAnalysis], Maybe.nothing[MiniSetup]
  )
  private[sbt] def f1[A](f: A => A): F1[A, A] =
    new F1[A, A] {
      def apply(a: A): A = f(a)
    }
  private[sbt] def foldMappers[A](mappers: Array[F1[A, Maybe[A]]]) =
    mappers.foldRight(f1[A](identity)) { (mapper, mappers) =>
      f1[A]({ p: A =>
        m2o(mapper(p)).getOrElse(mappers(p))
      })
    }
  def compilers(instance: ScalaInstance, cpOptions: XClasspathOptions, javaHome: Option[File],
    scalac: ScalaCompiler): Compilers =
    {
      val javac = JavaTools.directOrFork(instance, cpOptions, javaHome)
      new Compilers(scalac, javac)
    }
  def compilers(javaTools: XJavaTools, scalac: ScalaCompiler): Compilers =
    new Compilers(scalac, javaTools)
}
