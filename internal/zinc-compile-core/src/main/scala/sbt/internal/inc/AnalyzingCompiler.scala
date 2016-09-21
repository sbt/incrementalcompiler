/* sbt -- Simple Build Tool
 * Copyright 2009, 2010  Mark Harrah
 */
package sbt
package internal
package inc

import xsbti.{ AnalysisCallback, Logger => xLogger, Maybe, Reporter }
import xsbti.compile.{ CachedCompiler, CachedCompilerProvider, ClasspathOptions, DependencyChanges, GlobalsCache, CompileProgress, Output, ScalaCompiler, ScalaInstance => XScalaInstance }
import java.io.File
import java.net.{ URL, URLClassLoader }
import sbt.util.Logger
import sbt.io.syntax._

/**
 * Interface to the Scala compiler that uses the dependency analysis plugin.  This class uses the Scala library and compiler
 * provided by scalaInstance.  This class requires a ComponentManager in order to obtain the interface code to scalac and
 * the analysis plugin.  Because these call Scala code for a different Scala version than the one used for this class, they must
 * be compiled for the version of Scala being used.
 */
final class AnalyzingCompiler private (val scalaInstance: XScalaInstance, val provider: CompilerInterfaceProvider, override val classpathOptions: ClasspathOptions, onArgsF: Seq[String] => Unit) extends CachedCompilerProvider with ScalaCompiler {
  def this(scalaInstance: XScalaInstance, provider: CompilerInterfaceProvider, cp: ClasspathOptions) =
    this(scalaInstance, provider, cp, _ => ())
  def this(scalaInstance: XScalaInstance, provider: CompilerInterfaceProvider) = this(scalaInstance, provider, ClasspathOptionsUtil.auto)

  @deprecated("Renamed to `classpathOptions`", "1.0.0")
  val cp = classpathOptions

  def onArgs(f: Seq[String] => Unit): AnalyzingCompiler = new AnalyzingCompiler(scalaInstance, provider, classpathOptions, f)

  def compile(sources: Array[File], changes: DependencyChanges, options: Array[String], output: Output, callback: AnalysisCallback, reporter: Reporter, cache: GlobalsCache, log: xLogger, progressOpt: Maybe[CompileProgress]): Unit =
    {
      val cached = cache(options.toArray, output, !changes.isEmpty, this, log, reporter)
      val progress = if (progressOpt.isDefined) progressOpt.get else IgnoreProgress
      compile(sources.toArray, changes, callback, log, reporter, progress, cached)
    }

  def compile(sources: Array[File], changes: DependencyChanges, callback: AnalysisCallback, log: xLogger, reporter: Reporter, progress: CompileProgress, compiler: CachedCompiler): Unit = {
    onArgsF(compiler.commandArguments(sources.toArray))
    call("xsbt.CompilerInterface", "run", log)(
      classOf[Array[File]], classOf[DependencyChanges], classOf[AnalysisCallback], classOf[xLogger], classOf[Reporter], classOf[CompileProgress], classOf[CachedCompiler]
    )(
      sources, changes, callback, log, reporter, progress, compiler
    )
    ()
  }
  def newCachedCompiler(arguments: Array[String], output: Output, log: xLogger, reporter: Reporter, resident: Boolean): CachedCompiler =
    newCachedCompiler(arguments: Seq[String], output, log, reporter, resident)

  def newCachedCompiler(arguments: Seq[String], output: Output, log: xLogger, reporter: Reporter, resident: Boolean): CachedCompiler =
    {
      call("xsbt.CompilerInterface", "newCompiler", log)(
        classOf[Array[String]], classOf[Output], classOf[xLogger], classOf[Reporter], classOf[Boolean]
      )(
        arguments.toArray[String]: Array[String], output, log, reporter, resident: java.lang.Boolean
      ).
        asInstanceOf[CachedCompiler]
    }

  def doc(sources: Seq[File], classpath: Seq[File], outputDirectory: File, options: Seq[String], maximumErrors: Int, log: Logger): Unit =
    doc(sources, classpath, outputDirectory, options, log, new LoggerReporter(maximumErrors, log))
  def doc(sources: Seq[File], classpath: Seq[File], outputDirectory: File, options: Seq[String], log: Logger, reporter: Reporter): Unit =
    {
      val arguments = (new CompilerArguments(scalaInstance, classpathOptions))(sources, classpath, Some(outputDirectory), options)
      onArgsF(arguments)
      call("xsbt.ScaladocInterface", "run", log)(classOf[Array[String]], classOf[xLogger], classOf[Reporter])(
        arguments.toArray[String]: Array[String], log, reporter
      )
      ()
    }
  def console(classpath: Seq[File], options: Seq[String], initialCommands: String, cleanupCommands: String, log: Logger)(loader: Option[ClassLoader] = None, bindings: Seq[(String, Any)] = Nil): Unit =
    {
      onArgsF(consoleCommandArguments(classpath, options, log))
      val (classpathString, bootClasspath) = consoleClasspaths(classpath)
      val (names, values) = bindings.unzip
      call("xsbt.ConsoleInterface", "run", log)(
        classOf[Array[String]], classOf[String], classOf[String], classOf[String], classOf[String], classOf[ClassLoader], classOf[Array[String]], classOf[Array[Any]], classOf[xLogger]
      )(
        options.toArray[String]: Array[String], bootClasspath, classpathString, initialCommands, cleanupCommands, loader.orNull, names.toArray[String], values.toArray[Any], log
      )
      ()
    }

  private[this] def consoleClasspaths(classpath: Seq[File]): (String, String) =
    {
      val arguments = new CompilerArguments(scalaInstance, classpathOptions)
      val classpathString = CompilerArguments.absString(arguments.finishClasspath(classpath))
      val bootClasspath = if (classpathOptions.autoBoot) arguments.createBootClasspathFor(classpath) else ""
      (classpathString, bootClasspath)
    }
  def consoleCommandArguments(classpath: Seq[File], options: Seq[String], log: Logger): Seq[String] =
    {
      val (classpathString, bootClasspath) = consoleClasspaths(classpath)
      val argsObj = call("xsbt.ConsoleInterface", "commandArguments", log)(
        classOf[Array[String]], classOf[String], classOf[String], classOf[xLogger]
      )(
        options.toArray[String]: Array[String], bootClasspath, classpathString, log
      )
      argsObj.asInstanceOf[Array[String]].toSeq
    }
  def force(log: Logger): Unit = { provider(scalaInstance, log); () }
  private def call(interfaceClassName: String, methodName: String, log: Logger)(argTypes: Class[_]*)(args: AnyRef*): AnyRef =
    {
      val interfaceClass = getInterfaceClass(interfaceClassName, log)
      val interface = interfaceClass.newInstance.asInstanceOf[AnyRef]
      val method = interfaceClass.getMethod(methodName, argTypes: _*)
      try { method.invoke(interface, args: _*) }
      catch {
        case e: java.lang.reflect.InvocationTargetException =>
          e.getCause match {
            case c: xsbti.CompileFailed => throw new CompileFailed(c.arguments, c.toString, c.problems)
            case t                      => throw t
          }
      }
    }
  private[this] def loader(log: Logger) =
    {
      val interfaceJar = provider(scalaInstance, log)
      // this goes to scalaInstance.loader for scala classes and the loader of this class for xsbti classes
      val dual = createDualLoader(scalaInstance.loader, getClass.getClassLoader)
      new URLClassLoader(Array(interfaceJar.toURI.toURL), dual)
    }
  private[this] def getInterfaceClass(name: String, log: Logger) = Class.forName(name, true, loader(log))
  protected def createDualLoader(scalaLoader: ClassLoader, sbtLoader: ClassLoader): ClassLoader =
    {
      val xsbtiFilter = (name: String) => name.startsWith("xsbti.")
      val notXsbtiFilter = (name: String) => !xsbtiFilter(name)
      new classpath.DualLoader(scalaLoader, notXsbtiFilter, x => true, sbtLoader, xsbtiFilter, x => false)
    }
  override def toString = "Analyzing compiler (Scala " + scalaInstance.actualVersion + ")"
}
object AnalyzingCompiler {
  import sbt.io.IO.{ copy, createDirectory, zip, jars, unzip, withTemporaryDirectory }

  // Note: The Scala build now depends on some details of this method:
  //   https://github.com/jsuereth/scala/commit/3431860048df8d2a381fb85a526097e00154eae0
  /**
   * Extract sources from source jars, compile them with the xsbti interfaces on the classpath, and package the compiled classes and
   * any resources from the source jars into a final jar.
   */
  def compileSources(sourceJars: Iterable[File], targetJar: File, xsbtiJars: Iterable[File], id: String, compiler: RawCompiler, log: Logger): Unit = {
    val isSource = (f: File) => isSourceName(f.getName)
    def keepIfSource(files: Set[File]): Set[File] = if (files.exists(isSource)) files else Set()

    withTemporaryDirectory { dir =>
      val extractedSources = (Set[File]() /: sourceJars) { (extracted, sourceJar) => extracted ++ keepIfSource(unzip(sourceJar, dir)) }
      val (sourceFiles, resources) = extractedSources.partition(isSource)
      withTemporaryDirectory { outputDirectory =>
        log.info("'" + id + "' not yet compiled for Scala " + compiler.scalaInstance.actualVersion + ". Compiling...")
        val start = System.currentTimeMillis
        try {
          compiler(sourceFiles.toSeq, compiler.scalaInstance.libraryJar +: (xsbtiJars.toSeq ++ sourceJars), outputDirectory, "-nowarn" :: Nil)
          log.info("  Compilation completed in " + (System.currentTimeMillis - start) / 1000.0 + " s")
        } catch { case e: xsbti.CompileFailed => throw new CompileFailed(e.arguments, "Error compiling sbt component '" + id + "'", e.problems) }
        import sbt.io.Path._
        copy(resources pair rebase(dir, outputDirectory))
        zip(outputDirectory.allPaths.pair(relativeTo(outputDirectory), errorIfNone = false), targetJar)
      }
    }
  }
  private def isSourceName(name: String): Boolean = name.endsWith(".scala") || name.endsWith(".java")
}

private[this] object IgnoreProgress extends CompileProgress {
  def startUnit(phase: String, unitPath: String): Unit = ()
  def advance(current: Int, total: Int) = true
}
