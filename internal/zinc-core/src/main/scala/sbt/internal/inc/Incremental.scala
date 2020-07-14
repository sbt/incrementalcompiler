/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package sbt
package internal
package inc

import java.nio.file.Path
import java.util.EnumSet
import sbt.internal.inc.Analysis.{ LocalProduct, NonLocalProduct }
import sbt.util.{ InterfaceUtil, Level, Logger }
import sbt.util.InterfaceUtil.jo2o
import scala.collection.JavaConverters._
import xsbti.{ FileConverter, Position, Problem, Severity, UseScope, VirtualFile, VirtualFileRef }
import xsbt.api.{ APIUtil, HashAPI, NameHashing }
import xsbti.api._
import xsbti.compile.{
  AnalysisContents,
  AnalysisStore,
  CompileAnalysis,
  CompileProgress,
  DependencyChanges,
  IncOptions,
  MiniSetup,
  Output,
  ClassFileManager => XClassFileManager
}
import xsbti.compile.analysis.{ ReadStamps, Stamp => XStamp }

/**
 * Helper methods for running incremental compilation.
 * This is responsible for is adapting any xsbti.AnalysisCallback into one
 * compatible with the [[sbt.internal.inc.Incremental]] class.
 */
object Incremental {
  class PrefixingLogger(val prefix: String)(orig: Logger) extends Logger {
    def trace(t: => Throwable): Unit = orig.trace(t)
    def success(message: => String): Unit = orig.success(message)
    def log(level: Level.Value, message: => String): Unit = level match {
      case Level.Debug => orig.log(level, message.replaceAll("(?m)^", prefix))
      case _           => orig.log(level, message)
    }
  }

  /**
   * This is a callback from AnalysisCallback back up to Zinc code to
   * perform mid-compilation.
   *
   * @param classFileManager
   */
  abstract class IncrementalCallback(classFileManager: XClassFileManager) {

    /**
     * Merge latest analysis as of pickling into pruned previous analysis, compute invalidations
     * and decide whether we need another cycle.
     */
    def mergeAndInvalidate(partialAnalysis: Analysis, completingCycle: Boolean): CompileCycleResult

    /**
     * Merge latest analysis as of analyzer into pruned previous analysis and inform file manager.
     */
    def completeCycle(
        prev: Option[CompileCycleResult],
        partialAnalysis: Analysis
    ): CompileCycleResult

    def previousAnalysisPruned: Analysis

    /**
     * @return true when the compilation cycle is compiling all the sources; false, otherwise.
     */
    def isFullCompilation: Boolean
  }

  sealed trait CompileCycle {
    def run(
        sources: Set[VirtualFile],
        changes: DependencyChanges,
        incHandler: IncrementalCallback
    ): CompileCycleResult
  }
  case class CompileCycleResult(
      continue: Boolean,
      nextInvalidations: Set[String],
      analysis: Analysis
  )
  object CompileCycleResult {
    def apply(
        continue: Boolean,
        nextInvalidations: Set[String],
        analysis: Analysis
    ): CompileCycleResult =
      new CompileCycleResult(continue, nextInvalidations, analysis)
    def empty = CompileCycleResult(false, Set.empty, Analysis.empty)
  }

  /**
   * Runs the incremental compilation algorithm.
   *
   * @param sources The full set of input sources
   * @param converter FileConverter to convert between Path and VirtualFileRef.
   * @param lookup An instance of the `Lookup` that implements looking up both classpath elements
   *               and Analysis object instances by a binary class name.
   * @param compile The mechanism to run a single 'step' of compile, for ALL source files involved.
   * @param previous0 The previous dependency Analysis (or an empty one).
   * @param output The configured output directory/directory mapping for source files.
   * @param log Where all log messages should go
   * @param options Incremental compiler options (like name hashing vs. not).
   * @return A flag of whether or not compilation completed successfully, and the resulting
   *         dependency analysis object.
   */
  def apply(
      sources: Set[VirtualFile],
      converter: FileConverter,
      lookup: Lookup,
      previous0: CompileAnalysis,
      options: IncOptions,
      currentSetup: MiniSetup,
      stamper: ReadStamps,
      output: Output,
      outputJarContent: JarUtils.OutputJarContent,
      earlyOutput: Option[Output],
      earlyAnalysisStore: Option[AnalysisStore],
      progress: Option[CompileProgress],
      log: Logger
  )(
      compile: (
          Set[VirtualFile],
          DependencyChanges,
          xsbti.AnalysisCallback,
          XClassFileManager
      ) => Unit
  ): (Boolean, Analysis) = {
    log.debug(s"[zinc] IncrementalCompile -----------")
    val previous = previous0 match { case a: Analysis => a }
    val currentStamper = Stamps.initial(stamper)
    val internalBinaryToSourceClassName = (binaryClassName: String) =>
      previous.relations.productClassName.reverse(binaryClassName).headOption
    val internalSourceToClassNamesMap: VirtualFile => Set[String] = (f: VirtualFile) =>
      previous.relations.classNames(f)
    val externalAPI = getExternalAPI(lookup)
    val profiler = options.externalHooks.getInvalidationProfiler
    val runProfiler = new AdaptedRunProfiler(profiler.profileRun)
    val incremental: IncrementalCommon = new IncrementalNameHashing(log, options, runProfiler)
    try {
      incrementalCompile(
        sources,
        converter,
        lookup,
        previous,
        currentStamper,
        (vs, depCh, cb, cfm) => {
          val startTime = System.nanoTime()
          compile(vs, depCh, cb, cfm)
          runProfiler.timeCompilation(startTime, System.nanoTime() - startTime)
        },
        new AnalysisCallback.Builder(
          internalBinaryToSourceClassName,
          internalSourceToClassNamesMap,
          externalAPI,
          currentStamper,
          options,
          currentSetup,
          converter,
          lookup,
          output,
          outputJarContent,
          earlyOutput,
          earlyAnalysisStore,
          progress,
          log
        ),
        incremental,
        options,
        currentSetup,
        output,
        outputJarContent,
        log
      )
    } catch {
      case _: xsbti.CompileCancelled =>
        log.info("Compilation has been cancelled")
        // in case compilation got cancelled potential partial compilation results (e.g. produced classs files) got rolled back
        // and we can report back as there was no change (false) and return a previous Analysis which is still up-to-date
        (false, previous)
    } finally runProfiler.registerRun()
  }

  def getExternalAPI(lookup: Lookup): (VirtualFileRef, String) => Option[AnalyzedClass] =
    (_: VirtualFileRef, binaryClassName: String) => lookup.lookupAnalyzedClass(binaryClassName)

  /**
   * Runs the incremental compiler algorithm.
   *
   * @param sources   The sources to compile
   * @param converter FileConverter to convert between Path and VirtualFileRef.
   * @param lookup
   *              An instance of the `Lookup` that implements looking up both classpath elements
   *              and Analysis object instances by a binary class name.
   * @param previous0 The previous dependency Analysis (or an empty one).
   * @param current  A mechanism for generating stamps (timestamps, hashes, etc).
   * @param compile  The function which can run one level of compile.
   * @param callbackBuilder The builder that builds callback where we report dependency issues.
   * @param log  The log where we write debugging information
   * @param options  Incremental compilation options
   * @param outputJarContent Object that holds cached content of output jar
   * @param profiler An implementation of an invalidation profiler, empty by default.
   * @param equivS  The means of testing whether two "Stamps" are the same.
   * @return
   *         A flag of whether or not compilation completed successfully, and the resulting dependency analysis object.
   */
  def incrementalCompile(
      sources: Set[VirtualFile],
      converter: FileConverter,
      lookup: Lookup,
      previous0: CompileAnalysis,
      current: ReadStamps,
      compile: (
          Set[VirtualFile],
          DependencyChanges,
          xsbti.AnalysisCallback,
          XClassFileManager
      ) => Unit,
      callbackBuilder: AnalysisCallback.Builder,
      incremental: IncrementalCommon,
      options: IncOptions,
      currentSetup: MiniSetup,
      output: Output,
      outputJarContent: JarUtils.OutputJarContent,
      log: sbt.util.Logger
  )(implicit equivS: Equiv[XStamp]): (Boolean, Analysis) = {
    log.debug("IncrementalCompile.incrementalCompile")
    val previous = previous0 match { case a: Analysis => a }
    val initialChanges =
      incremental.detectInitialChanges(sources, previous, current, lookup, converter, output)
    log.debug(s"> initialChanges = $initialChanges")
    val binaryChanges = new DependencyChanges {
      val modifiedLibraries = initialChanges.libraryDeps.toArray
      val modifiedClasses = initialChanges.external.allModified.toArray
      def isEmpty = modifiedLibraries.isEmpty && modifiedClasses.isEmpty
    }
    val (initialInvClasses, initialInvSources0) =
      incremental.invalidateInitial(previous.relations, initialChanges)

    // If there's any compilation at all, invalidate all java sources too, so we have access to their type information.
    val javaSources: Set[VirtualFileRef] = sources
      .filter(_.name.endsWith(".java"))
      .map(_.asInstanceOf[VirtualFileRef])
    val initialInvSources =
      if (initialInvSources0.nonEmpty) initialInvSources0 ++ javaSources
      else Set.empty[VirtualFileRef]
    if (initialInvClasses.nonEmpty || initialInvSources.nonEmpty) {
      if (initialInvSources == sources)
        incremental.log.debug(s"all ${initialInvSources.size} sources are invalidated")
      else
        incremental.log.debug(
          "All initially invalidated classes: " + initialInvClasses + "\n" +
            "All initially invalidated sources:" + initialInvSources + "\n"
        )
    }
    val analysis = manageClassfiles(options, converter, output, outputJarContent) {
      classfileManager =>
        incremental.cycle(
          initialInvClasses,
          initialInvSources,
          sources,
          converter,
          binaryChanges,
          lookup,
          previous,
          doCompile(compile, callbackBuilder, classfileManager),
          classfileManager,
          output,
          1
        )
    }
    (initialInvClasses.nonEmpty || initialInvSources.nonEmpty, analysis)
  }

  /**
   * Compilation unit in each compile cycle.
   */
  def doCompile(
      compile: (
          Set[VirtualFile],
          DependencyChanges,
          xsbti.AnalysisCallback,
          XClassFileManager
      ) => Unit,
      callbackBuilder: AnalysisCallback.Builder,
      classFileManager: XClassFileManager
  ): CompileCycle = new CompileCycle {
    override def run(
        srcs: Set[VirtualFile],
        changes: DependencyChanges,
        incHandler: IncrementalCallback
    ): CompileCycleResult = {
      // Note `ClassFileManager` is shared among multiple cycles in the same incremental compile run,
      // in order to rollback entirely if transaction fails. `AnalysisCallback` is used by each cycle
      // to report its own analysis individually.
      val callback = callbackBuilder.build(incHandler)
      compile(srcs, changes, callback, classFileManager)
      callback.getCycleResultOnce
    }
  }

  // the name of system property that was meant to enable debugging mode of incremental compiler but
  // it ended up being used just to enable debugging of relations. That's why if you migrate to new
  // API for configuring incremental compiler (IncOptions) it's enough to control value of `relationsDebug`
  // flag to achieve the same effect as using `incDebugProp`.
  @deprecated("Use `IncOptions.relationsDebug` flag to enable debugging of relations.", "0.13.2")
  val incDebugProp = "xsbt.inc.debug"

  private[inc] val apiDebugProp = "xsbt.api.debug"
  private[inc] def apiDebug(options: IncOptions): Boolean =
    options.apiDebug || java.lang.Boolean.getBoolean(apiDebugProp)

  private[sbt] def prune(
      invalidatedSrcs: Set[VirtualFile],
      previous0: CompileAnalysis,
      output: Output,
      outputJarContent: JarUtils.OutputJarContent,
      converter: FileConverter
  ): Analysis = {
    val previous = previous0.asInstanceOf[Analysis]
    IncrementalCommon.pruneClassFilesOfInvalidations(
      invalidatedSrcs,
      previous,
      ClassFileManager.deleteImmediately(output, outputJarContent),
      converter
    )
  }

  private[this] def manageClassfiles[T](
      options: IncOptions,
      converter: FileConverter,
      output: Output,
      outputJarContent: JarUtils.OutputJarContent
  )(run: XClassFileManager => T): T = {
    val classfileManager =
      ClassFileManager.getClassFileManager(options, converter, output, outputJarContent)
    val result = try run(classfileManager)
    catch {
      case e: Throwable =>
        classfileManager.complete(false)
        throw e
    }
    classfileManager.complete(true)
    result
  }
}

private object AnalysisCallback {

  /** Allow creating new callback instance to be used in each compile iteration */
  class Builder(
      internalBinaryToSourceClassName: String => Option[String],
      internalSourceToClassNamesMap: VirtualFile => Set[String],
      externalAPI: (VirtualFileRef, String) => Option[AnalyzedClass],
      stampReader: ReadStamps,
      options: IncOptions,
      currentSetup: MiniSetup,
      converter: FileConverter,
      lookup: Lookup,
      output: Output,
      outputJarContent: JarUtils.OutputJarContent,
      earlyOutput: Option[Output],
      earlyAnalysisStore: Option[AnalysisStore],
      progress: Option[CompileProgress],
      log: Logger
  ) {
    def build(incHandler: Incremental.IncrementalCallback): AnalysisCallback = {
      new AnalysisCallback(
        internalBinaryToSourceClassName,
        internalSourceToClassNamesMap,
        externalAPI,
        stampReader,
        options,
        currentSetup,
        outputJarContent,
        converter,
        lookup,
        output,
        earlyOutput,
        earlyAnalysisStore,
        progress,
        incHandler,
        log
      )
    }
  }
}

private final class AnalysisCallback(
    internalBinaryToSourceClassName: String => Option[String],
    internalSourceToClassNamesMap: VirtualFile => Set[String],
    externalAPI: (VirtualFileRef, String) => Option[AnalyzedClass],
    stampReader: ReadStamps,
    options: IncOptions,
    currentSetup: MiniSetup,
    outputJarContent: JarUtils.OutputJarContent,
    converter: FileConverter,
    lookup: Lookup,
    output: Output,
    earlyOutput: Option[Output],
    earlyAnalysisStore: Option[AnalysisStore],
    progress: Option[CompileProgress],
    incHandler: Incremental.IncrementalCallback,
    log: Logger
) extends xsbti.AnalysisCallback {
  import Incremental.CompileCycleResult

  // This must have a unique value per AnalysisCallback
  private[this] val compileStartTime: Long = System.currentTimeMillis()
  private[this] val compilation: Compilation = Compilation(compileStartTime, output)

  private val hooks = options.externalHooks
  private val provenance = jo2o(output.getSingleOutput).fold("")(hooks.getProvenance.get(_)).intern

  override def toString =
    (List("Class APIs", "Object APIs", "Library deps", "Products", "Source deps") zip
      List(classApis, objectApis, libraryDeps, nonLocalClasses, intSrcDeps))
      .map { case (label, map) => label + "\n\t" + map.mkString("\n\t") }
      .mkString("\n")

  case class ApiInfo(
      publicHash: HashAPI.Hash,
      extraHash: HashAPI.Hash,
      classLike: ClassLike
  )

  import java.util.concurrent.{ ConcurrentLinkedQueue, ConcurrentHashMap }
  import scala.collection.concurrent.TrieMap

  private type ConcurrentSet[A] = ConcurrentHashMap.KeySetView[A, java.lang.Boolean]

  private[this] val srcs = ConcurrentHashMap.newKeySet[VirtualFile]()
  private[this] val classApis = new TrieMap[String, ApiInfo]
  private[this] val objectApis = new TrieMap[String, ApiInfo]
  private[this] val classPublicNameHashes = new TrieMap[String, Array[NameHash]]
  private[this] val objectPublicNameHashes = new TrieMap[String, Array[NameHash]]
  private[this] val usedNames = new TrieMap[String, ConcurrentSet[UsedName]]
  private[this] val unreporteds = new TrieMap[VirtualFileRef, ConcurrentLinkedQueue[Problem]]
  private[this] val reporteds = new TrieMap[VirtualFileRef, ConcurrentLinkedQueue[Problem]]
  private[this] val mainClasses = new TrieMap[VirtualFileRef, ConcurrentLinkedQueue[String]]
  private[this] val libraryDeps = new TrieMap[VirtualFileRef, ConcurrentSet[VirtualFile]]

  // source file to set of generated (class file, binary class name); only non local classes are stored here
  private[this] val nonLocalClasses =
    new TrieMap[VirtualFileRef, ConcurrentSet[(VirtualFileRef, String)]]
  private[this] val localClasses = new TrieMap[VirtualFileRef, ConcurrentSet[VirtualFileRef]]
  // mapping between src class name and binary (flat) class name for classes generated from src file
  private[this] val classNames = new TrieMap[VirtualFileRef, ConcurrentSet[(String, String)]]
  // generated class file to its source class name
  private[this] val classToSource = new TrieMap[VirtualFileRef, String]
  // internal source dependencies
  private[this] val intSrcDeps = new TrieMap[String, ConcurrentSet[InternalDependency]]
  // external source dependencies
  private[this] val extSrcDeps = new TrieMap[String, ConcurrentSet[ExternalDependency]]
  private[this] val binaryClassName = new TrieMap[VirtualFile, String]
  // source files containing a macro def.
  private[this] val macroClasses = ConcurrentHashMap.newKeySet[String]()

  // Results of invalidation calculations (including whether to continue cycles) - the analysis at this point is
  // not useful and so isn't included.
  private[this] var invalidationResults: Option[CompileCycleResult] = None

  private def add[A, B](map: TrieMap[A, ConcurrentSet[B]], a: A, b: B): Unit = {
    map.getOrElseUpdate(a, ConcurrentHashMap.newKeySet[B]()).add(b)
    ()
  }

  override def isPickleJava: Boolean = {
    currentSetup.options.scalacOptions.contains("-Ypickle-java")
  }

  def startSource(source: VirtualFile): Unit = {
    if (options.strictMode()) {
      assert(
        !srcs.contains(source),
        s"The startSource can be called only once per source file: $source"
      )
    }
    srcs.add(source)
    ()
  }

  def problem(
      category: String,
      pos: Position,
      msg: String,
      severity: Severity,
      reported: Boolean
  ): Unit = {
    for (path <- jo2o(pos.sourcePath())) {
      val source = VirtualFileRef.of(path)
      val map = if (reported) reporteds else unreporteds
      map
        .getOrElseUpdate(source, new ConcurrentLinkedQueue)
        .add(InterfaceUtil.problem(category, pos, msg, severity, None))
    }
  }

  def classDependency(onClassName: String, sourceClassName: String, context: DependencyContext) = {
    if (onClassName != sourceClassName)
      add(intSrcDeps, sourceClassName, InternalDependency.of(sourceClassName, onClassName, context))
  }

  private[this] def externalLibraryDependency(
      binary: VirtualFile,
      className: String,
      source: VirtualFileRef,
      context: DependencyContext
  ): Unit = {
    binaryClassName.put(binary, className)
    add(libraryDeps, source, binary)
  }

  private[this] def externalSourceDependency(
      sourceClassName: String,
      targetBinaryClassName: String,
      targetClass: AnalyzedClass,
      context: DependencyContext
  ): Unit = {
    val dependency =
      ExternalDependency.of(sourceClassName, targetBinaryClassName, targetClass, context)
    add(extSrcDeps, sourceClassName, dependency)
  }

  // since the binary at this point could either *.class files or
  // library JARs, we need to accept Path here.
  def binaryDependency(
      classFile: Path,
      onBinaryClassName: String,
      fromClassName: String,
      fromSourceFile: VirtualFileRef,
      context: DependencyContext
  ) =
    internalBinaryToSourceClassName(onBinaryClassName) match {
      case Some(dependsOn) => // dependsOn is a source class name
        // dependency is a product of a source not included in this compilation
        classDependency(dependsOn, fromClassName, context)
      case None =>
        val vf = converter.toVirtualFile(classFile)
        classToSource.get(vf) match {
          case Some(dependsOn) =>
            // dependency is a product of a source in this compilation step,
            //  but not in the same compiler run (as in javac v. scalac)
            classDependency(dependsOn, fromClassName, context)
          case None =>
            externalDependency(classFile, onBinaryClassName, fromClassName, fromSourceFile, context)
        }
    }

  private[this] def externalDependency(
      classFile: Path,
      onBinaryName: String,
      sourceClassName: String,
      sourceFile: VirtualFileRef,
      context: DependencyContext
  ): Unit = {
    // TODO: handle library JARs and rt.jar.
    val vf = converter.toVirtualFile(classFile)
    externalAPI(vf, onBinaryName) match {
      case Some(api) =>
        // dependency is a product of a source in another project
        val targetBinaryClassName = onBinaryName
        externalSourceDependency(sourceClassName, targetBinaryClassName, api, context)
      case None =>
        // dependency is some other binary on the classpath.
        // exclude dependency tracking with rt.jar, for example java.lang.String -> rt.jar.
        if (vf.name != "rt.jar") {
          externalLibraryDependency(
            vf,
            onBinaryName,
            sourceFile,
            context
          )
        }
    }
  }

  def generatedNonLocalClass(
      source: VirtualFileRef,
      classFile: Path,
      binaryClassName: String,
      srcClassName: String
  ): Unit = {
    //println(s"Generated non local class ${source}, ${classFile}, ${binaryClassName}, ${srcClassName}")
    val vf = converter.toVirtualFile(classFile)
    add(nonLocalClasses, source, (vf, binaryClassName))
    add(classNames, source, (srcClassName, binaryClassName))
    classToSource.put(vf, srcClassName)
    ()
  }

  def generatedLocalClass(source: VirtualFileRef, classFile: Path): Unit = {
    //println(s"Generated local class ${source}, ${classFile}")
    val vf = converter.toVirtualFile(classFile)
    add(localClasses, source, vf)
    ()
  }

  def api(sourceFile: VirtualFileRef, classApi: ClassLike): Unit = {
    import xsbt.api.{ APIUtil, HashAPI }
    val className = classApi.name
    if (APIUtil.isScalaSourceName(sourceFile.id) && APIUtil.hasMacro(classApi))
      macroClasses.add(className)
    val shouldMinimize = !Incremental.apiDebug(options)
    val savedClassApi = if (shouldMinimize) APIUtil.minimize(classApi) else classApi
    val apiHash: HashAPI.Hash = HashAPI(classApi)
    val nameHashes = (new xsbt.api.NameHashing(options.useOptimizedSealed())).nameHashes(classApi)
    classApi.definitionType match {
      case d @ (DefinitionType.ClassDef | DefinitionType.Trait) =>
        val extraApiHash = {
          if (d != DefinitionType.Trait) apiHash
          else HashAPI(_.hashAPI(classApi), includePrivateDefsInTrait = true)
        }

        classApis(className) = ApiInfo(apiHash, extraApiHash, savedClassApi)
        classPublicNameHashes(className) = nameHashes.toArray
      case DefinitionType.Module | DefinitionType.PackageModule =>
        objectApis(className) = ApiInfo(apiHash, apiHash, savedClassApi)
        objectPublicNameHashes(className) = nameHashes.toArray
    }
  }

  def mainClass(sourceFile: VirtualFileRef, className: String): Unit = {
    mainClasses.getOrElseUpdate(sourceFile, new ConcurrentLinkedQueue).add(className)
    ()
  }

  def usedName(className: String, name: String, useScopes: EnumSet[UseScope]) =
    add(usedNames, className, UsedName(name, useScopes))

  override def enabled(): Boolean = options.enabled

  private[this] var gotten: Boolean = false
  def getCycleResultOnce: CompileCycleResult = {
    assert(!gotten, "can't call AnalysisCallback#getCycleResultOnce more than once")
    gotten = true
    // notify that early artifact writing is not going to happen because of macros
    def notifyEarlyArifactFailure(): Unit =
      if (!writtenEarlyArtifacts) {
        progress foreach { p =>
          p.earlyOutputComplete(false)
        }
      }
    outputJarContent.scalacRunCompleted()
    val a = getAnalysis
    if (options.pipelining) {
      invalidationResults match {
        case None =>
          val early = incHandler.previousAnalysisPruned
          if (!hasAnyMacro(early)) writeEarlyArtifacts(early)
          else notifyEarlyArifactFailure()
        case Some(CompileCycleResult(false, _, _)) => notifyEarlyArifactFailure()
        case _                                     => ()
      }
    }
    // assert(writtenEarlyArtifacts, s"early artifact $earlyOutput hasn't been written")
    incHandler.completeCycle(invalidationResults, a)
  }

  private def getAnalysis: Analysis = {
    val analysis0 = addProductsAndDeps(Analysis.empty)
    addUsedNames(addCompilation(analysis0))
  }

  def getOrNil[A, B](m: collection.Map[A, Seq[B]], a: A): Seq[B] = m.get(a).toList.flatten
  def addCompilation(base: Analysis): Analysis =
    base.copy(compilations = base.compilations.add(compilation))
  def addUsedNames(base: Analysis): Analysis = usedNames.foldLeft(base) {
    case (a, (className, names)) =>
      import scala.collection.JavaConverters._
      names.asScala.foldLeft(a) {
        case (a, name) => a.copy(relations = a.relations.addUsedName(className, name))
      }
  }

  private def companionsWithHash(className: String): (Companions, HashAPI.Hash, HashAPI.Hash) = {
    val emptyHash = -1
    val emptyClass =
      ApiInfo(emptyHash, emptyHash, APIUtil.emptyClassLike(className, DefinitionType.ClassDef))
    val emptyObject =
      ApiInfo(emptyHash, emptyHash, APIUtil.emptyClassLike(className, DefinitionType.Module))
    val ApiInfo(classApiHash, classHashExtra, classApi) = classApis.getOrElse(className, emptyClass)
    val ApiInfo(objectApiHash, objectHashExtra, objectApi) =
      objectApis.getOrElse(className, emptyObject)
    val companions = Companions.of(classApi, objectApi)
    val apiHash = (classApiHash, objectApiHash).hashCode
    val extraHash = (classHashExtra, objectHashExtra).hashCode
    (companions, apiHash, extraHash)
  }

  private def nameHashesForCompanions(className: String): Array[NameHash] = {
    val classNameHashes = classPublicNameHashes.get(className)
    val objectNameHashes = objectPublicNameHashes.get(className)
    (classNameHashes, objectNameHashes) match {
      case (Some(nm1), Some(nm2)) =>
        NameHashing.merge(nm1, nm2)
      case (Some(nm), None) => nm
      case (None, Some(nm)) => nm
      case (None, None)     => sys.error("Failed to find name hashes for " + className)
    }
  }

  private def analyzeClass(name: String): AnalyzedClass = {
    val hasMacro: Boolean = macroClasses.contains(name)
    val (companions, apiHash, extraHash) = companionsWithHash(name)
    val nameHashes = nameHashesForCompanions(name)
    val safeCompanions = SafeLazyProxy(companions)
    AnalyzedClass.of(
      compileStartTime,
      name,
      safeCompanions,
      apiHash,
      nameHashes,
      hasMacro,
      extraHash,
      provenance
    )
  }

  def addProductsAndDeps(base: Analysis): Analysis = {
    import scala.collection.JavaConverters._
    srcs.asScala.foldLeft(base) {
      case (a, src) =>
        val stamp = stampReader.source(src)
        val classesInSrc = classNames
          .getOrElse(src, ConcurrentHashMap.newKeySet[(String, String)]())
          .asScala
          .map(_._1)
        val analyzedApis = classesInSrc.map(analyzeClass)
        val info = SourceInfos.makeInfo(
          getOrNil(reporteds.mapValues({ _.asScala.toSeq }).toMap, src),
          getOrNil(unreporteds.mapValues({ _.asScala.toSeq }).toMap, src),
          getOrNil(mainClasses.mapValues({ _.asScala.toSeq }).toMap, src)
        )
        val libraries: collection.mutable.Set[VirtualFile] =
          libraryDeps.getOrElse(src, ConcurrentHashMap.newKeySet[VirtualFile]).asScala
        val localProds = localClasses
          .getOrElse(src, ConcurrentHashMap.newKeySet[VirtualFileRef]())
          .asScala map { classFile =>
          val classFileStamp = stampReader.product(classFile)
          LocalProduct(classFile, classFileStamp)
        }
        val binaryToSrcClassName =
          (classNames.getOrElse(src, ConcurrentHashMap.newKeySet[(String, String)]()).asScala map {
            case (srcClassName, binaryClassName) => (binaryClassName, srcClassName)
          }).toMap
        val nonLocalProds = nonLocalClasses
          .getOrElse(src, ConcurrentHashMap.newKeySet[(VirtualFileRef, String)]())
          .asScala map {
          case (classFile, binaryClassName) =>
            val srcClassName = binaryToSrcClassName(binaryClassName)
            val classFileStamp = stampReader.product(classFile)
            NonLocalProduct(srcClassName, binaryClassName, classFile, classFileStamp)
        }

        val internalDeps = classesInSrc.flatMap(
          cls =>
            intSrcDeps.getOrElse(cls, ConcurrentHashMap.newKeySet[InternalDependency]()).asScala
        )
        val externalDeps = classesInSrc.flatMap(
          cls =>
            extSrcDeps.getOrElse(cls, ConcurrentHashMap.newKeySet[ExternalDependency]()).asScala
        )
        val libDeps = libraries.map(d => (d, binaryClassName(d), stampReader.library(d)))

        a.addSource(
          src,
          analyzedApis,
          stamp,
          info,
          nonLocalProds,
          localProds,
          internalDeps,
          externalDeps,
          libDeps
        )
    }
  }

  override def apiPhaseCompleted(): Unit = {
    // If we know we're done with cycles (presumably because all sources were invalidated) we can store early analysis
    // and picke data now.  Otherwise, we need to wait for dependency information to decide if there are more cycles.
    if (options.pipelining() && incHandler.isFullCompilation) {
      val a = getAnalysis
      val CompileCycleResult(continue, invalidations, merged) =
        incHandler.mergeAndInvalidate(a, false)
      if (!hasAnyMacro(merged)) {
        assert(
          !continue && invalidations.isEmpty,
          "everything was supposed to be invalidated already"
        )
        invalidationResults = Some(CompileCycleResult.empty)
        writeEarlyArtifacts(merged)
      }
    }
  }

  override def dependencyPhaseCompleted(): Unit = {
    if (invalidationResults.isEmpty) {
      val a = getAnalysis
      val CompileCycleResult(continue, invalidations, merged) =
        incHandler.mergeAndInvalidate(a, false)
      // Store invalidations and continuation decision; the analysis will be computed again after Analyze phase.
      invalidationResults = Some(CompileCycleResult(continue, invalidations, Analysis.empty))
      // If there will be no more compilation cycles, store the early analysis file and update the pickle jar
      if (options.pipelining && !continue && !hasAnyMacro(merged)) {
        writeEarlyArtifacts(merged)
      }
    }
    outputJarContent.dependencyPhaseCompleted()
  }

  override def classesInOutputJar(): java.util.Set[String] = {
    outputJarContent.get().asJava
  }

  def hasAnyMacro(merged: Analysis): Boolean =
    merged.apis.internal.values.exists(p => p.hasMacro)

  private[this] var writtenEarlyArtifacts: Boolean = false
  private def writeEarlyArtifacts(merged: Analysis): Unit = {
    writtenEarlyArtifacts = true
    // log.info(s"writeEarlyArtifacts to $earlyOutput")
    earlyAnalysisStore map { store =>
      store.set(AnalysisContents.create(merged, currentSetup))
    }
    for {
      earlyO <- earlyOutput
      pickleJarPath <- jo2o(earlyO.getSingleOutput())
    } {
      // List classes defined in the files that were compiled in this run.
      val knownProducts = merged.relations.allSources
        .flatMap(merged.relations.products)
        .flatMap(extractProductPath)
      PickleJar.write(pickleJarPath, knownProducts.toSet)
    }
    progress foreach { p =>
      p.earlyOutputComplete(true)
    }
  }

  private def extractProductPath(product: VirtualFileRef): Option[String] = {
    jo2o(output.getSingleOutput) match {
      case Some(so) if so.getFileName.toString.endsWith(".jar") =>
        new JarUtils.ClassInJar(product.id).toClassFilePath
      case Some(so) =>
        val productPath = converter.toPath(product)
        sbt.io.IO.relativize(so.toFile, productPath.toFile)
      case _ => sys.error(s"unsupported output $output")
    }
  }
}
