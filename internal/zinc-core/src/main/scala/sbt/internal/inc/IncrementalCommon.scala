package sbt
package internal
package inc

import java.io.File
import sbt.util.Logger
import Logger.m2o
import scala.annotation.tailrec
import scala.collection.mutable
import xsbti.Maybe
import xsbti.api.{ AnalyzedClass, Compilation }
import xsbti.compile.{ DependencyChanges, IncOptions, IncOptionsUtil, CompileAnalysis, FileWatch, FileChanges }

private[inc] abstract class IncrementalCommon(log: sbt.util.Logger, options: IncOptions) {

  // setting the related system property to true will skip checking that the class name
  // still comes from the same classpath entry.  This can workaround bugs in classpath construction,
  // such as the currently problematic -javabootclasspath.  This is subject to removal at any time.
  private[this] def skipClasspathLookup = java.lang.Boolean.getBoolean("xsbt.skip.cp.lookup")

  val wrappedLog = new Incremental.PrefixingLogger("[inv] ")(log)
  def debug(s: => String) = if (options.relationsDebug) wrappedLog.debug(s) else ()

  // TODO: the Analysis for the last successful compilation should get returned + Boolean indicating success
  // TODO: full external name changes, scopeInvalidations
  @tailrec final def cycle(invalidatedRaw: Set[String], modifiedSrcs: Set[File], allSources: Set[File],
    binaryChanges: DependencyChanges, previous: Analysis, trackEndTime: Option[Long],
    doCompile: (Set[File], DependencyChanges) => Analysis, classfileManager: ClassfileManager,
    cycleNum: Int): Analysis =
    if (invalidatedRaw.isEmpty && modifiedSrcs.isEmpty)
      previous
    else {
      val invalidatedPackageObjects = this.invalidatedPackageObjects(invalidatedRaw, previous.relations,
        previous.apis)
      if (invalidatedPackageObjects.nonEmpty)
        log.debug(s"Invalidated package objects: ${invalidatedPackageObjects}")
      val withPackageObjects = invalidatedRaw ++ invalidatedPackageObjects
      val invalidatedClasses = withPackageObjects

      val current = recompileClasses(invalidatedClasses, modifiedSrcs, allSources, binaryChanges, previous, trackEndTime, doCompile,
        classfileManager)

      // modifiedSrc have to be mapped to class names both of previous and current analysis because classes might be
      // removed (it's handled by `previous`) or added (it's handled by `current`) or renamed (it's handled by both)
      val recompiledClasses = invalidatedClasses ++
        modifiedSrcs.flatMap(previous.relations.classNames) ++ modifiedSrcs.flatMap(current.relations.classNames)

      val incChanges = changedIncremental(recompiledClasses, previous.apis.internalAPI _, current.apis.internalAPI _)
      debug("\nChanges:\n" + incChanges)
      val transitiveStep = options.transitiveStep
      val classToSourceMapper = new ClassToSourceMapper(previous.relations, current.relations)
      val incInv = invalidateIncremental(current.relations, current.apis, incChanges, recompiledClasses,
        cycleNum >= transitiveStep, classToSourceMapper.isDefinedInScalaSrc)
      cycle(incInv, Set.empty, allSources, emptyChanges, current, trackEndTime, doCompile, classfileManager, cycleNum + 1)
    }

  private[this] def recompileClasses(classes: Set[String], modifiedSrcs: Set[File], allSources: Set[File],
    binaryChanges: DependencyChanges, previous: Analysis, trackEndTime: Option[Long],
    doCompile: (Set[File], DependencyChanges) => Analysis,
    classfileManager: ClassfileManager): Analysis = {
    val invalidatedSources = classes.flatMap(previous.relations.definesClass) ++ modifiedSrcs
    val invalidatedSourcesForCompilation = expand(invalidatedSources, allSources)
    val pruned = Incremental.prune(invalidatedSourcesForCompilation, previous, classfileManager)
    debug("********* Pruned: \n" + pruned.relations + "\n*********")

    val fresh = doCompile(invalidatedSourcesForCompilation, binaryChanges)
    classfileManager.generated(fresh.relations.allProducts)
    debug("********* Fresh: \n" + fresh.relations + "\n*********")
    val merged0 = pruned ++ fresh
    //.copy(relations = pruned.relations ++ fresh.relations, apis = pruned.apis ++ fresh.apis)
    debug("********* Merged: \n" + merged0.relations + "\n*********")
    trackEndTime match {
      case Some(x) => merged0.copy(stamps = merged0.stamps.changeTime(previous.stamps.changeEndTime, trackEndTime))
      case _       => merged0
    }
  }

  private[this] def emptyChanges: DependencyChanges = new DependencyChanges {
    val modifiedBinaries = new Array[File](0)
    val modifiedClasses = new Array[String](0)
    def isEmpty = true
  }
  private[this] def expand(invalidated: Set[File], all: Set[File]): Set[File] = {
    val recompileAllFraction = options.recompileAllFraction
    if (invalidated.size > all.size * recompileAllFraction) {
      log.debug("Recompiling all " + all.size + " sources: invalidated sources (" + invalidated.size + ") exceeded " + (recompileAllFraction * 100.0) + "% of all sources")
      all ++ invalidated // need the union because all doesn't contain removed sources
    } else invalidated
  }

  protected def invalidatedPackageObjects(invalidatedClasses: Set[String], relations: Relations,
    apis: APIs): Set[String]

  /**
   * Logs API changes using debug-level logging. The API are obtained using the APIDiff class.
   *
   * NOTE: This method creates a new APIDiff instance on every invocation.
   */
  private def logApiChanges(apiChanges: Iterable[APIChange], oldAPIMapping: String => AnalyzedClass,
    newAPIMapping: String => AnalyzedClass): Unit = {
    val contextSize = options.apiDiffContextSize
    try {
      val wrappedLog = new Incremental.PrefixingLogger("[diff] ")(log)
      val apiDiff = new APIDiff
      apiChanges foreach {
        case APIChangeDueToMacroDefinition(src) =>
          wrappedLog.debug(s"Public API is considered to be changed because $src contains a macro definition.")
        case apiChange: NamesChange =>
          val src = apiChange.modifiedClass
          val oldApi = oldAPIMapping(src)
          val newApi = newAPIMapping(src)
          val apiUnifiedPatch = apiDiff.generateApiDiff(src.toString, oldApi.api, newApi.api, contextSize)
          wrappedLog.debug(s"Detected a change in a public API ($src):\n$apiUnifiedPatch")
      }
    } catch {
      case e: ClassNotFoundException =>
        log.error("You have api debugging enabled but DiffUtils library cannot be found on sbt's classpath")
      case e: LinkageError =>
        log.error("Encountered linkage error while trying to load DiffUtils library.")
        log.trace(e)
      case e: Exception =>
        log.error("An exception has been thrown while trying to dump an api diff.")
        log.trace(e)
    }
  }

  /**
   * Accepts the classes that were recompiled during the last step and functions
   * providing the API before and after the last step.  The functions should return
   * an empty API if the class did not/does not exist.
   */
  def changedIncremental(lastClasses: collection.Set[String], oldAPI: String => AnalyzedClass,
    newAPI: String => AnalyzedClass): APIChanges =
    {
      val oldApis = lastClasses.toSeq map oldAPI
      val newApis = lastClasses.toSeq map newAPI
      val apiChanges = (lastClasses, oldApis, newApis).zipped.flatMap {
        (className, oldApi, newApi) => sameClass(className, oldApi, newApi)
      }

      if (Incremental.apiDebug(options) && apiChanges.nonEmpty) {
        logApiChanges(apiChanges, oldAPI, newAPI)
      }

      new APIChanges(apiChanges)
    }
  def sameClass(className: String, a: AnalyzedClass, b: AnalyzedClass): Option[APIChange] = {
    // Clients of a modified class (ie, one that doesn't satisfy `shortcutSameClass`) containing macros must be recompiled.
    val hasMacro = a.hasMacro || b.hasMacro
    if (shortcutSameClass(a, b)) {
      None
    } else {
      if (hasMacro && IncOptionsUtil.getRecompileOnMacroDef(options)) {
        Some(APIChangeDueToMacroDefinition(className))
      } else sameAPI(className, a, b)
    }
  }

  protected def sameAPI(className: String, a: AnalyzedClass, b: AnalyzedClass): Option[APIChange]

  def shortcutSameClass(a: AnalyzedClass, b: AnalyzedClass): Boolean =
    sameCompilation(a.compilation, b.compilation) && (a.apiHash == b.apiHash)
  def sameCompilation(a: Compilation, b: Compilation): Boolean = a.startTime == b.startTime && a.outputs.corresponds(b.outputs) {
    case (co1, co2) => co1.sourceDirectory == co2.sourceDirectory && co1.outputDirectory == co2.outputDirectory
  }

  def changedInitial(sources: Set[File], previousAnalysis0: CompileAnalysis, current: ReadStamps,
    lookup: Lookup, fileWatch: FileWatch)(implicit equivS: Equiv[Stamp]): InitialChanges =
    {
      val previousAnalysis = previousAnalysis0 match { case a: Analysis => a }
      val previous = previousAnalysis.stamps
      val startTime = previous.changeEndTime
      val endTime = current.changeEndTime
      val previousAPIs = previousAnalysis.apis
      val srcChanges = changes(previous.allInternalSources.toSet, sources, f => !equivS.equiv(previous.internalSource(f), current.internalSource(f)),
        fileWatch.sourceChanges, startTime)
      val removedProducts = previous.allProducts.filter(p => !equivS.equiv(previous.product(p), current.product(p))).toSet
      val binaryDepChanges = previous.allBinaries.filter(externalBinaryModified(lookup, previous, current)).toSet
      val extChanges = changedIncremental(previousAPIs.allExternals, previousAPIs.externalAPI _, currentExternalAPI(lookup))

      InitialChanges(startTime, endTime, srcChanges, removedProducts, binaryDepChanges, extChanges)
    }

  def changes(previous: Set[File], current: Set[File], existingModified: File => Boolean,
    fileWatch: Long => Maybe[FileChanges], since: Option[Long]): Changes[File] =
    {
      def changes0 = new Changes[File] {
        private val inBoth = previous & current
        val removed = previous -- inBoth
        val added = current -- inBoth
        val (changed, unmodified) = inBoth.partition(existingModified)
      }
      // Changes using the File Watcher API
      def fromFileChanges(fc: FileChanges) = new Changes[File] {
        import scala.collection.JavaConverters._
        private val inBoth = previous & current
        private val reportedChanges: Set[File] = fc.changed.asScala.toSet
        val removed = previous -- inBoth
        val added = current -- inBoth
        val changed = inBoth & reportedChanges
        val unmodified = inBoth -- changed
        override def toString: String =
          s"""Changes($removed,$added, changed = $changed, unmodified = $unmodified)"""
      }
      val fileChanges = since flatMap { t => m2o(fileWatch(t)) }
      val result = fileChanges match {
        case Some(c) => fromFileChanges(c)
        case None    => changes0
      }
      log.debug(s"changes - since: $since result: $result")
      result
    }

  def invalidateIncremental(previous: Relations, apis: APIs, changes: APIChanges,
    recompiledClasses: Set[String], transitive: Boolean, isScalaClass: String => Boolean): Set[String] =
    {
      val dependsOnClass = previous.memberRef.internal.reverse _
      val propagated: Set[String] =
        if (transitive)
          transitiveDependencies(dependsOnClass, changes.allModified.toSet)
        else
          invalidateIntermediate(previous, changes, isScalaClass)

      val dups = invalidateDuplicates(previous)
      if (dups.nonEmpty)
        log.debug("Invalidated due to generated class file collision: " + dups)

      val inv: Set[String] = propagated ++ dups
      val newlyInvalidated = (inv -- recompiledClasses) ++ dups
      log.debug("All newly invalidated classes after taking into account (previously) recompiled classes:" + newlyInvalidated)
      if (newlyInvalidated.isEmpty) Set.empty else inv
    }

  /** Invalidate all classes that claim to produce the same class file as another class. */
  def invalidateDuplicates(merged: Relations): Set[String] =
    merged.srcProd.reverseMap.flatMap {
      case (classFile, sources) =>
        if (sources.size > 1) sources.flatMap(merged.classNames) else Nil
    }.toSet

  /**
   * Returns the transitive class dependencies of `initial`.
   * Because the intermediate steps do not pull in cycles, this result includes the initial classes
   * if they are part of a cycle containing newly invalidated classes.
   */
  def transitiveDependencies(dependsOnClass: String => Set[String], initial: Set[String]): Set[String] =
    {
      val transitiveWithInitial = transitiveDeps(initial)(dependsOnClass)
      val transitivePartial = includeInitialCond(initial, transitiveWithInitial, dependsOnClass)
      log.debug("Final step, transitive dependencies:\n\t" + transitivePartial)
      transitivePartial
    }

  /** Invalidates classes and sources based on initially detected 'changes' to the sources, products, and dependencies.*/
  def invalidateInitial(previous: Relations, changes: InitialChanges): (Set[String], Set[File]) =
    {
      def classNames(srcs: Set[File]): Set[String] =
        srcs.flatMap(previous.classNames)
      val srcChanges = changes.internalSrc
      val modifiedSrcs = srcChanges.changed
      val addedSrcs = srcChanges.added
      val removedClasses = classNames(srcChanges.removed)
      val dependentOnRemovedClasses = removedClasses.flatMap(previous.memberRef.internal.reverse)
      val modifiedClasses = classNames(modifiedSrcs)
      val invalidatedClasses = removedClasses ++ dependentOnRemovedClasses ++ modifiedClasses
      val byProduct = changes.removedProducts.flatMap(previous.produced)
      val byBinaryDep = changes.binaryDeps.flatMap(previous.usesBinary)
      val classToSrc = new ClassToSourceMapper(previous, previous)
      val byExtSrcDep = {
        val classNames = invalidateByAllExternal(previous, changes.external, classToSrc.isDefinedInScalaSrc) //changes.external.modified.flatMap(previous.usesExternal) // ++ scopeInvalidations
        classNames
      }
      checkAbsolute(srcChanges.added.toList)
      log.debug(
        "\nInitial source changes: \n\tremoved:" + srcChanges.removed + "\n\tadded: " + srcChanges.added + "\n\tmodified: " + srcChanges.changed +
          "\nInvalidated products: " + changes.removedProducts +
          "\nExternal API changes: " + changes.external +
          "\nModified binary dependencies: " + changes.binaryDeps +
          "\nInitial directly invalidated classes: " + invalidatedClasses +
          "\n\nSources indirectly invalidated by:" +
          "\n\tproduct: " + byProduct +
          "\n\tbinary dep: " + byBinaryDep +
          "\n\texternal source: " + byExtSrcDep
      )

      (invalidatedClasses ++ byExtSrcDep, addedSrcs ++ modifiedSrcs ++ byProduct ++ byBinaryDep)
    }
  private[this] def checkAbsolute(addedSources: List[File]): Unit =
    if (addedSources.nonEmpty) {
      addedSources.filterNot(_.isAbsolute) match {
        case first :: more =>
          val fileStrings = more match {
            case Nil      => first.toString
            case x :: Nil => s"$first and $x"
            case _        => s"$first and ${more.size} others"
          }
          sys.error(s"The incremental compiler requires absolute sources, but some were relative: $fileStrings")
        case Nil =>
      }
    }

  def invalidateByAllExternal(relations: Relations, externalAPIChanges: APIChanges, isScalaClass: String => Boolean): Set[String] = {
    (externalAPIChanges.apiChanges.flatMap { externalAPIChange =>
      invalidateByExternal(relations, externalAPIChange, isScalaClass)
    }).toSet
  }

  /** Classes invalidated by `external` classes in other projects according to the previous `relations`. */
  protected def invalidateByExternal(relations: Relations, externalAPIChange: APIChange, isScalaClass: String => Boolean): Set[String]

  /** Intermediate invalidation step: steps after the initial invalidation, but before the final transitive invalidation. */
  def invalidateIntermediate(relations: Relations, changes: APIChanges, isScalaClass: String => Boolean): Set[String] =
    {
      invalidateClasses(relations, changes, isScalaClass)
    }
  /**
   * Invalidates inheritance dependencies, transitively.  Then, invalidates direct dependencies.  Finally, excludes initial dependencies not
   * included in a cycle with newly invalidated classes.
   */
  private def invalidateClasses(relations: Relations, changes: APIChanges, isScalaClass: String => Boolean): Set[String] =
    {
      val initial = changes.allModified.toSet
      val all = (changes.apiChanges flatMap { change =>
        invalidateClass(relations, change, isScalaClass)
      }).toSet
      includeInitialCond(initial, all, allDeps(relations))
    }

  protected def allDeps(relations: Relations): (String) => Set[String]

  protected def invalidateClass(relations: Relations, change: APIChange, isScalaClass: String => Boolean): Set[String]

  /**
   * Conditionally include initial classes that are dependencies of newly invalidated classes.
   * Initial classes included in this step can be because of a cycle, but not always.
   */
  private[this] def includeInitialCond(initial: Set[String], currentInvalidations: Set[String],
    allDeps: String => Set[String]): Set[String] =
    {
      val newInv = currentInvalidations -- initial
      log.debug("New invalidations:\n\t" + newInv)
      val transitiveOfNew = transitiveDeps(newInv)(allDeps)
      val initialDependsOnNew = transitiveOfNew & initial
      log.debug("Previously invalidated, but (transitively) depend on new invalidations:\n\t" + initialDependsOnNew)
      newInv ++ initialDependsOnNew
    }

  def externalBinaryModified(lookup: Lookup, previous: Stamps, current: ReadStamps)(implicit equivS: Equiv[Stamp]): File => Boolean =
    dependsOn =>
      {
        def inv(reason: String): Boolean = {
          log.debug("Invalidating " + dependsOn + ": " + reason)
          true
        }
        def entryModified(className: String, classpathEntry: File): Boolean =
          {
            val resolved = Locate.resolve(classpathEntry, className)
            if (resolved.getCanonicalPath != dependsOn.getCanonicalPath)
              inv("class " + className + " now provided by " + resolved.getCanonicalPath)
            else
              fileModified(dependsOn, resolved)
          }
        def fileModified(previousFile: File, currentFile: File): Boolean =
          {
            val previousStamp = previous.binary(previousFile)
            val currentStamp = current.binary(currentFile)
            if (equivS.equiv(previousStamp, currentStamp))
              false
            else
              inv("stamp changed from " + previousStamp + " to " + currentStamp)
          }
        def dependencyModified(file: File): Boolean =
          previous.className(file) match {
            case None => inv("no class name was mapped for it.")
            case Some(name) => lookup.lookupOnClasspath(name) match {
              case None    => inv("could not find class " + name + " on the classpath.")
              case Some(e) => entryModified(name, e)
            }
          }

        lookup.lookupAnalysis(dependsOn).isEmpty &&
          (if (skipClasspathLookup) fileModified(dependsOn, dependsOn) else dependencyModified(dependsOn))

      }

  def currentExternalAPI(lookup: Lookup): String => AnalyzedClass = {
    binaryClassName =>
      {
        orEmpty(
          for {
            analysis0 <- lookup.lookupAnalysis(binaryClassName)
            analysis = analysis0 match { case a: Analysis => a }
            className <- analysis.relations.binaryClassName.reverse(binaryClassName).headOption
          } yield analysis.apis.internalAPI(className)
        )
      }
  }

  def orEmpty(o: Option[AnalyzedClass]): AnalyzedClass = o getOrElse APIs.emptyAnalyzedClass
  def orTrue(o: Option[Boolean]): Boolean = o getOrElse true

  protected def transitiveDeps[T](nodes: Iterable[T], logging: Boolean = true)(dependencies: T => Iterable[T]): Set[T] =
    {
      val xs = new collection.mutable.HashSet[T]
      def all(from: T, tos: Iterable[T]): Unit = tos.foreach(to => visit(from, to))
      def visit(from: T, to: T): Unit =
        if (!xs.contains(to)) {
          if (logging)
            log.debug(s"Including $to by $from")
          xs += to
          all(to, dependencies(to))
        }
      if (logging)
        log.debug("Initial set of included nodes: " + nodes)
      nodes foreach { start =>
        xs += start
        all(start, dependencies(start))
      }
      xs.toSet
    }
}
