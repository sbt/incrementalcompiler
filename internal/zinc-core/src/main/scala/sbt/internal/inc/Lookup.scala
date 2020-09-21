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

package sbt.internal.inc

import java.util
import java.util.Optional

import xsbti.api.AnalyzedClass
import xsbti.{ VirtualFileRef, VirtualFile }
import xsbti.compile.{ Changes, CompileAnalysis, ExternalHooks, FileHash }

/**
 * A trait that encapsulates looking up elements on a classpath and looking up
 * an external (for another subproject) Analysis instance.
 */
trait Lookup extends ExternalLookup {

  /** Returns the current classpath if the classpath has changed from the last compilation. */
  def changedClasspathHash: Option[Vector[FileHash]]

  def analyses: Vector[CompileAnalysis]

  /**
   * Lookup an element on the classpath corresponding to a given binary class name.
   * If found class file is stored in a jar file, the jar file is returned.
   *
   * @param binaryClassName
   * @return
   */
  def lookupOnClasspath(binaryClassName: String): Option[VirtualFileRef]

  /**
   * Return an Analysis instance that has the given binary class name registered as a product.
   *
   * @param binaryClassName
   * @return
   */
  def lookupAnalysis(binaryClassName: String): Option[CompileAnalysis]

  override def lookupAnalyzedClass(binaryClassName: String, file: Option[VirtualFileRef]) = {
    // This is the default, slow, route, via Analysis; overridden in LookupImpl for the fast-track.
    for {
      analysis0 <- lookupAnalysis(binaryClassName)
      analysis = analysis0 match { case a: Analysis => a }
      className <- analysis.relations.productClassName.reverse(binaryClassName).headOption
      analyzedClass <- analysis.apis.internal.get(className)
    } yield analyzedClass
  }

}

/**
 * Defines a hook interface that IDEs or build tools can mock to modify the way
 * Zinc invalidates the incremental compiler. These hooks operate at a high-level
 * of abstraction and only allow to modify the inputs of the initial change detection.
 */
trait ExternalLookup extends ExternalHooks.Lookup {
  import sbt.internal.inc.JavaInterfaceUtil.EnrichOption
  import scala.collection.JavaConverters._

  /**
   * Find the external `AnalyzedClass` (from another analysis) given a class name and, if available,
   * the jar file (or class file) the class comes from.
   *
   * @return The `AnalyzedClass` associated with the given class name, if one is found.
   *         None => Found class somewhere outside of project.  No analysis possible.
   *         Some(analyzed) if analyzed.provenance.isEmpty => Couldn't find it.
   *         Some(analyzed) => good
   */
  def lookupAnalyzedClass(
      binaryClassName: String,
      file: Option[VirtualFileRef],
  ): Option[AnalyzedClass]

  /**
   * Used to provide information from external tools into sbt (e.g. IDEs)
   *
   * @param previousAnalysis
   * @return None if is unable to determine what was changed, changes otherwise
   */
  def changedSources(previousAnalysis: CompileAnalysis): Option[Changes[VirtualFileRef]]

  override def getChangedSources(
      previousAnalysis: CompileAnalysis
  ): Optional[Changes[VirtualFileRef]] = changedSources(previousAnalysis).toOptional

  /**
   * Used to provide information from external tools into sbt (e.g. IDEs)
   *
   * @param previousAnalysis
   * @return None if is unable to determine what was changed, changes otherwise
   */
  def changedBinaries(previousAnalysis: CompileAnalysis): Option[Set[VirtualFileRef]]

  override def getChangedBinaries(
      previousAnalysis: CompileAnalysis
  ): Optional[util.Set[VirtualFileRef]] = changedBinaries(previousAnalysis).map(_.asJava).toOptional

  /**
   * Used to provide information from external tools into sbt (e.g. IDEs)
   *
   * @param previousAnalysis
   * @return None if is unable to determine what was changed, changes otherwise
   */
  def removedProducts(previousAnalysis: CompileAnalysis): Option[Set[VirtualFileRef]]

  override def getRemovedProducts(
      previousAnalysis: CompileAnalysis
  ): Optional[util.Set[VirtualFileRef]] = removedProducts(previousAnalysis).map(_.asJava).toOptional

  /**
   * Used to provide information from external tools into sbt (e.g. IDEs)
   * @return API changes
   */
  def shouldDoIncrementalCompilation(
      changedClasses: Set[String],
      analysis: CompileAnalysis
  ): Boolean

  override def shouldDoIncrementalCompilation(
      changedClasses: util.Set[String],
      previousAnalysis: CompileAnalysis
  ): Boolean = {
    import scala.collection.JavaConverters._
    shouldDoIncrementalCompilation(changedClasses.iterator().asScala.toSet, previousAnalysis)
  }

  /**
   * Used to override whether we should proceed with making an early output.
   *
   * By default we do not make an early output in the presence of any macros
   * because macro expansion (in a downstream subproject) requires the macro implementation
   * to be present in bytecode form, rather than just just a pickle-containing JAR.
   *
   * If you're careful micromanaging the separation of macro implementations
   * (e.g. `def impl(c: Context) = ...`) from macro definitions
   * (e.g. `def foo: Unit = macro Foo.impl`) you can safely override this.
   */
  def shouldDoEarlyOutput(analysis: CompileAnalysis): Boolean = {
    analysis.asInstanceOf[Analysis].apis.internal.values.forall(!_.hasMacro)
  }

}

trait NoopExternalLookup extends ExternalLookup {
  override def lookupAnalyzedClass(binaryClassName: String, file: Option[VirtualFileRef]) = None
  override def changedSources(previous: CompileAnalysis): Option[Changes[VirtualFileRef]] = None
  override def changedBinaries(previous: CompileAnalysis): Option[Set[VirtualFileRef]] = None
  override def removedProducts(previous: CompileAnalysis): Option[Set[VirtualFileRef]] = None

  override def shouldDoIncrementalCompilation(
      changedClasses: Set[String],
      analysis: CompileAnalysis
  ): Boolean = true

  override def hashClasspath(classpath: Array[VirtualFile]): Optional[Array[FileHash]] =
    Optional.empty()

}
