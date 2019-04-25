package sbt.internal.inc

import java.io.File
import java.util

import xsbti._
import xsbti.api.{ DependencyContext, ClassLike }

import scala.collection.mutable.ArrayBuffer

class TestCallback extends AnalysisCallback {
  case class TestUsedName(name: String, scopes: util.EnumSet[UseScope])

  val classDependencies = new ArrayBuffer[(String, String, DependencyContext)]
  val binaryDependencies = new ArrayBuffer[(File, String, String, DependencyContext)]
  val productClassesToSources = scala.collection.mutable.Map.empty[File, File]
  val usedNamesAndScopes =
    scala.collection.mutable.Map.empty[String, Set[TestUsedName]].withDefaultValue(Set.empty)
  val classNames =
    scala.collection.mutable.Map.empty[File, Set[(String, String)]].withDefaultValue(Set.empty)
  val apis: scala.collection.mutable.Map[File, Set[ClassLike]] = scala.collection.mutable.Map.empty

  def usedNames = usedNamesAndScopes.mapValues(_.map(_.name))

  def startSource(source: File): Unit = {
    assert(!apis.contains(source),
           s"The startSource can be called only once per source file: $source")
    apis(source) = Set.empty
  }

  def classDependency(onClassName: String,
                      sourceClassName: String,
                      context: DependencyContext): Unit = {
    if (onClassName != sourceClassName)
      classDependencies += ((onClassName, sourceClassName, context))
    ()
  }
  def binaryDependency(onBinary: File,
                       onBinaryClassName: String,
                       fromClassName: String,
                       fromSourceFile: File,
                       context: DependencyContext): Unit = {
    binaryDependencies += ((onBinary, onBinaryClassName, fromClassName, context))
    ()
  }
  def generatedNonLocalClass(sourceFile: File,
                             classFile: File,
                             binaryClassName: String,
                             srcClassName: String): Unit = {
    productClassesToSources += ((classFile, sourceFile))
    classNames(sourceFile) += ((srcClassName, binaryClassName))
    ()
  }

  def generatedLocalClass(sourceFile: File, classFile: File): Unit = {
    productClassesToSources += ((classFile, sourceFile))
    ()
  }

  def usedName(className: String, name: String, scopes: util.EnumSet[UseScope]): Unit =
    usedNamesAndScopes(className) += TestUsedName(name, scopes)

  def api(source: File, api: ClassLike): Unit = {
    apis(source) += api
    ()
  }

  def mainClass(source: File, className: String): Unit = ()

  override def enabled(): Boolean = true

  def problem(category: String,
              pos: xsbti.Position,
              message: String,
              severity: xsbti.Severity,
              reported: Boolean): Unit = ()

  override def dependencyPhaseCompleted(): Unit = {}

  override def apiPhaseCompleted(): Unit = {}

  override def classesInOutputJar(): util.Set[String] = java.util.Collections.emptySet()
}

object TestCallback {
  case class ExtractedClassDependencies(memberRef: Map[String, Set[String]],
                                        inheritance: Map[String, Set[String]],
                                        localInheritance: Map[String, Set[String]])
  object ExtractedClassDependencies {
    def fromPairs(
        memberRefPairs: Seq[(String, String)],
        inheritancePairs: Seq[(String, String)],
        localInheritancePairs: Seq[(String, String)]
    ): ExtractedClassDependencies = {
      ExtractedClassDependencies(pairsToMultiMap(memberRefPairs),
                                 pairsToMultiMap(inheritancePairs),
                                 pairsToMultiMap(localInheritancePairs))
    }

    private def pairsToMultiMap[A, B](pairs: Seq[(A, B)]): Map[A, Set[B]] = {
      import scala.collection.mutable.{ HashMap, MultiMap }
      val emptyMultiMap = new HashMap[A, scala.collection.mutable.Set[B]] with MultiMap[A, B]
      val multiMap = pairs.foldLeft(emptyMultiMap) {
        case (acc, (key, value)) =>
          acc.addBinding(key, value)
      }
      // convert all collections to immutable variants
      multiMap.toMap.mapValues(_.toSet).withDefaultValue(Set.empty)
    }
  }
}
