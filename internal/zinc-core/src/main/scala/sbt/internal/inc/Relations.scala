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

import sbt.internal.util.Relation
import xsbti.VirtualFileRef
import xsbti.api.{ DependencyContext, ExternalDependency, InternalDependency }
import xsbti.api.DependencyContext._
import Relations.ClassDependencies
import xsbti.compile.analysis.{ Stamp => XStamp }

/**
 * Provides mappings between source files, generated classes (products), and binaries.
 * Dependencies that are tracked include internal: a dependency on a source in the same compilation group (project),
 * external: a dependency on a source in another compilation group (tracked as the name of the class),
 * library: a dependency on a class or jar file not generated by a source file in any tracked compilation group,
 * inherited: a dependency that resulted from a public template inheriting,
 * direct: any type of dependency, including inheritance.
 */
trait Relations {

  /** All sources _with at least one product_ . */
  def allSources: collection.Set[VirtualFileRef]

  /** All products associated with sources. */
  def allProducts: collection.Set[VirtualFileRef]

  /** All files that are recorded as a library dependency of a source file.*/
  def allLibraryDeps: collection.Set[VirtualFileRef]

  /** All files in another compilation group (project) that are recorded as a source dependency of a source file in this group.*/
  def allExternalDeps: collection.Set[String]

  /**
   * Names (fully qualified, at the pickler phase) of classes defined in source file `src`.
   */
  def classNames(src: VirtualFileRef): Set[String]

  /** Source files that generated a class with the given fully qualified `name`. This is typically a set containing a single file. */
  def definesClass(name: String): Set[VirtualFileRef]

  /** The classes that were generated for source file `src`. */
  def products(src: VirtualFileRef): Set[VirtualFileRef]

  /** The source files that generated class file `prod`.  This is typically a set containing a single file. */
  def produced(prod: VirtualFileRef): Set[VirtualFileRef]

  /** The library dependencies for the source file `src`. */
  def libraryDeps(src: VirtualFileRef): Set[VirtualFileRef]

  /** The source files that depend on library file `dep`. */
  def usesLibrary(dep: VirtualFileRef): Set[VirtualFileRef]

  /** The library class names for the library JAR file `lib`. */
  def libraryClassNames(lib: VirtualFileRef): Set[String]

  /** The library files that generated a class with the given fully qualified `name`. This is typically a set containing a single file. */
  def libraryDefinesClass(name: String): Set[VirtualFileRef]

  /** Internal source dependencies for `src`.  This includes both direct and inherited dependencies.  */
  def internalClassDeps(className: String): Set[String]

  /** Internal source files that depend on internal source `dep`.  This includes both direct and inherited dependencies.  */
  def usesInternalClass(className: String): Set[String]

  /** External source dependencies that internal source file `src` depends on.  This includes both direct and inherited dependencies.  */
  def externalDeps(className: String): Set[String]

  /** Internal source dependencies that depend on external source file `dep`.  This includes both direct and inherited dependencies.  */
  def usesExternal(className: String): Set[String]

  private[inc] def usedNames(className: String): Set[UsedName]

  /**
   * Records that the file `src` generates products `products`, has internal dependencies `internalDeps`,
   * has external dependencies `externalDeps` and library dependencies `libraryDeps`.
   */
  def addSource(
      src: VirtualFileRef,
      products: Iterable[VirtualFileRef],
      classes: Iterable[(String, String)],
      internalDeps: Iterable[InternalDependency],
      externalDeps: Iterable[ExternalDependency],
      libraryDeps: Iterable[(VirtualFileRef, String, XStamp)]
  ): Relations = {
    addProducts(src, products)
      .addClasses(src, classes)
      .addInternalSrcDeps(src, internalDeps)
      .addExternalDeps(src, externalDeps)
      .addLibraryDeps(src, libraryDeps)
  }

  /**
   * Records all the products `prods` generated by `src`
   */
  private[inc] def addProducts(
      src: VirtualFileRef,
      prods: Iterable[VirtualFileRef]
  ): Relations

  /**
   * Records all the classes `classes` generated by `src`
   *
   * a single entry in `classes` collection is `(src class name, binary class name)`
   */
  private[inc] def addClasses(src: VirtualFileRef, classes: Iterable[(String, String)]): Relations

  /**
   * Records all the internal source dependencies `deps` of `src`
   */
  private[inc] def addInternalSrcDeps(
      src: VirtualFileRef,
      deps: Iterable[InternalDependency]
  ): Relations

  /**
   * Records all the external dependencies `deps` of `src`
   */
  private[inc] def addExternalDeps(
      src: VirtualFileRef,
      deps: Iterable[ExternalDependency]
  ): Relations

  /**
   * Records all the library dependencies `deps` of `src`
   */
  private[inc] def addLibraryDeps(
      src: VirtualFileRef,
      deps: Iterable[(VirtualFileRef, String, XStamp)]
  ): Relations

  private[inc] def addUsedName(className: String, name: UsedName): Relations

  /** Concatenates the two relations. Acts naively, i.e., doesn't internalize external deps on added files. */
  def ++(o: Relations): Relations

  /** Drops all dependency mappings a->b where a is in `sources`. Acts naively, i.e., doesn't externalize internal deps on removed files. */
  def --(sources: Iterable[VirtualFileRef]): Relations

  /** The relation between internal sources and generated class files. */
  def srcProd: Relation[VirtualFileRef, VirtualFileRef]

  /** The dependency relation between internal sources and library JARs. */
  def libraryDep: Relation[VirtualFileRef, VirtualFileRef]

  /** The dependency relation between library JARs and class names. */
  def libraryClassName: Relation[VirtualFileRef, String]

  /**
   * The relation between source and product class names.
   *
   * Only non-local classes, objects and traits are tracked by this relation.
   * For classes, nested objects and traits it's 1-1 relation. For top level objects it's 1-2 relation. E.g., for
   *
   *   object A
   *
   * The binaryClass will have two entries:
   *
   *   A -> A, A -> A$
   *
   * This reflects Scala's compiler behavior of generating two class files per top level object declaration.
   */
  def productClassName: Relation[String, String]

  /** The dependency relation between internal classes.*/
  def internalClassDep: Relation[String, String]

  /** The dependency relation between internal and external classes.*/
  def externalClassDep: Relation[String, String]

  /** All the internal dependencies */
  private[inc] def internalDependencies: InternalDependencies

  /** All the external dependencies */
  private[inc] def externalDependencies: ExternalDependencies

  /**
   * The class dependency relation between classes introduced by member reference.
   *
   * NOTE: All inheritance dependencies are included in this relation because in order to
   * inherit from a member you have to refer to it. If you check documentation of `inheritance`
   * you'll see that there's small oddity related to traits being the first parent of a
   * class/trait that results in additional parents being introduced due to normalization.
   * This relation properly accounts for that so the invariant that `memberRef` is a superset
   * of `inheritance` is preserved.
   */
  private[inc] def memberRef: ClassDependencies

  /**
   * The class dependency relation between classes introduced by inheritance.
   * The dependency by inheritance is introduced when a template (class or trait) mentions
   * a given type in a parent position.
   *
   * NOTE: Due to an oddity in how Scala's type checker works there's one unexpected dependency
   * on a class being introduced. An example illustrates the best the problem. Let's consider
   * the following structure:
   *
   * trait A extends B
   * trait B extends C
   * trait C extends D
   * class D
   *
   * We are interested in dependencies by inheritance of `A`. One would expect it to be just `B`
   * but the answer is `B` and `D`. The reason is because Scala's type checker performs a certain
   * normalization so the first parent of a type is a class. Therefore the example above is normalized
   * to the following form:
   *
   * trait A extends D with B
   * trait B extends D with C
   * trait C extends D
   * class D
   *
   * Therefore if you inherit from a trait you'll get an additional dependency on a class that is
   * resolved transitively. You should not rely on this behavior, though.
   *
   */
  private[inc] def inheritance: ClassDependencies

  /**
   * The class dependency introduced by a local class but accounted for an outer, non local class.
   *
   * This type of a dependency arises when a triple of classes is involved. Let's consider an
   * example:
   *
   *   // A.scala
   *   class A
   *
   *   // B.scala
   *   class Outer {
   *     def foo: Unit = {
   *       class Foo extends A
   *     }
   *   }
   *
   * The `Foo` class introduced dependency on `A` by inheritance. However, only non local classes
   * are tracked in dependency graph so dependency from `Foo` is mapped to `Outer` (which is non
   * local class that contains `Foo`).
   *
   * Why don't we just express this situation as `Outer` depending on `A` by regular inheritance
   * dependency? Because inheritance dependencies are invalidated transitively. It would mean that
   * in case `A` is changed, all classes inheriting from `Outer` would be invalidated too. This
   * suboptimal because classes inheriting from `Outer` cannot be affected by changes to `A`.
   *
   * Why not map it to `memberRef` relation that is not invalidated transitively? Because `memberRef`
   * dependencies are subject to name hashing pruning but this is incorrect for inheritance dependencies.
   * Hence we need a special relation that expresses dependencies introduced by inheritance but is not
   * invalidated transitively.
   *
   * See https://github.com/sbt/sbt/issues/1104#issuecomment-169146039 for more details.
   */
  private[inc] def localInheritance: ClassDependencies

  /** The relation between a source file and the fully qualified names of classes generated from it.*/
  def classes: Relation[VirtualFileRef, String]

  /**
   * Relation between source files and _unqualified_ term and type names used in given source file.
   */
  private[inc] def names: Relation[String, UsedName]
}

object Relations {

  /** Tracks internal and external source dependencies for a specific dependency type, such as direct or inherited.*/
  private[inc] final class ClassDependencies(
      val internal: Relation[String, String],
      val external: Relation[String, String]
  ) {
    def addInternal(className: String, dependsOn: Iterable[String]): ClassDependencies =
      new ClassDependencies(internal + (className, dependsOn), external)
    def addExternal(className: String, dependsOn: Iterable[String]): ClassDependencies =
      new ClassDependencies(internal, external + (className, dependsOn))

    /** Drops all dependency mappings from `sources`. Acts naively, i.e., doesn't externalize internal deps on removed files.*/
    def --(classNames: Iterable[String]): ClassDependencies =
      new ClassDependencies(internal -- classNames, external -- classNames)
    def ++(o: ClassDependencies): ClassDependencies =
      new ClassDependencies(internal ++ o.internal, external ++ o.external)

    override def equals(other: Any) = other match {
      case o: ClassDependencies => internal == o.internal && external == o.external
      case _                    => false
    }
    override def toString: String = s"ClassDependencies(internal = $internal, external = $external)"

    override def hashCode = (internal, external).hashCode
  }

  private[sbt] def getOrEmpty[A, B, K](m: Map[K, Relation[A, B]], k: K): Relation[A, B] =
    m.getOrElse(k, Relation.empty)

  def empty: Relations = new MRelationsNameHashing(
    srcProd = Relation.empty,
    libraryDep = Relation.empty,
    libraryClassName = Relation.empty,
    internalDependencies = InternalDependencies.empty,
    externalDependencies = ExternalDependencies.empty,
    classes = Relation.empty,
    names = Relation.empty,
    productClassName = Relation.empty
  )

  private[inc] def make(
      srcProd: Relation[VirtualFileRef, VirtualFileRef],
      libraryDep: Relation[VirtualFileRef, VirtualFileRef],
      libraryClassName: Relation[VirtualFileRef, String],
      internalDependencies: InternalDependencies,
      externalDependencies: ExternalDependencies,
      classes: Relation[VirtualFileRef, String],
      names: Relation[String, UsedName],
      productClassName: Relation[String, String]
  ): Relations =
    new MRelationsNameHashing(
      srcProd,
      libraryDep,
      libraryClassName,
      internalDependencies = internalDependencies,
      externalDependencies = externalDependencies,
      classes,
      names,
      productClassName
    )

  private[inc] def makeClassDependencies(
      internal: Relation[String, String],
      external: Relation[String, String]
  ): ClassDependencies =
    new ClassDependencies(internal, external)
}

private[inc] object DependencyCollection {

  /**
   * Combine `m1` and `m2` such that the result contains all the dependencies they represent.
   * `m1` is expected to be smaller than `m2`.
   */
  def joinMaps[T](
      m1: Map[DependencyContext, Relation[String, T]],
      m2: Map[DependencyContext, Relation[String, T]]
  ) =
    m1.foldLeft(m2) {
      case (tmp, (key, values)) => tmp.updated(key, tmp.getOrElse(key, Relation.empty) ++ values)
    }
}

private[inc] object InternalDependencies {

  /**
   * Constructs an empty `InteralDependencies`
   */
  def empty = InternalDependencies(Map.empty)
}

private case class InternalDependencies(
    dependencies: Map[DependencyContext, Relation[String, String]]
) {

  /**
   * Adds `dep` to the dependencies
   */
  def +(dep: InternalDependency): InternalDependencies =
    InternalDependencies(
      dependencies.updated(
        dep.context,
        dependencies
          .getOrElse(dep.context, Relation.empty) + (dep.sourceClassName, dep.targetClassName)
      )
    )

  /**
   * Adds all `deps` to the dependencies
   */
  def ++(deps: Iterable[InternalDependency]): InternalDependencies = deps.foldLeft(this)(_ + _)
  def ++(deps: InternalDependencies): InternalDependencies =
    InternalDependencies(DependencyCollection.joinMaps(dependencies, deps.dependencies))

  /**
   * Removes all dependencies from `sources` to another file from the dependencies
   */
  def --(classes: Iterable[String]): InternalDependencies = {
    InternalDependencies(
      dependencies.iterator
        .map { case (k, v) => k -> (v -- classes) }
        .filter(_._2.size > 0)
        .toMap
    )
  }
}

private[inc] object ExternalDependencies {

  /**
   * Constructs an empty `ExternalDependencies`
   */
  def empty = ExternalDependencies(Map.empty)
}

private case class ExternalDependencies(
    dependencies: Map[DependencyContext, Relation[String, String]]
) {

  /**
   * Adds `dep` to the dependencies
   */
  def +(dep: ExternalDependency): ExternalDependencies =
    ExternalDependencies(
      dependencies.updated(
        dep.context,
        dependencies
          .getOrElse(dep.context, Relation.empty) + (dep.sourceClassName, dep.targetProductClassName)
      )
    )

  /**
   * Adds all `deps` to the dependencies
   */
  def ++(deps: Iterable[ExternalDependency]): ExternalDependencies = deps.foldLeft(this)(_ + _)
  def ++(deps: ExternalDependencies): ExternalDependencies =
    ExternalDependencies(DependencyCollection.joinMaps(dependencies, deps.dependencies))

  /**
   * Removes all dependencies from `sources` to another file from the dependencies
   */
  def --(classNames: Iterable[String]): ExternalDependencies =
    ExternalDependencies(
      dependencies.iterator
        .map { case (k, v) => k -> (v -- classNames) }
        .filter(_._2.size > 0)
        .toMap
    )
}

private[inc] sealed trait RelationDescriptor[A, B] {
  val header: String
  val selectCorresponding: Relations => Relation[A, B]
  def firstWrite(a: A): String
  def firstRead(s: String): A
  def secondWrite(b: B): String
  def secondRead(s: String): B
}

// private[inc] case class FFRelationDescriptor(
//     header: String,
//     selectCorresponding: Relations => Relation[File, File]
// ) extends RelationDescriptor[File, File] {
//   override def firstWrite(a: File): String = a.toString
//   override def secondWrite(b: File): String = b.toString
//   override def firstRead(s: String): File = new File(s)
//   override def secondRead(s: String): File = new File(s)
// }

// private[inc] case class FSRelationDescriptor(
//     header: String,
//     selectCorresponding: Relations => Relation[File, String]
// ) extends RelationDescriptor[File, String] {
//   override def firstWrite(a: File): String = a.toString
//   override def secondWrite(b: String): String = b
//   override def firstRead(s: String): File = new File(s)
//   override def secondRead(s: String): String = s
// }

// private[inc] case class SFRelationDescriptor(
//     header: String,
//     selectCorresponding: Relations => Relation[String, File]
// ) extends RelationDescriptor[String, File] {
//   override def firstWrite(a: String): String = a
//   override def secondWrite(b: File): String = b.toString
//   override def firstRead(s: String): String = s
//   override def secondRead(s: String): File = new File(s)
// }

private[inc] case class SSRelationDescriptor(
    header: String,
    selectCorresponding: Relations => Relation[String, String]
) extends RelationDescriptor[String, String] {
  override def firstWrite(a: String): String = a
  override def secondWrite(b: String): String = b
  override def firstRead(s: String): String = s
  override def secondRead(s: String): String = s
}

/**
 * An abstract class that contains common functionality inherited by two implementations of Relations trait.
 *
 * A little note why we have two different implementations of Relations trait. This is needed for the time
 * being when we are slowly migrating to the new invalidation algorithm called "name hashing" which requires
 * some subtle changes to dependency tracking. For some time we plan to keep both algorithms side-by-side
 * and have a runtime switch which allows to pick one. So we need logic for both old and new dependency
 * tracking to be available. That's exactly what two subclasses of MRelationsCommon implement. Once name
 * hashing is proven to be stable and reliable we'll phase out the old algorithm and the old dependency tracking
 * logic.
 *
 * `srcProd` is a relation between a source file and a product: (source, product).
 * Note that some source files may not have a product and will not be included in this relation.
 *
 * `libraryDeps` is a relation between a source file and a library dependency: (source, library dependency).
 *   This only includes dependencies on classes and jars that do not have a corresponding source/API to track instead.
 *   A class or jar with a corresponding source should only be tracked in one of the source dependency relations.
 * `libraryClassName` is a relationship between a library JAR file and class names.
 *
 * `classes` is a relation between a source file and its generated fully-qualified class names.
 */
private abstract class MRelationsCommon(
    val srcProd: Relation[VirtualFileRef, VirtualFileRef],
    val libraryDep: Relation[VirtualFileRef, VirtualFileRef],
    val libraryClassName: Relation[VirtualFileRef, String],
    val classes: Relation[VirtualFileRef, String],
    val productClassName: Relation[String, String]
) extends Relations {
  def allSources: collection.Set[VirtualFileRef] = srcProd._1s

  def allProducts: collection.Set[VirtualFileRef] = srcProd._2s
  def allLibraryDeps: collection.Set[VirtualFileRef] = libraryDep._2s
  def allExternalDeps: collection.Set[String] = externalClassDep._2s

  def classNames(src: VirtualFileRef): Set[String] = classes.forward(src)
  def definesClass(name: String): Set[VirtualFileRef] = classes.reverse(name)

  def products(src: VirtualFileRef): Set[VirtualFileRef] = srcProd.forward(src)
  def produced(prod: VirtualFileRef): Set[VirtualFileRef] = srcProd.reverse(prod)

  def libraryDeps(src: VirtualFileRef): Set[VirtualFileRef] = libraryDep.forward(src)
  def usesLibrary(dep: VirtualFileRef): Set[VirtualFileRef] = libraryDep.reverse(dep)
  def libraryClassNames(lib: VirtualFileRef): Set[String] = libraryClassName.forward(lib)
  def libraryDefinesClass(name: String): Set[VirtualFileRef] = libraryClassName.reverse(name)

  def internalClassDeps(className: String): Set[String] =
    internalClassDep.forward(className)
  def usesInternalClass(className: String): Set[String] =
    internalClassDep.reverse(className)

  def externalDeps(className: String): Set[String] = externalClassDep.forward(className)
  def usesExternal(className: String): Set[String] = externalClassDep.reverse(className)

  private[inc] def usedNames(className: String): Set[UsedName] = names.forward(className)

  /** Making large Relations a little readable. */
  private val userDir = sys.props("user.dir").stripSuffix("/") + "/"
  private def nocwd(s: String) = s stripPrefix userDir
  private def line_s(kv: (Any, Any)) =
    "    " + nocwd("" + kv._1) + " -> " + nocwd("" + kv._2) + "\n"
  protected def relation_s(r: Relation[_, _]) = (
    if (r.forwardMap.isEmpty) "Relation [ ]"
    else (r.all.toSeq.map(line_s).sorted) mkString ("Relation [\n", "", "]")
  )
}

/**
 * This class implements Relations trait with support for tracking of `memberRef` and `inheritance` class
 * dependencies. Therefore this class implements the new (compared to sbt 0.13.0) dependency tracking logic
 * needed by the name hashing invalidation algorithm.
 */
private class MRelationsNameHashing(
    srcProd: Relation[VirtualFileRef, VirtualFileRef],
    libraryDep: Relation[VirtualFileRef, VirtualFileRef],
    libraryClassName: Relation[VirtualFileRef, String],
    val internalDependencies: InternalDependencies,
    val externalDependencies: ExternalDependencies,
    classes: Relation[VirtualFileRef, String],
    val names: Relation[String, UsedName],
    productClassName: Relation[String, String]
) extends MRelationsCommon(srcProd, libraryDep, libraryClassName, classes, productClassName) {

  def internalClassDep: Relation[String, String] = memberRef.internal
  def externalClassDep: Relation[String, String] = memberRef.external

  def addProducts(src: VirtualFileRef, products: Iterable[VirtualFileRef]): Relations =
    new MRelationsNameHashing(
      srcProd ++ products.map(p => (src, p)),
      libraryDep,
      libraryClassName,
      internalDependencies = internalDependencies,
      externalDependencies = externalDependencies,
      classes = classes,
      names = names,
      productClassName = productClassName
    )

  private[inc] def addClasses(src: VirtualFileRef, classes: Iterable[(String, String)]): Relations =
    new MRelationsNameHashing(
      srcProd = srcProd,
      libraryDep,
      libraryClassName,
      internalDependencies = internalDependencies,
      externalDependencies = externalDependencies,
      this.classes ++ classes.map(c => (src, c._1)),
      names = names,
      productClassName = this.productClassName ++ classes
    )

  def addInternalSrcDeps(src: VirtualFileRef, deps: Iterable[InternalDependency]) =
    new MRelationsNameHashing(
      srcProd,
      libraryDep,
      libraryClassName,
      internalDependencies = internalDependencies ++ deps,
      externalDependencies = externalDependencies,
      classes,
      names,
      productClassName = productClassName
    )

  def addExternalDeps(src: VirtualFileRef, deps: Iterable[ExternalDependency]) =
    new MRelationsNameHashing(
      srcProd,
      libraryDep,
      libraryClassName,
      internalDependencies = internalDependencies,
      externalDependencies = externalDependencies ++ deps,
      classes,
      names,
      productClassName = productClassName
    )

  def addLibraryDeps(src: VirtualFileRef, deps: Iterable[(VirtualFileRef, String, XStamp)]) =
    new MRelationsNameHashing(
      srcProd,
      libraryDep + (src, deps.map(_._1)),
      libraryClassName ++ (deps map { d =>
        d._1 -> d._2
      }),
      internalDependencies = internalDependencies,
      externalDependencies = externalDependencies,
      classes,
      names,
      productClassName = productClassName
    )

  override private[inc] def addUsedName(className: String, name: UsedName): Relations =
    new MRelationsNameHashing(
      srcProd,
      libraryDep,
      libraryClassName,
      internalDependencies = internalDependencies,
      externalDependencies = externalDependencies,
      classes,
      names = names + (className, name),
      productClassName = productClassName
    )

  override def inheritance: ClassDependencies =
    new ClassDependencies(
      internalDependencies.dependencies.getOrElse(DependencyByInheritance, Relation.empty),
      externalDependencies.dependencies.getOrElse(DependencyByInheritance, Relation.empty)
    )
  override def localInheritance: ClassDependencies =
    new ClassDependencies(
      internalDependencies.dependencies.getOrElse(LocalDependencyByInheritance, Relation.empty),
      externalDependencies.dependencies.getOrElse(LocalDependencyByInheritance, Relation.empty)
    )
  override def memberRef: ClassDependencies =
    new ClassDependencies(
      internalDependencies.dependencies.getOrElse(DependencyByMemberRef, Relation.empty),
      externalDependencies.dependencies.getOrElse(DependencyByMemberRef, Relation.empty)
    )

  def ++(o: Relations): Relations = {
    new MRelationsNameHashing(
      srcProd ++ o.srcProd,
      libraryDep ++ o.libraryDep,
      libraryClassName ++ o.libraryClassName,
      internalDependencies = internalDependencies ++ o.internalDependencies,
      externalDependencies = externalDependencies ++ o.externalDependencies,
      classes ++ o.classes,
      names = names ++ o.names,
      productClassName = productClassName ++ o.productClassName
    )
  }

  // def removeFile(sources: Iterable[File]) = {
  //   new MRelationsNameHashing(
  //     srcProd, // -- sources,
  //     libraryDep, // -- sources,
  //     libraryClassName -- sources,
  //     internalDependencies = internalDependencies, // -- classesInSources,
  //     externalDependencies = externalDependencies, // -- classesInSources,
  //     classes, // -- sources,
  //     names = names, // -- classesInSources,
  //     productClassName = productClassName, // -- classesInSources
  //   )
  // }

  def --(sources: Iterable[VirtualFileRef]) = {
    val classesInSources = sources.flatMap(classNames)
    new MRelationsNameHashing(
      srcProd -- sources,
      libraryDep -- sources,
      libraryClassName, // -- sources,
      internalDependencies = internalDependencies -- classesInSources,
      externalDependencies = externalDependencies -- classesInSources,
      classes -- sources,
      names = names -- classesInSources,
      productClassName = productClassName -- classesInSources
    )
  }

  override def equals(other: Any) = other match {
    case o: MRelationsNameHashing =>
      srcProd == o.srcProd && libraryDep == o.libraryDep && memberRef == o.memberRef &&
        inheritance == o.inheritance && classes == o.classes
    case _ => false
  }

  override def hashCode =
    (srcProd :: libraryDep :: libraryClassName :: memberRef :: inheritance :: classes :: Nil).hashCode

  override def toString: String = {
    val internalDepsStr = (internalDependencies.dependencies map {
      case (k, vs) => k + " " + relation_s(vs)
    }).mkString("\n    ", "\n    ", "")
    val externalDepsStr = (externalDependencies.dependencies map {
      case (k, vs) => k + " " + relation_s(vs)
    }).mkString("\n    ", "\n    ", "")
    s"""
    |Relations (with name hashing enabled):
    |  products: ${relation_s(srcProd)}
    |  library deps: ${relation_s(libraryDep)}
    |  library class names: ${relation_s(libraryClassName)}
    |  internalDependencies: $internalDepsStr
    |  externalDependencies: $externalDepsStr
    |  class names: ${relation_s(classes)}
    |  used names: ${relation_s(names)}
    |  product class names: ${relation_s(productClassName)}
    """.trim.stripMargin
  }
}
