/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package sbt
package internal
package inc

import java.lang.reflect.{ Array => _, _ }
import java.lang.annotation.Annotation
import annotation.tailrec
import inc.classfile.ClassFile
import xsbti.api
import xsbti.api.SafeLazyProxy
import collection.mutable
import sbt.io.IO

object ClassToAPI {
  def apply(c: Seq[Class[_]]): Seq[api.ClassLike] = process(c)._1

  // (api, public inherited classes)
  def process(
      classes: Seq[Class[_]]): (Seq[api.ClassLike], Seq[String], Set[(Class[_], Class[_])]) = {
    val cmap = emptyClassMap
    classes.foreach(toDefinitions(cmap)) // force recording of class definitions
    cmap.lz.foreach(_.get()) // force thunks to ensure all inherited dependencies are recorded
    val classApis = cmap.allNonLocalClasses.toSeq
    val mainClasses = cmap.mainClasses.toSeq
    val inDeps = cmap.inherited.toSet
    cmap.clear()
    (classApis, mainClasses, inDeps)
  }

  // Avoiding implicit allocation.
  private def arrayMap[T <: AnyRef, U <: AnyRef: reflect.ClassTag](xs: Array[T])(
      f: T => U): Array[U] = {
    val len = xs.length
    var i = 0
    val res = new Array[U](len)
    while (i < len) {
      res(i) = f(xs(i))
      i += 1
    }
    res
  }

  def packages(c: Seq[Class[_]]): Set[String] =
    c.flatMap(packageName).toSet

  def isTopLevel(c: Class[_]): Boolean =
    c.getEnclosingClass eq null

  final class ClassMap private[sbt] (
      private[sbt] val memo: mutable.Map[String, Seq[api.ClassLikeDef]],
      private[sbt] val inherited: mutable.Set[(Class[_], Class[_])],
      private[sbt] val lz: mutable.Buffer[xsbti.api.Lazy[_]],
      private[sbt] val allNonLocalClasses: mutable.Set[api.ClassLike],
      private[sbt] val mainClasses: mutable.Set[String]
  ) {
    def clear(): Unit = {
      memo.clear()
      inherited.clear()
      lz.clear()
    }
  }
  def emptyClassMap: ClassMap =
    new ClassMap(new mutable.HashMap,
                 new mutable.HashSet,
                 new mutable.ListBuffer,
                 new mutable.HashSet,
                 new mutable.HashSet)

  /**
   * The binary name encoding for Scala has been broken for a while. It does not
   * follow the Java source name rules because it does not append a `$` after the
   * mirror class of an object, which already ends in `$`.
   *
   * Zinc works around this fix in the only way it can: it gets the enclosing class,
   * which is obtained from the `InnerClasses` java classfile metadata, and it
   * reconstructs the path from there. It circumvents, by design, the core issue:
   * it doesn't call Java's name parser at all.
   *
   * This issue has been fixed in the JDK9, but unfortunately it's going to be tormenting
   * us for a while because 2.13 does not target Java 9, and we're still at 2.12.x.
   *
   * <a href="http://bugs.java.com/bugdatabase/view_bug.do?bug_id=8057919">This is the fix in Java 9.</a>
   *
   * In order to fully fix this issue, the Scala compiler should do the desired behaviour
   * in 2.13.x. 2.12.x is already lost since this fix is binary incompatible. The appropriate
   * issue is here: https://github.com/scala/bug/issues/2034.
   *
   * @return The canonical name if not null, the blank string otherwise.
   */
  def handleMalformedNameOf(c: Class[_], isRecursive: Boolean = false): String = {
    if (c == null) "" // Return nothing if it hits the top-level class
    else {
      val className = c.getName
      // Adds a `dollar` if it's a recursive call, else nothing
      val atEnd: String = if (isRecursive) "$" else ""
      try {
        val canonicalName = c.getCanonicalName
        if (canonicalName == null) className
        else canonicalName + atEnd
      } catch {
        case malformedError: java.lang.InternalError
            if malformedError.getMessage.contains("Malformed class name") =>
          val enclosingClass = c.getEnclosingClass
          val enclosingName = enclosingClass.getName
          val restOfName = c.getName.stripPrefix(enclosingName)
          handleMalformedNameOf(enclosingClass, true) + restOfName + atEnd
      }
    }
  }

  def classCanonicalName(c: Class[_]): String = handleMalformedNameOf(c)

  def toDefinitions(cmap: ClassMap)(c: Class[_]): Seq[api.ClassLikeDef] =
    cmap.memo.getOrElseUpdate(classCanonicalName(c), toDefinitions0(c, cmap))

  def toDefinitions0(c: Class[_], cmap: ClassMap): Seq[api.ClassLikeDef] = {
    import api.DefinitionType.{ ClassDef, Module, Trait }
    val enclPkg = packageName(c)
    val mods = modifiers(c.getModifiers)
    val acc = access(c.getModifiers, enclPkg)
    val annots = annotations(c.getAnnotations)
    val children = childrenOfSealedClass(c)
    val topLevel = c.getEnclosingClass == null
    val name = classCanonicalName(c)
    val tpe = if (Modifier.isInterface(c.getModifiers)) Trait else ClassDef
    lazy val (static, instance) = structure(c, enclPkg, cmap)
    val cls = api.ClassLike.of(name,
                               acc,
                               mods,
                               annots,
                               tpe,
                               lzyS(Empty),
                               lzy(instance, cmap),
                               emptyStringArray,
                               children.toArray,
                               topLevel,
                               typeParameters(typeParameterTypes(c)))
    val clsDef =
      api.ClassLikeDef.of(name, acc, mods, annots, typeParameters(typeParameterTypes(c)), tpe)
    val stat = api.ClassLike.of(name,
                                acc,
                                mods,
                                annots,
                                Module,
                                lzyS(Empty),
                                lzy(static, cmap),
                                emptyStringArray,
                                emptyTypeArray,
                                topLevel,
                                emptyTypeParameterArray)
    val statDef = api.ClassLikeDef.of(name, acc, mods, annots, emptyTypeParameterArray, Module)
    val defs = cls :: stat :: Nil
    val defsEmptyMembers = clsDef :: statDef :: Nil
    cmap.memo(name) = defsEmptyMembers
    cmap.allNonLocalClasses ++= defs

    if (c.getMethods.exists(
          meth =>
            meth.getName == "main" &&
              Modifier.isStatic(meth.getModifiers) &&
              meth.getParameterTypes.length == 1 &&
              meth.getParameterTypes.head == classOf[Array[String]] &&
              meth.getReturnType == java.lang.Void.TYPE)) {
      cmap.mainClasses += name
    }

    defsEmptyMembers
  }

  /** Returns the (static structure, instance structure, inherited classes) for `c`. */
  def structure(c: Class[_],
                enclPkg: Option[String],
                cmap: ClassMap): (api.Structure, api.Structure) = {
    lazy val cf = classFileForClass(c)
    val methods = mergeMap(c, c.getDeclaredMethods, c.getMethods, methodToDef(enclPkg))
    val fields = mergeMap(c, c.getDeclaredFields, c.getFields, fieldToDef(c, cf, enclPkg))
    val constructors =
      mergeMap(c, c.getDeclaredConstructors, c.getConstructors, constructorToDef(enclPkg))
    val cl = c.getClassLoader
    val name = c.getCanonicalName
    val innerClasses = new mutable.ArrayBuffer[Class[_]]()
    val publicClasses = new mutable.ArrayBuffer[Class[_]]()
    val it = cf.innerClasses.toIterator
    while (it.hasNext) {
      val info = it.next
      try {
        if (info.outerClassName == name) {
          val innerClass = cl.loadClass(info.innerClassName)
          innerClasses += innerClass
          if (info.isPublic) {
            publicClasses += innerClass
          }
        }
      } catch {
        case _: ClassNotFoundException => // do nothing
      }
    }
    val classes = merge[Class[_]](c,
                                  innerClasses.toArray[Class[_]],
                                  publicClasses.toArray[Class[_]],
                                  toDefinitions(cmap),
                                  (_: Seq[Class[_]]).partition(isStatic),
                                  _.getEnclosingClass != c)
    val all = methods ++ fields ++ constructors ++ classes
    val parentJavaTypes = allSuperTypes(c)
    if (!Modifier.isPrivate(c.getModifiers))
      cmap.inherited ++= parentJavaTypes.collect { case parent: Class[_] => c -> parent }
    val parentTypes = types(parentJavaTypes)
    val instanceStructure =
      api.Structure.of(lzyS(parentTypes), lzyS(all.declared.toArray), lzyS(all.inherited.toArray))
    val staticStructure = api.Structure.of(lzyEmptyTpeArray,
                                           lzyS(all.staticDeclared.toArray),
                                           lzyS(all.staticInherited.toArray))
    (staticStructure, instanceStructure)
  }

  /** TODO: over time, ClassToAPI should switch the majority of access to the classfile parser */
  private[this] def classFileForClass(c: Class[_]): ClassFile =
    classfile.Parser.apply(IO.classfileLocation(c))

  @inline private[this] def lzyS[T <: AnyRef](t: T): xsbti.api.Lazy[T] = SafeLazyProxy.strict(t)
  @inline final def lzy[T <: AnyRef](t: => T): xsbti.api.Lazy[T] = SafeLazyProxy(t)
  private[this] def lzy[T <: AnyRef](t: => T, cmap: ClassMap): xsbti.api.Lazy[T] = {
    val s = lzy(t)
    cmap.lz += s
    s
  }

  private val emptyStringArray = new Array[String](0)
  private val emptyTypeArray = new Array[xsbti.api.Type](0)
  private val emptyAnnotationArray = new Array[xsbti.api.Annotation](0)
  private val emptyTypeParameterArray = new Array[xsbti.api.TypeParameter](0)
  private val lzyEmptyTpeArray = lzyS(emptyTypeArray)
  private val lzyEmptyDefArray = lzyS(new Array[xsbti.api.ClassDefinition](0))

  private def allSuperTypes(t: Type): Seq[Type] = {
    @tailrec def accumulate(t: Type, accum: Seq[Type] = Seq.empty): Seq[Type] = t match {
      case c: Class[_] =>
        val (parent, interfaces) = (c.getGenericSuperclass, c.getGenericInterfaces)
        accumulate(parent, (accum :+ parent) ++ flattenAll(interfaces))
      case p: ParameterizedType =>
        accumulate(p.getRawType, accum)
      case _ =>
        accum
    }
    @tailrec def flattenAll(interfaces: Seq[Type], accum: Seq[Type] = Seq.empty): Seq[Type] = {
      if (interfaces.nonEmpty) {
        val raw = interfaces map { case p: ParameterizedType => p.getRawType; case i => i }
        val children = raw flatMap {
          case i: Class[_] => i.getGenericInterfaces; case _ => Seq.empty
        }
        flattenAll(children, accum ++ interfaces ++ children)
      } else
        accum
    }
    accumulate(t).filterNot(_ == null).distinct
  }

  def types(ts: Seq[Type]): Array[api.Type] =
    ts.filter(_ ne null).map(reference).toArray
  def upperBounds(ts: Array[Type]): api.Type =
    api.Structure.of(lzy(types(ts)), lzyEmptyDefArray, lzyEmptyDefArray)

  @deprecated("No longer used", "0.13.0")
  def parents(c: Class[_]): Seq[api.Type] = types(allSuperTypes(c))

  @deprecated("Use fieldToDef[4] instead", "0.13.9")
  def fieldToDef(enclPkg: Option[String])(f: Field): api.FieldLike = {
    val c = f.getDeclaringClass
    fieldToDef(c, classFileForClass(c), enclPkg)(f)
  }

  def fieldToDef(c: Class[_], cf: => ClassFile, enclPkg: Option[String])(
      f: Field
  ): api.FieldLike = {
    val name = f.getName
    val accs = access(f.getModifiers, enclPkg)
    val mods = modifiers(f.getModifiers)
    val annots = annotations(f.getDeclaredAnnotations)
    val fieldTpe = reference(returnType(f))
    // generate a more specific type for constant fields
    val specificTpe: Option[api.Type] =
      if (mods.isFinal) {
        try {
          cf.constantValue(name).map(singletonForConstantField(c, f, _))
        } catch {
          case e: Throwable =>
            throw new IllegalStateException(
              s"Failed to parse class $c: this may mean your classfiles are corrupted. Please clean and try again.",
              e
            )
        }
      } else {
        None
      }
    val tpe = specificTpe.getOrElse(fieldTpe)
    if (mods.isFinal) {
      api.Val.of(name, accs, mods, annots, tpe)
    } else {
      api.Var.of(name, accs, mods, annots, tpe)
    }
  }

  /**
   * Creates a Singleton type that includes both the type and ConstantValue for the given Field.
   *
   * Since java compilers are allowed to inline constant (static final primitive) fields in
   * downstream classfiles, we generate a type that will cause APIs to match only when both
   * the type and value of the field match. We include the classname mostly for readability.
   *
   * Because this type is purely synthetic, it's fine that the name might contain filename-
   * banned characters.
   */
  private def singletonForConstantField(c: Class[_], field: Field, constantValue: AnyRef) =
    api.Singleton.of(
      pathFromStrings(
        c.getName
          .split("\\.")
          .toSeq :+ (field.getName + "$" + returnType(field) + "$" + constantValue)
      )
    )

  def methodToDef(enclPkg: Option[String])(m: Method): api.Def =
    defLike(
      m.getName,
      m.getModifiers,
      m.getDeclaredAnnotations,
      typeParameterTypes(m),
      m.getParameterAnnotations,
      parameterTypes(m),
      Option(returnType(m)),
      exceptionTypes(m),
      m.isVarArgs,
      enclPkg
    )

  /** Use the unique constructor format defined in [[xsbt.ClassName.constructorName]]. */
  private def uniqueConstructorName(constructor: Constructor[_]): String =
    s"${name(constructor).replace('.', ';')};init;"
  def constructorToDef(enclPkg: Option[String])(c: Constructor[_]): api.Def =
    defLike(
      uniqueConstructorName(c),
      c.getModifiers,
      c.getDeclaredAnnotations,
      typeParameterTypes(c),
      c.getParameterAnnotations,
      parameterTypes(c),
      None,
      exceptionTypes(c),
      c.isVarArgs,
      enclPkg
    )

  def defLike[T <: GenericDeclaration](name: String,
                                       mods: Int,
                                       annots: Array[Annotation],
                                       tps: Array[TypeVariable[T]],
                                       paramAnnots: Array[Array[Annotation]],
                                       paramTypes: Array[Type],
                                       retType: Option[Type],
                                       exceptions: Array[Type],
                                       varArgs: Boolean,
                                       enclPkg: Option[String]): api.Def = {
    val varArgPosition = if (varArgs) paramTypes.length - 1 else -1
    val isVarArg = List.tabulate(paramTypes.length)(_ == varArgPosition)
    val pa = (paramAnnots, paramTypes, isVarArg).zipped map {
      case (a, p, v) => parameter(a, p, v)
    }
    val params = api.ParameterList.of(pa, false)
    val ret = retType match { case Some(rt) => reference(rt); case None => Empty }
    api.Def.of(name,
               access(mods, enclPkg),
               modifiers(mods),
               annotations(annots) ++ exceptionAnnotations(exceptions),
               typeParameters(tps),
               Array(params),
               ret)
  }

  def exceptionAnnotations(exceptions: Array[Type]): Array[api.Annotation] =
    if (exceptions.length == 0) emptyAnnotationArray
    else
      arrayMap(exceptions)(t =>
        api.Annotation.of(Throws, Array(api.AnnotationArgument.of("value", t.toString))))

  def parameter(annots: Array[Annotation], parameter: Type, varArgs: Boolean): api.MethodParameter =
    api.MethodParameter.of(
      "",
      annotated(reference(parameter), annots),
      false,
      if (varArgs) api.ParameterModifier.Repeated else api.ParameterModifier.Plain)

  def annotated(t: api.Type, annots: Array[Annotation]): api.Type = (
    if (annots.length == 0) t
    else api.Annotated.of(t, annotations(annots))
  )

  case class Defs(declared: Seq[api.ClassDefinition],
                  inherited: Seq[api.ClassDefinition],
                  staticDeclared: Seq[api.ClassDefinition],
                  staticInherited: Seq[api.ClassDefinition]) {
    def ++(o: Defs) =
      Defs(declared ++ o.declared,
           inherited ++ o.inherited,
           staticDeclared ++ o.staticDeclared,
           staticInherited ++ o.staticInherited)
  }
  def mergeMap[T <: Member](of: Class[_],
                            self: Seq[T],
                            public: Seq[T],
                            f: T => api.ClassDefinition): Defs =
    merge[T](of, self, public, x => f(x) :: Nil, splitStatic, _.getDeclaringClass != of)

  def merge[T](of: Class[_],
               self: Seq[T],
               public: Seq[T],
               f: T => Seq[api.ClassDefinition],
               splitStatic: Seq[T] => (Seq[T], Seq[T]),
               isInherited: T => Boolean): Defs = {
    val (selfStatic, selfInstance) = splitStatic(self)
    val (inheritedStatic, inheritedInstance) = splitStatic(public filter isInherited)
    Defs(selfInstance flatMap f,
         inheritedInstance flatMap f,
         selfStatic flatMap f,
         inheritedStatic flatMap f)
  }

  def splitStatic[T <: Member](defs: Seq[T]): (Seq[T], Seq[T]) =
    defs partition isStatic

  def isStatic(c: Class[_]): Boolean = Modifier.isStatic(c.getModifiers)
  def isStatic(a: Member): Boolean = Modifier.isStatic(a.getModifiers)

  def typeParameters[T <: GenericDeclaration](
      tps: Array[TypeVariable[T]]): Array[api.TypeParameter] =
    if (tps.length == 0) emptyTypeParameterArray
    else arrayMap(tps)(typeParameter)

  def typeParameter[T <: GenericDeclaration](tp: TypeVariable[T]): api.TypeParameter =
    api.TypeParameter.of(typeVariable(tp),
                         emptyAnnotationArray,
                         emptyTypeParameterArray,
                         api.Variance.Invariant,
                         NothingRef,
                         upperBounds(tp.getBounds))

  // needs to be stable across compilations
  def typeVariable[T <: GenericDeclaration](tv: TypeVariable[T]): String =
    name(tv.getGenericDeclaration) + " " + tv.getName

  def reduceHash(in: Array[Byte]): Int =
    (0 /: in)((acc, b) => (acc * 43) ^ b)

  def name(gd: GenericDeclaration): String =
    gd match {
      case c: Class[_]       => classCanonicalName(c)
      case m: Method         => m.getName
      case c: Constructor[_] => c.getName
    }

  def modifiers(i: Int): api.Modifiers = {
    import Modifier.{ isAbstract, isFinal }
    new api.Modifiers(isAbstract(i), false, isFinal(i), false, false, false, false, false)
  }
  def access(i: Int, pkg: Option[String]): api.Access = {
    import Modifier.{ isPublic, isPrivate, isProtected }
    if (isPublic(i)) Public
    else if (isPrivate(i)) Private
    else if (isProtected(i)) Protected
    else packagePrivate(pkg)
  }

  def annotations(a: Array[Annotation]): Array[api.Annotation] =
    if (a.length == 0) emptyAnnotationArray else arrayMap(a)(annotation)
  def annotation(a: Annotation): api.Annotation =
    api.Annotation.of(reference(a.annotationType), Array(javaAnnotation(a.toString)))

  /**
   * This method mimics Scala compiler's behavior of `Symbol.children` method when Symbol corresponds to
   * a Java-defined enum class. Java's enum is modelled as a sealed class and enum's constants are modelled as
   * children.
   *
   * We need this logic to trigger recompilation due to changes to pattern exhaustivity checking results.
   */
  private def childrenOfSealedClass(c: Class[_]): Seq[api.Type] =
    if (!c.isEnum) emptyTypeArray
    else {
      // Calling getCanonicalName() on classes from enum constants yields same string as enumClazz.getCanonicalName
      // Moreover old behaviour create new instance of enum - what may fail (e.g. in static block )
      Array(reference(c))
    }

  // full information not available from reflection
  def javaAnnotation(s: String): api.AnnotationArgument =
    api.AnnotationArgument.of("toString", s)

  def array(tpe: api.Type): api.Type = api.Parameterized.of(ArrayRef, Array(tpe))
  def reference(c: Class[_]): api.Type =
    if (c.isArray) array(reference(c.getComponentType))
    else if (c.isPrimitive) primitive(c.getName)
    else reference(classCanonicalName(c))

  // does not handle primitives
  def reference(s: String): api.Type = {
    val (pkg, cls) = packageAndName(s)
    pkg match {
      // translate all primitives?
      case None => api.Projection.of(Empty, cls)
      case Some(p) =>
        api.Projection.of(api.Singleton.of(pathFromString(p)), cls)
    }
  }

  // sbt/zinc#389: Ignore nulls coming from generic parameter types of lambdas
  private[this] def ignoreNulls[T](genericTypes: Array[T]): Array[T] =
    genericTypes.filter(_ != null)

  def referenceP(t: ParameterizedType): api.Parameterized = {
    val targs = ignoreNulls(t.getActualTypeArguments)
    val args = if (targs.isEmpty) emptyTypeArray else arrayMap(targs)(t => reference(t): api.Type)
    val base = reference(t.getRawType)
    api.Parameterized.of(base, args)
  }

  def reference(t: Type): api.Type =
    t match {
      case _: WildcardType       => reference("_")
      case tv: TypeVariable[_]   => api.ParameterRef.of(typeVariable(tv))
      case pt: ParameterizedType => referenceP(pt)
      case gat: GenericArrayType => array(reference(gat.getGenericComponentType))
      case c: Class[_]           => reference(c)
    }

  def pathFromString(s: String): api.Path =
    pathFromStrings(s.split("\\."))
  def pathFromStrings(ss: Seq[String]): api.Path =
    api.Path.of((ss.map(api.Id.of(_)) :+ ThisRef).toArray)
  def packageName(c: Class[_]) = packageAndName(c)._1
  def packageAndName(c: Class[_]): (Option[String], String) =
    packageAndName(c.getName)
  def packageAndName(name: String): (Option[String], String) = {
    val lastDot = name.lastIndexOf('.')
    if (lastDot >= 0)
      (Some(name.substring(0, lastDot)), name.substring(lastDot + 1))
    else
      (None, name)
  }

  val Empty = api.EmptyType.of()
  val ThisRef = api.This.of()

  val Public = api.Public.of()
  val Unqualified = api.Unqualified.of()
  val Private = api.Private.of(Unqualified)
  val Protected = api.Protected.of(Unqualified)
  def packagePrivate(pkg: Option[String]): api.Access =
    api.Private.of(api.IdQualifier.of(pkg getOrElse ""))

  val ArrayRef = reference("scala.Array")
  val Throws = reference("scala.throws")
  val NothingRef = reference("scala.Nothing")

  private[this] def PrimitiveNames =
    Seq("boolean", "byte", "char", "short", "int", "long", "float", "double")
  private[this] def PrimitiveMap = PrimitiveNames.map(j => (j, j.capitalize)) :+ ("void" -> "Unit")
  private[this] val PrimitiveRefs = PrimitiveMap.map {
    case (n, sn) => (n, reference("scala." + sn))
  }.toMap
  def primitive(name: String): api.Type = PrimitiveRefs(name)

  private[this] def returnType(f: Field): Type = f.getGenericType
  private[this] def returnType(m: Method): Type = m.getGenericReturnType
  private[this] def exceptionTypes(c: Constructor[_]): Array[Type] = c.getGenericExceptionTypes

  private[this] def exceptionTypes(m: Method): Array[Type] = m.getGenericExceptionTypes

  private[this] def parameterTypes(m: Method): Array[Type] =
    ignoreNulls(m.getGenericParameterTypes)

  private[this] def parameterTypes(c: Constructor[_]): Array[Type] =
    ignoreNulls(c.getGenericParameterTypes)

  private[this] def typeParameterTypes[T](m: Constructor[T]): Array[TypeVariable[Constructor[T]]] =
    m.getTypeParameters
  private[this] def typeParameterTypes[T](m: Class[T]): Array[TypeVariable[Class[T]]] =
    m.getTypeParameters
  private[this] def typeParameterTypes(m: Method): Array[TypeVariable[Method]] =
    m.getTypeParameters
}
