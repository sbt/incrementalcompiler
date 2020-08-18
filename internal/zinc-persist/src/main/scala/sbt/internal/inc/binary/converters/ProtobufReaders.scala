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

package sbt.internal.inc.binary.converters

import com.github.ghik.silencer.silent
import java.nio.file.{ Path, Paths }
import java.util.{ List => JList, Map => JMap }
import sbt.internal.inc.Relations.ClassDependencies
import sbt.internal.inc._
import sbt.internal.inc.binary.converters.ProtobufDefaults.EmptyLazyCompanions
import sbt.util.InterfaceUtil
import xsbti.{ Position, Problem, Severity, T2, UseScope, VirtualFileRef }
import xsbti.compile.{ CompileOrder, FileHash, MiniOptions, MiniSetup, Output, OutputGroup }
import xsbti.compile.analysis.{ Compilation, ReadMapper, SourceInfo, Stamp }
import sbt.internal.inc.binary.converters.ProtobufDefaults.Feedback.StringToException
import sbt.internal.inc.binary.converters.ProtobufDefaults.Feedback.{ Readers => ReadersFeedback }
import sbt.internal.inc.binary.converters.ProtobufDefaults.{ Classes, ReadersConstants }
import sbt.internal.util.Relation
import scala.collection.JavaConverters._
import xsbti.api._

import ProtobufDefaults.{ MissingString, MissingInt }

final class ProtobufReaders(mapper: ReadMapper, currentVersion: Schema.Version) {
  def fromPathString(path: String): Path = Paths.get(path)
  def fromPathStringV(path: String): VirtualFileRef = {
    VirtualFileRef.of(path)
  }

  def fromStampType(stampType: Schema.Stamps.StampType): Stamp = {
    import sbt.internal.inc.{ EmptyStamp, LastModified, Hash }
    stampType.getTypeCase match {
      case Schema.Stamps.StampType.TypeCase.TYPE_NOT_SET => EmptyStamp
      case Schema.Stamps.StampType.TypeCase.FARMHASH =>
        FarmHash.fromLong(stampType.getFarmHash.getHash)
      case Schema.Stamps.StampType.TypeCase.HASH =>
        Hash.unsafeFromString(stampType.getHash.getHash) // fair assumption
      case Schema.Stamps.StampType.TypeCase.LASTMODIFIED =>
        new LastModified(stampType.getLastModified.getMillis)
    }
  }

  def fromStamps(stamps: Schema.Stamps): Stamps = {
    // Note that boilerplate here is inteded, abstraction is expensive
    def fromBinarySchemaMap(
        stamps: JMap[String, Schema.Stamps.StampType]
    ): Map[VirtualFileRef, Stamp] = {
      stamps.asScala.iterator.map {
        case (path, schemaStamp) =>
          val file = fromPathStringV(path)
          val newFile = mapper.mapBinaryFile(file)
          val stamp = fromStampType(schemaStamp)
          val newStamp = mapper.mapBinaryStamp(newFile, stamp)
          newFile -> newStamp
      }.toMap
    }

    def fromSourceSchemaMap(
        stamps: JMap[String, Schema.Stamps.StampType]
    ): Map[VirtualFileRef, Stamp] = {
      stamps.asScala.iterator.map {
        case (path, schemaStamp) =>
          val file = fromPathStringV(path)
          val newFile = mapper.mapSourceFile(file)
          val stamp = fromStampType(schemaStamp)
          val newStamp = mapper.mapSourceStamp(newFile, stamp)
          newFile -> newStamp
      }.toMap
    }

    def fromProductSchemaMap(
        stamps: JMap[String, Schema.Stamps.StampType]
    ): Map[VirtualFileRef, Stamp] = {
      stamps.asScala.iterator.map {
        case (path, schemaStamp) =>
          val file = fromPathStringV(path)
          val newFile = mapper.mapProductFile(file)
          val stamp = fromStampType(schemaStamp)
          val newStamp = mapper.mapProductStamp(newFile, stamp)
          newFile -> newStamp
      }.toMap
    }

    val libraries = fromBinarySchemaMap(stamps.getBinaryStampsMap)
    val sources = fromSourceSchemaMap(stamps.getSourceStampsMap)
    val products = fromProductSchemaMap(stamps.getProductStampsMap)
    Stamps(
      products = products,
      sources = sources,
      libraries = libraries,
    )
  }

  def fromOutputGroup(outputGroup: Schema.OutputGroup): OutputGroup = {
    val sourcePath = fromPathString(outputGroup.getSourcePath)
    val sourceDir = mapper.mapSourceDir(sourcePath)
    val targetPath = fromPathString(outputGroup.getTargetPath)
    val targetDir = mapper.mapOutputDir(targetPath)
    CompileOutput.outputGroup(sourceDir, targetDir)
  }

  def fromCompilationOutput(c: Schema.Compilation): Output = {
    import Schema.Compilation.{ OutputCase => CompilationOutput }
    c.getOutputCase match {
      case CompilationOutput.SINGLEOUTPUT =>
        val single = c.getSingleOutput
        val target = fromPathString(single.getTarget)
        val outputDir = mapper.mapOutputDir(target)
        CompileOutput(outputDir)
      case CompilationOutput.MULTIPLEOUTPUT =>
        val multiple = c.getMultipleOutput
        val groups = multiple.getOutputGroupsList.asScala.iterator.map(fromOutputGroup).toArray
        CompileOutput(groups)
      case CompilationOutput.OUTPUT_NOT_SET =>
        ReadersFeedback.ExpectedOutputInCompilationOutput.!!
    }
  }

  def fromCompilation(compilation: Schema.Compilation): Compilation = {
    val output = fromCompilationOutput(compilation)
    new sbt.internal.inc.Compilation(compilation.getStartTimeMillis, output)
  }

  def fromCompilations(compilations0: Schema.Compilations): Compilations = {
    val compilations = compilations0.getCompilationsList.asScala.iterator.map(fromCompilation)
    val castedCompilations = (compilations.map { case c: sbt.internal.inc.Compilation => c }).toSeq
    Compilations.of(castedCompilations)
  }

  def fromPosition(position: Schema.Position): Position = {
    InterfaceUtil.position(
      line0 = fromInt(position.getLine),
      content = position.getLineContent,
      offset0 = fromInt(position.getOffset),
      pointer0 = fromInt(position.getPointer),
      pointerSpace0 = fromString(position.getPointerSpace),
      sourcePath0 = fromString(position.getSourcePath),
      sourceFile0 = fromString(position.getSourceFilepath).map(fromPathString).map(_.toFile),
      startOffset0 = fromInt(position.getStartOffset),
      endOffset0 = fromInt(position.getEndOffset),
      startLine0 = fromInt(position.getStartLine),
      startColumn0 = fromInt(position.getStartColumn),
      endLine0 = fromInt(position.getEndLine),
      endColumn0 = fromInt(position.getEndColumn),
    )
  }

  def fromSeverity(severity: Schema.Severity, id: Int): Severity = {
    severity match {
      case Schema.Severity.INFO         => Severity.Info
      case Schema.Severity.WARN         => Severity.Warn
      case Schema.Severity.ERROR        => Severity.Error
      case Schema.Severity.UNRECOGNIZED => ReadersFeedback.unrecognizedSeverity(id).!!
    }
  }

  private def fromString(value: String): Option[String] =
    if (value == MissingString) None else Some(value)

  private def fromInt(value: Int): Option[Integer] =
    if (value == MissingInt) None else Some(value)

  def fromProblem(problem: Schema.Problem): Problem = {
    val category = problem.getCategory
    val message = problem.getMessage
    val severity = fromSeverity(problem.getSeverity, problem.getSeverityValue)
    val position =
      if (problem.hasPosition) fromPosition(problem.getPosition)
      else ReadersFeedback.ExpectedPositionInProblem.!!
    val rendered = fromString(problem.getRendered)
    InterfaceUtil.problem(category, position, message, severity, rendered)
  }

  def fromSourceInfo(sourceInfo: Schema.SourceInfo): SourceInfo = {
    val mainClasses = sourceInfo.getMainClassesList.asScala.toSeq
    val reportedProblems =
      sourceInfo.getReportedProblemsList.asScala.iterator.map(fromProblem).toSeq
    val unreportedProblems =
      sourceInfo.getUnreportedProblemsList.asScala.iterator.map(fromProblem).toSeq
    SourceInfos.makeInfo(
      reported = reportedProblems,
      unreported = unreportedProblems,
      mainClasses = mainClasses
    )
  }

  def fromSourceInfos(sourceInfos0: Schema.SourceInfos): SourceInfos = {
    val sourceInfos = sourceInfos0.getSourceInfosMap.asScala.iterator.map {
      case (path, value) =>
        val file = mapper.mapSourceFile(fromPathStringV(path))
        val sourceInfo = fromSourceInfo(value)
        file -> sourceInfo
    }
    SourceInfos.of(sourceInfos.toMap)
  }

  def fromClasspathFileHash(fileHash: Schema.FileHash): FileHash = {
    val hash = fileHash.getHash
    val classpathEntry = fromPathString(fileHash.getPath)
    val newClasspathEntry = mapper.mapClasspathEntry(classpathEntry)
    FileHash.of(newClasspathEntry, hash)
  }

  def fromMiniOptions(miniOptions: Schema.MiniOptions): MiniOptions = {
    val classpathHash =
      miniOptions.getClasspathHashList.asScala.iterator.map(fromClasspathFileHash).toArray
    val javacOptions =
      miniOptions.getJavacOptionsList.asScala.iterator.map(mapper.mapJavacOption).toArray
    val scalacOptions =
      miniOptions.getScalacOptionsList.asScala.iterator.map(mapper.mapScalacOption).toArray
    MiniOptions.of(classpathHash, scalacOptions, javacOptions)
  }

  def fromCompileOrder(compileOrder: Schema.CompileOrder, id: Int): CompileOrder = {
    compileOrder match {
      case Schema.CompileOrder.MIXED         => CompileOrder.Mixed
      case Schema.CompileOrder.JAVATHENSCALA => CompileOrder.JavaThenScala
      case Schema.CompileOrder.SCALATHENJAVA => CompileOrder.ScalaThenJava: @silent
      case Schema.CompileOrder.UNRECOGNIZED  => ReadersFeedback.unrecognizedOrder(id).!!
    }
  }

  def fromStringTuple(tuple: Schema.Tuple): T2[String, String] = {
    InterfaceUtil.t2(tuple.getFirst -> tuple.getSecond)
  }

  def fromMiniSetupOutput(miniSetup: Schema.MiniSetup): Output = {
    import Schema.MiniSetup.{ OutputCase => MiniSetupOutput }
    miniSetup.getOutputCase match {
      case MiniSetupOutput.SINGLEOUTPUT =>
        val single = miniSetup.getSingleOutput
        val targetDir = fromPathString(single.getTarget)
        val outputDir = mapper.mapOutputDir(targetDir)
        CompileOutput(outputDir)
      case MiniSetupOutput.MULTIPLEOUTPUT =>
        val multiple = miniSetup.getMultipleOutput
        val groups = multiple.getOutputGroupsList.asScala.iterator.map(fromOutputGroup).toArray
        CompileOutput(groups)
      case MiniSetupOutput.OUTPUT_NOT_SET =>
        ReadersFeedback.ExpectedOutputInCompilationOutput.!!
    }
  }

  def fromMiniSetup(miniSetup: Schema.MiniSetup): MiniSetup = {
    val output = fromMiniSetupOutput(miniSetup)
    val miniOptions =
      if (miniSetup.hasMiniOptions) fromMiniOptions(miniSetup.getMiniOptions)
      else ReadersFeedback.ExpectedMiniOptionsInSetup.!!
    val compilerVersion = miniSetup.getCompilerVersion
    val compileOrder = fromCompileOrder(miniSetup.getCompileOrder, miniSetup.getCompileOrderValue)
    val storeApis = miniSetup.getStoreApis
    val extra = miniSetup.getExtraList.asScala.iterator.map(fromStringTuple).toArray
    val original =
      MiniSetup.of(
        output, // note this is a dummy value
        miniOptions,
        compilerVersion,
        compileOrder,
        storeApis,
        extra
      )
    mapper.mapMiniSetup(original)
  }

  implicit class EfficientTraverse[T](seq: JList[T]) {
    def toZincArray[R: scala.reflect.ClassTag](f: T => R): Array[R] =
      seq.asScala.iterator.map(f).toArray
  }

  implicit class OptionReader[T](option: Option[T]) {
    def read[R](from: T => R, errorMessage: => String): R =
      option.fold(errorMessage.!!)(from)
  }

  def fromPath(path: Schema.Path): xsbti.api.Path = {
    def fromPathComponent(pathComponent: Schema.Path.PathComponent): PathComponent = {
      import ReadersFeedback.ExpectedPathInSuper
      import Schema.Path.{ PathComponent => SchemaPath }
      import SchemaPath.{ ComponentCase => SchemaComponent }
      import Classes.{ Component, PathComponent }
      pathComponent.getComponentCase match {
        case SchemaComponent.ID =>
          val c = pathComponent.getId
          Id.of(c.getId)
        case SchemaComponent.SUPER =>
          val c = pathComponent.getSuper
          val q =
            if (c.hasQualifier) fromPath(c.getQualifier)
            else ExpectedPathInSuper.!!
          Super.of(q)
        case SchemaComponent.THIS => ReadersConstants.This
        case SchemaComponent.COMPONENT_NOT_SET =>
          ReadersFeedback.expected(Component, PathComponent).!!
      }
    }
    val components = path.getComponentsList.toZincArray(fromPathComponent)
    xsbti.api.Path.of(components)
  }

  def fromAnnotation(annotation: Schema.Annotation): Annotation = {
    def fromAnnotationArgument(argument: Schema.AnnotationArgument): AnnotationArgument = {
      val name = argument.getName
      val value = argument.getValue
      AnnotationArgument.of(name, value)
    }

    val arguments = annotation.getArgumentsList.toZincArray(fromAnnotationArgument)
    val b =
      if (annotation.hasBase) Some(annotation.getBase)
      else None
    val base = b.read(fromType, ReadersFeedback.expectedBaseIn(Classes.Annotation))
    Annotation.of(base, arguments)
  }

  def fromStructure(tpe: Schema.Type.Structure): Structure = {
    def `lazy`[T](value: T): Lazy[T] = SafeLazyProxy.strict(value)
    val parents = `lazy`(tpe.getParentsList.toZincArray(fromType))
    val declared = `lazy`(tpe.getDeclaredList.toZincArray(fromClassDefinition))
    val inherited = `lazy`(tpe.getInheritedList.toZincArray(fromClassDefinition))
    Structure.of(parents, declared, inherited)
  }

  def fromType(`type`: Schema.Type): Type = {
    import ReadersFeedback.expectedBaseIn
    def fromParameterRef(tpe: Schema.Type.ParameterRef): ParameterRef = {
      ParameterRef.of(tpe.getId)
    }

    def fromParameterized(tpe: Schema.Type.Parameterized): Parameterized = {
      val baseType =
        if (tpe.hasBaseType) fromType(tpe.getBaseType)
        else expectedBaseIn(Classes.Parameterized).!!
      val typeArguments = tpe.getTypeArgumentsList.toZincArray(fromType)
      Parameterized.of(baseType, typeArguments)
    }

    def fromPolymorphic(tpe: Schema.Type.Polymorphic): Polymorphic = {
      val baseType =
        if (tpe.hasBaseType) fromType(tpe.getBaseType)
        else expectedBaseIn(Classes.Polymorphic).!!
      val typeParameters = tpe.getTypeParametersList.toZincArray(fromTypeParameter)
      Polymorphic.of(baseType, typeParameters)
    }

    def fromConstant(tpe: Schema.Type.Constant): Constant = {
      val baseType =
        if (tpe.hasBaseType) fromType(tpe.getBaseType)
        else expectedBaseIn(Classes.Constant).!!
      val value = tpe.getValue
      Constant.of(baseType, value)
    }

    def fromExistential(tpe: Schema.Type.Existential): Existential = {
      val b =
        if (tpe.hasBaseType) Some(tpe.getBaseType)
        else None
      val baseType = b.read(fromType, expectedBaseIn(Classes.Existential))
      val clause = tpe.getClauseList.toZincArray(fromTypeParameter)
      Existential.of(baseType, clause)
    }

    def fromSingleton(tpe: Schema.Type.Singleton): Singleton = {
      val path =
        if (tpe.hasPath) fromPath(tpe.getPath)
        else ReadersFeedback.ExpectedPathInSingleton.!!
      Singleton.of(path)
    }

    def fromProjection(tpe: Schema.Type.Projection): Projection = {
      val id = tpe.getId
      val prefix =
        if (tpe.hasPrefix) fromType(tpe.getPrefix)
        else ReadersFeedback.ExpectedPrefixInProjection.!!
      Projection.of(prefix, id)
    }

    def fromAnnotated(tpe: Schema.Type.Annotated): Annotated = {
      val baseType =
        if (tpe.hasBaseType) fromType(tpe.getBaseType)
        else expectedBaseIn(Classes.Annotated).!!
      val annotations = tpe.getAnnotationsList.toZincArray(fromAnnotation)
      Annotated.of(baseType, annotations)
    }
    import Schema.Type.ValueCase
    `type`.getValueCase match {
      case ValueCase.PARAMETERREF  => fromParameterRef(`type`.getParameterRef)
      case ValueCase.PARAMETERIZED => fromParameterized(`type`.getParameterized)
      case ValueCase.STRUCTURE     => fromStructure(`type`.getStructure)
      case ValueCase.POLYMORPHIC   => fromPolymorphic(`type`.getPolymorphic)
      case ValueCase.CONSTANT      => fromConstant(`type`.getConstant)
      case ValueCase.EXISTENTIAL   => fromExistential(`type`.getExistential)
      case ValueCase.SINGLETON     => fromSingleton(`type`.getSingleton)
      case ValueCase.PROJECTION    => fromProjection(`type`.getProjection)
      case ValueCase.ANNOTATED     => fromAnnotated(`type`.getAnnotated)
      case ValueCase.EMPTYTYPE     => ReadersConstants.EmptyType
      case ValueCase.VALUE_NOT_SET => ReadersFeedback.ExpectedNonEmptyType.!!
    }
  }

  def fromModifiers(modifiers: Schema.Modifiers): Modifiers =
    InternalApiProxy.Modifiers(modifiers.getFlags)

  def fromAccess(access: Schema.Access): Access = {
    def fromQualifier(qualifier: Schema.Qualifier): Qualifier = {
      import Schema.Qualifier.{ TypeCase => QualifierType }
      qualifier.getTypeCase match {
        case QualifierType.IDQUALIFIER =>
          val q = qualifier.getIdQualifier
          IdQualifier.of(q.getValue)
        case QualifierType.THISQUALIFIER => ReadersConstants.ThisQualifier
        case QualifierType.UNQUALIFIED   => ReadersConstants.Unqualified
        case QualifierType.TYPE_NOT_SET  => ReadersFeedback.ExpectedNonEmptyQualifier.!!
      }
    }

    def readQualifier(qualifier: Option[Schema.Qualifier]): Qualifier =
      qualifier.read(fromQualifier, ReadersFeedback.ExpectedQualifierInAccess)

    access.getTypeCase match {
      case Schema.Access.TypeCase.PUBLIC => ReadersConstants.Public
      case Schema.Access.TypeCase.PROTECTED =>
        val a = access.getProtected
        Protected.of(readQualifier(if (a.hasQualifier) Some(a.getQualifier) else None))
      case Schema.Access.TypeCase.PRIVATE =>
        val a = access.getPrivate
        Private.of(readQualifier(if (a.hasQualifier) Some(a.getQualifier) else None))
      case Schema.Access.TypeCase.TYPE_NOT_SET => ReadersFeedback.ExpectedValidAccessType.!!
    }
  }

  def fromDefinitionType(definitionType: Schema.DefinitionType): DefinitionType = {
    definitionType match {
      case Schema.DefinitionType.CLASSDEF      => DefinitionType.ClassDef
      case Schema.DefinitionType.MODULE        => DefinitionType.Module
      case Schema.DefinitionType.TRAIT         => DefinitionType.Trait
      case Schema.DefinitionType.PACKAGEMODULE => DefinitionType.PackageModule
      case Schema.DefinitionType.UNRECOGNIZED  => ReadersFeedback.UnrecognizedDefinitionType.!!
    }
  }

  def fromClassDefinition(classDefinition: Schema.ClassDefinition): ClassDefinition = {
    import ReadersFeedback.{ MissingModifiersInDef, MissingAccessInDef, expectedTypeIn }
    import ReadersFeedback.{
      ExpectedReturnTypeInDef,
      ExpectedLowerBoundInTypeDeclaration,
      ExpectedUpperBoundInTypeDeclaration
    }

    val name = classDefinition.getName
    val access =
      if (classDefinition.hasAccess) fromAccess(classDefinition.getAccess)
      else MissingAccessInDef.!!
    val modifiers =
      if (classDefinition.hasModifiers) fromModifiers(classDefinition.getModifiers)
      else MissingModifiersInDef.!!
    val annotations = classDefinition.getAnnotationsList.toZincArray(fromAnnotation)

    def fromParameterList(parameterList: Schema.ParameterList): ParameterList = {
      def fromMethodParameter(methodParameter: Schema.MethodParameter): MethodParameter = {
        def fromParameterModifier(modifier: Schema.ParameterModifier): ParameterModifier = {
          modifier match {
            case Schema.ParameterModifier.PLAIN    => ParameterModifier.Plain
            case Schema.ParameterModifier.BYNAME   => ParameterModifier.ByName
            case Schema.ParameterModifier.REPEATED => ParameterModifier.Repeated
            case Schema.ParameterModifier.UNRECOGNIZED =>
              ReadersFeedback.UnrecognizedParamModifier.!!
          }
        }
        val name = methodParameter.getName
        val hasDefault = methodParameter.getHasDefault
        val `type` =
          if (methodParameter.hasType) fromType(methodParameter.getType)
          else expectedTypeIn(Classes.MethodParameter).!!
        val modifier = fromParameterModifier(methodParameter.getModifier)
        MethodParameter.of(name, `type`, hasDefault, modifier)
      }

      val isImplicit = parameterList.getIsImplicit
      val parameters = parameterList.getParametersList.toZincArray(fromMethodParameter)
      ParameterList.of(parameters, isImplicit)
    }

    def fromClassLikeDef(defDef: Schema.ClassDefinition.ClassLikeDef): ClassLikeDef = {
      val definitionType = fromDefinitionType(defDef.getDefinitionType)
      val typeParameters = defDef.getTypeParametersList.toZincArray(fromTypeParameter)
      ClassLikeDef.of(name, access, modifiers, annotations, typeParameters, definitionType)
    }

    def fromDefDef(defDef: Schema.ClassDefinition.Def): Def = {
      val returnType =
        if (defDef.hasReturnType) fromType(defDef.getReturnType)
        else ExpectedReturnTypeInDef.!!
      val typeParameters = defDef.getTypeParametersList.toZincArray(fromTypeParameter)
      val valueParameters = defDef.getValueParametersList.toZincArray(fromParameterList)
      Def.of(name, access, modifiers, annotations, typeParameters, valueParameters, returnType)
    }

    def fromValDef(valDef: Schema.ClassDefinition.Val): Val = {
      val tpe =
        if (valDef.hasType) fromType(valDef.getType)
        else expectedTypeIn(Classes.Val).!!
      Val.of(name, access, modifiers, annotations, tpe)
    }

    def fromVarDef(varDef: Schema.ClassDefinition.Var): Var = {
      val tpe =
        if (varDef.hasType) fromType(varDef.getType)
        else expectedTypeIn(Classes.Var).!!
      Var.of(name, access, modifiers, annotations, tpe)
    }

    def fromTypeAlias(typeAlias: Schema.ClassDefinition.TypeAlias): TypeAlias = {
      val tpe =
        if (typeAlias.hasType) fromType(typeAlias.getType)
        else expectedTypeIn(Classes.TypeAlias).!!
      val typeParameters = typeAlias.getTypeParametersList.toZincArray(fromTypeParameter)
      TypeAlias.of(name, access, modifiers, annotations, typeParameters, tpe)
    }

    def fromTypeDeclaration(decl: Schema.ClassDefinition.TypeDeclaration): TypeDeclaration = {
      val lowerBound =
        if (decl.hasLowerBound) fromType(decl.getLowerBound)
        else ExpectedLowerBoundInTypeDeclaration.!!
      val upperBound =
        if (decl.hasUpperBound) fromType(decl.getUpperBound)
        else ExpectedUpperBoundInTypeDeclaration.!!
      val typeParams = decl.getTypeParametersList.toZincArray(fromTypeParameter)
      TypeDeclaration.of(name, access, modifiers, annotations, typeParams, lowerBound, upperBound)
    }

    import Schema.ClassDefinition.{ ExtraCase => DefType }
    classDefinition.getExtraCase match {
      case DefType.CLASSLIKEDEF    => fromClassLikeDef(classDefinition.getClassLikeDef)
      case DefType.DEFDEF          => fromDefDef(classDefinition.getDefDef)
      case DefType.VALDEF          => fromValDef(classDefinition.getValDef)
      case DefType.VARDEF          => fromVarDef(classDefinition.getVarDef)
      case DefType.TYPEALIAS       => fromTypeAlias(classDefinition.getTypeAlias)
      case DefType.TYPEDECLARATION => fromTypeDeclaration(classDefinition.getTypeDeclaration)
      case DefType.EXTRA_NOT_SET   => ReadersFeedback.ExpectedNonEmptyDefType.!!
    }
  }

  def fromTypeParameter(typeParameter: Schema.TypeParameter): TypeParameter = {
    def fromVariance(variance: Schema.Variance): Variance = {
      variance match {
        case Schema.Variance.INVARIANT     => Variance.Invariant
        case Schema.Variance.COVARIANT     => Variance.Covariant
        case Schema.Variance.CONTRAVARIANT => Variance.Contravariant
        case Schema.Variance.UNRECOGNIZED  => ReadersFeedback.UnrecognizedVariance.!!
      }
    }

    import ReadersFeedback.{ ExpectedLowerBoundInTypeParameter, ExpectedUpperBoundInTypeParameter }
    val id = typeParameter.getId
    val annotations = typeParameter.getAnnotationsList.toZincArray(fromAnnotation)
    val typeParameters = typeParameter.getTypeParametersList.toZincArray(fromTypeParameter)
    val variance = fromVariance(typeParameter.getVariance)
    val lowerBound =
      if (typeParameter.hasLowerBound) fromType(typeParameter.getLowerBound)
      else ExpectedLowerBoundInTypeParameter.!!
    val upperBound =
      if (typeParameter.hasUpperBound) fromType(typeParameter.getUpperBound)
      else ExpectedUpperBoundInTypeParameter.!!
    TypeParameter.of(id, annotations, typeParameters, variance, lowerBound, upperBound)
  }

  def fromClassLike(classLike: Schema.ClassLike): ClassLike = {
    def expectedMsg(msg: String) = ReadersFeedback.expected(msg, Classes.ClassLike)
    def expected(clazz: Class[_]) = expectedMsg(clazz.getName)
    val name = classLike.getName
    val access =
      if (classLike.hasAccess) fromAccess(classLike.getAccess)
      else expected(Classes.Access).!!
    val modifiers =
      if (classLike.hasModifiers) fromModifiers(classLike.getModifiers)
      else expected(Classes.Modifiers).!!
    val annotations = classLike.getAnnotationsList.toZincArray(fromAnnotation)

    import SafeLazyProxy.{ strict => mkLazy }
    val definitionType = fromDefinitionType(classLike.getDefinitionType)
    val selfType = mkLazy(
      if (classLike.hasSelfType) fromType(classLike.getSelfType) else expectedMsg("self type").!!
    )
    val structure = mkLazy(
      if (classLike.hasStructure) fromStructure(classLike.getStructure)
      else expected(Classes.Structure).!!
    )
    val savedAnnotations = classLike.getSavedAnnotationsList.asScala.toArray
    val childrenOfSealedClass = classLike.getChildrenOfSealedClassList.toZincArray(fromType)
    val topLevel = classLike.getTopLevel
    val typeParameters = classLike.getTypeParametersList.toZincArray(fromTypeParameter)
    ClassLike.of(
      name,
      access,
      modifiers,
      annotations,
      definitionType,
      selfType,
      structure,
      savedAnnotations,
      childrenOfSealedClass,
      topLevel,
      typeParameters
    )
  }

  def fromUseScope(useScope: Schema.UseScope, id: Int): UseScope = {
    useScope match {
      case Schema.UseScope.DEFAULT      => UseScope.Default
      case Schema.UseScope.IMPLICIT     => UseScope.Implicit
      case Schema.UseScope.PATMAT       => UseScope.PatMatTarget
      case Schema.UseScope.UNRECOGNIZED => ReadersFeedback.unrecognizedUseScope(id).!!
    }
  }

  def fromAnalyzedClass(
      shouldStoreApis: Boolean
  )(analyzedClass: Schema.AnalyzedClass): AnalyzedClass = {
    def fromCompanions(companions: Schema.Companions): Companions = {
      def expected(msg: String) = ReadersFeedback.expected(msg, Classes.Companions)
      val classApi =
        if (companions.hasClassApi) fromClassLike(companions.getClassApi)
        else expected("class api").!!
      val objectApi =
        if (companions.hasObjectApi) fromClassLike(companions.getObjectApi)
        else expected("object api").!!
      Companions.of(classApi, objectApi)
    }

    def fromNameHash(nameHash: Schema.NameHash): NameHash = {
      val name = nameHash.getName
      val hash = nameHash.getHash
      val scope = fromUseScope(nameHash.getScope, nameHash.getScopeValue)
      NameHash.of(name, scope, hash)
    }

    import SafeLazyProxy.{ strict => mkLazy }
    import ReadersFeedback.ExpectedCompanionsInAnalyzedClass
    val compilationTs = analyzedClass.getCompilationTimestamp
    val name = analyzedClass.getName
    val api =
      if (!shouldStoreApis) EmptyLazyCompanions
      else
        mkLazy(
          if (analyzedClass.hasApi) fromCompanions(analyzedClass.getApi)
          else ExpectedCompanionsInAnalyzedClass.!!
        )

    val apiHash = analyzedClass.getApiHash
    // Default to 0 to avoid issues when comparing hashes from two different analysis formats
    val extraHash = if (currentVersion == Schema.Version.V1) 0 else analyzedClass.getExtraHash
    val nameHashes = analyzedClass.getNameHashesList.toZincArray(fromNameHash)
    val hasMacro = analyzedClass.getHasMacro
    val provenance = analyzedClass.getProvenance.intern
    AnalyzedClass.of(compilationTs, name, api, apiHash, nameHashes, hasMacro, extraHash, provenance)
  }

  private final val stringId = identity[String] _
  private final val stringToFile = (path: String) => fromPathString(path)
  private final val stringToVFile = (path: String) => fromPathStringV(path)
  def fromRelations(relations: Schema.Relations): Relations = {
    import scala.collection.{ mutable, immutable }

    def fromMap[K, V](
        map: java.util.Map[String, Schema.Values],
        fk: String => K,
        fv: String => V
    ): Relation[K, V] = {
      val forward = mutable.HashMap[K, mutable.Builder[V, immutable.Set[V]]]()
      val reverse = mutable.HashMap[V, mutable.Builder[K, immutable.Set[K]]]()
      for ((kString, vs) <- map.asScala) {
        if (!vs.getValuesList.isEmpty) {
          val k = fk(kString)
          val vsBuilder = forward.getOrElseUpdate(k, immutable.Set.newBuilder[V])
          for (vString <- vs.getValuesList.asScala) {
            val v = fv(vString)
            vsBuilder += v
            reverse.getOrElseUpdate(v, immutable.Set.newBuilder[K]) += k
          }
        }
      }
      Relation.make[K, V](toImmutable(forward), toImmutable(reverse))
    }
    def toImmutable[A, B](
        map: mutable.Map[A, mutable.Builder[B, immutable.Set[B]]]
    ): immutable.Map[A, immutable.Set[B]] = {
      val builder = immutable.Map.newBuilder[A, immutable.Set[B]]
      for ((a, setBuilder) <- map)
        builder += (a -> setBuilder.result())
      builder.result()
    }

    def fromClassDependencies(classDependencies: Schema.ClassDependencies): ClassDependencies = {
      val internal = fromMap(classDependencies.getInternalMap, stringId, stringId)
      val external = fromMap(classDependencies.getExternalMap, stringId, stringId)
      new ClassDependencies(internal, external)
    }

    def fromUsedName(usedName: Schema.UsedName): UsedName = {
      val name = usedName.getName
      val len = usedName.getScopesCount
      val scopes = for {
        i <- 0 to len - 1
      } yield fromUseScope(usedName.getScopes(i), usedName.getScopesValue(i))
      UsedName.apply(name, scopes)
    }

    def fromUsedNamesMap(
        map: java.util.Map[String, Schema.UsedNames]
    ): Relation[String, UsedName] = {
      import scala.collection.{ mutable, immutable }
      val forward = mutable.HashMap[String, mutable.Builder[UsedName, immutable.Set[UsedName]]]()
      val reverse = mutable.HashMap[UsedName, mutable.Builder[String, immutable.Set[String]]]()
      for ((k, used) <- map.asScala) {
        val usedNames = used.getUsedNamesList.asScala
        if (!usedNames.isEmpty) {
          val usedNameBuilder = forward.getOrElseUpdate(k, immutable.Set.newBuilder[UsedName])
          for (SchemaUsedName <- usedNames) {
            val usedName = fromUsedName(SchemaUsedName)
            usedNameBuilder += usedName
            reverse.getOrElseUpdate(usedName, immutable.Set.newBuilder[String]) += k
          }
        }
      }
      Relation.make(toImmutable(forward), toImmutable(reverse))
    }

    def expected(msg: String) = ReadersFeedback.expected(msg, Classes.Relations)

    val srcProd = fromMap(relations.getSrcProdMap, stringToVFile, stringToVFile)
    val libraryDep = fromMap(relations.getLibraryDepMap, stringToVFile, stringToVFile)
    val libraryClassName =
      fromMap(relations.getLibraryClassNameMap, stringToVFile, stringId)
    val memberRef =
      if (relations.hasMemberRef) fromClassDependencies(relations.getMemberRef)
      else expected("member refs").!!
    val inheritance =
      if (relations.hasInheritance) fromClassDependencies(relations.getInheritance)
      else expected("inheritance").!!
    val localInheritance =
      if (relations.hasLocalInheritance) fromClassDependencies(relations.getLocalInheritance)
      else expected("local inheritance").!!
    val classes = fromMap(relations.getClassesMap, stringToVFile, stringId)
    val productClassName =
      fromMap(relations.getProductClassNameMap, stringId, stringId)
    val names = fromUsedNamesMap(relations.getNamesMap)
    val internal = InternalDependencies(
      Map(
        DependencyContext.DependencyByMemberRef -> memberRef.internal,
        DependencyContext.DependencyByInheritance -> inheritance.internal,
        DependencyContext.LocalDependencyByInheritance -> localInheritance.internal
      )
    )
    val external = ExternalDependencies(
      Map(
        DependencyContext.DependencyByMemberRef -> memberRef.external,
        DependencyContext.DependencyByInheritance -> inheritance.external,
        DependencyContext.LocalDependencyByInheritance -> localInheritance.external
      )
    )
    Relations.make(
      srcProd,
      libraryDep,
      libraryClassName,
      internal,
      external,
      classes,
      names,
      productClassName
    )
  }

  def fromApis(shouldStoreApis: Boolean)(apis: Schema.APIs): APIs = {
    val internal =
      apis.getInternalMap.asScala.iterator.map {
        case (k, v) => k -> fromAnalyzedClass(shouldStoreApis)(v)
      }.toMap
    val external =
      apis.getExternalMap.asScala.iterator.map {
        case (k, v) => k -> fromAnalyzedClass(shouldStoreApis)(v)
      }.toMap
    APIs(internal = internal, external = external)
  }

  def fromApisFile(apisFile: Schema.APIsFile, shouldStoreApis: Boolean): (APIs, Schema.Version) = {
    val apis =
      if (apisFile.hasApis) fromApis(shouldStoreApis)(apisFile.getApis)
      else ReadersFeedback.ExpectedApisInApisFile.!!
    val version = apisFile.getVersion
    apis -> version
  }

  def fromAnalysis(analysis: Schema.Analysis): Analysis = {
    def expected(clazz: Class[_]) = ReadersFeedback.expected(clazz, Classes.Analysis)
    val stamps =
      if (analysis.hasStamps) fromStamps(analysis.getStamps)
      else expected(Classes.Stamps).!!
    val relations =
      if (analysis.hasRelations) fromRelations(analysis.getRelations)
      else expected(Classes.Relations).!!
    val sourceInfos =
      if (analysis.hasSourceInfos) fromSourceInfos(analysis.getSourceInfos)
      else expected(Classes.SourceInfos).!!
    val compilations =
      if (analysis.hasCompilations) fromCompilations(analysis.getCompilations)
      else expected(Classes.Compilations).!!
    Analysis.Empty.copy(
      stamps = stamps,
      relations = relations,
      infos = sourceInfos,
      compilations = compilations
    )
  }

  def fromAnalysisFile(analysisFile: Schema.AnalysisFile): (Analysis, MiniSetup, Schema.Version) = {
    val version = analysisFile.getVersion
    val analysis =
      if (analysisFile.hasAnalysis) fromAnalysis(analysisFile.getAnalysis)
      else s"The analysis file from format ${version} could not be read.".!!
    val miniSetup =
      if (analysisFile.hasMiniSetup) fromMiniSetup(analysisFile.getMiniSetup)
      else s"The mini setup from format ${version} could not be read.".!!
    (analysis, miniSetup, version)
  }
}
