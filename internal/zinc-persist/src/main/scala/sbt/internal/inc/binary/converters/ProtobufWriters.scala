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
import java.io.File
import java.nio.file.Path

import sbt.internal.inc._
import xsbti.{ Position, Problem, Severity, T2, UseScope, VirtualFileRef }
import xsbti.compile.analysis.{ SourceInfo, Stamp, WriteMapper }
import sbt.internal.inc.binary.converters.ProtobufDefaults.Feedback.{ Writers => WritersFeedback }
import sbt.internal.inc.binary.converters.ProtobufDefaults.WritersConstants
import xsbti.api.{ Private, _ }
import xsbti.compile.{
  CompileOrder,
  FileHash,
  MiniOptions,
  MiniSetup,
  MultipleOutput,
  Output,
  OutputGroup,
  SingleOutput
}

final class ProtobufWriters(mapper: WriteMapper) {
  def toStringPath(file: File): String = {
    file.toPath.toString
  }
  def toStringPath(file: Path): String = {
    file.toString
  }
  def toStringPathV(file: VirtualFileRef): String = {
    file.id
  }

  def toStampType(stamp: Stamp): Schema.Stamps.StampType = {
    val s0 = Schema.Stamps.StampType.newBuilder
    stamp match {
      case hash: FarmHash =>
        val x = Schema.FarmHash.newBuilder
          .setHash(hash.hashValue)
          .build
        s0.setFarmHash(x)
      case hash: Hash =>
        val x = Schema.Hash.newBuilder
          .setHash(hash.hexHash)
          .build
        s0.setHash(x)
      case lm: LastModified =>
        val x = Schema.LastModified.newBuilder
          .setMillis(lm.value)
          .build
        s0.setLastModified(x)
      case _: Stamp => s0
    }
    s0.build
  }

  def toStamps(stamps: Stamps): Schema.Stamps = {
    // Note that boilerplate here is inteded, abstraction is expensive
    def toBinarySchemaMap(
        data: Map[VirtualFileRef, Stamp]
    ): Iterator[(String, Schema.Stamps.StampType)] = {
      data.iterator.map {
        case (binaryFile, stamp) =>
          val newBinaryFile = mapper.mapBinaryFile(binaryFile)
          val newPath = toStringPathV(newBinaryFile)
          val newBinaryStamp = mapper.mapBinaryStamp(binaryFile, stamp)
          val newStamp = toStampType(newBinaryStamp)
          newPath -> newStamp
      }
    }

    def toSourceSchemaMap(
        data: Map[VirtualFileRef, Stamp]
    ): Iterator[(String, Schema.Stamps.StampType)] = {
      data.iterator.map {
        case (sourceFile, stamp) =>
          val newSourceFile = mapper.mapSourceFile(sourceFile)
          val newPath = toStringPathV(newSourceFile)
          val newSourceStamp = mapper.mapSourceStamp(sourceFile, stamp)
          val newStamp = toStampType(newSourceStamp)
          newPath -> newStamp
      }
    }

    def toProductSchemaMap(
        data: Map[VirtualFileRef, Stamp]
    ): Iterator[(String, Schema.Stamps.StampType)] = {
      data.iterator.map {
        case (productFile, stamp) =>
          val newProductFile = mapper.mapProductFile(productFile)
          val newPath = toStringPathV(newProductFile)
          val newProductStamp = mapper.mapProductStamp(productFile, stamp)
          val newStamp = toStampType(newProductStamp)
          newPath -> newStamp
      }
    }

    val binaryStamps = toBinarySchemaMap(stamps.libraries)
    val sourceStamps = toSourceSchemaMap(stamps.sources)
    val productStamps = toProductSchemaMap(stamps.products)

    val builder = Schema.Stamps.newBuilder
    binaryStamps.foreach { case (k, v)  => builder.putBinaryStamps(k, v) }
    sourceStamps.foreach { case (k, v)  => builder.putSourceStamps(k, v) }
    productStamps.foreach { case (k, v) => builder.putProductStamps(k, v) }
    builder.build
  }

  def toOutputGroup(outputGroup: OutputGroup): Schema.OutputGroup = {
    val newSource = mapper.mapSourceDir(outputGroup.getSourceDirectoryAsPath)
    val newTarget = mapper.mapOutputDir(outputGroup.getOutputDirectoryAsPath)
    val sourcePath = toStringPath(newSource)
    val targetPath = toStringPath(newTarget)
    Schema.OutputGroup.newBuilder
      .setSourcePath(sourcePath)
      .setTargetPath(targetPath)
      .build
  }

  def setCompilationOutput(
      output: Output,
      builder: Schema.Compilation.Builder
  ): Schema.Compilation.Builder = {
    output match {
      case single0: SingleOutput =>
        val newOutputDir = mapper.mapOutputDir(single0.getOutputDirectoryAsPath)
        val targetPath = toStringPath(newOutputDir)
        val single = Schema.SingleOutput.newBuilder.setTarget(targetPath).build
        builder.setSingleOutput(single)
      case multiple0: MultipleOutput =>
        val multipleBuilder = Schema.MultipleOutput.newBuilder
        multiple0.getOutputGroups.foreach(g => multipleBuilder.addOutputGroups(toOutputGroup(g)))
        builder.setMultipleOutput(multipleBuilder.build)
      case _ => sys.error(WritersFeedback.UnexpectedEmptyOutput)
    }
  }

  def toCompilation(compilation: Compilation): Schema.Compilation = {
    val builder = Schema.Compilation.newBuilder
    val startTimeMillis = compilation.getStartTime
    setCompilationOutput(compilation.getOutput, builder)
      .setStartTimeMillis(startTimeMillis)
      .build
  }

  def toCompilations(compilations0: Compilations): Schema.Compilations = {
    val builder = Schema.Compilations.newBuilder
    compilations0.allCompilations.foreach(c => builder.addCompilations(toCompilation(c)))
    builder.build
  }

  import ProtobufDefaults.{ MissingString, MissingInt }
  import sbt.internal.inc.JavaInterfaceUtil._
  def toPosition(position: Position): Schema.Position =
    Schema.Position.newBuilder
      .setLine(position.line.toOption.fold(MissingInt)(_.toInt))
      .setOffset(position.offset.toOption.fold(MissingInt)(_.toInt))
      .setLineContent(position.lineContent)
      .setPointer(position.pointer.toOption.fold(MissingInt)(_.toInt))
      .setPointerSpace(position.pointerSpace.toOption.getOrElse(MissingString))
      .setSourcePath(position.sourcePath.toOption.getOrElse(MissingString))
      .setSourceFilepath(position.sourceFile.toOption.fold(MissingString)(toStringPath))
      .setStartOffset(position.startOffset.toOption.fold(MissingInt)(_.toInt))
      .setEndOffset(position.endOffset.toOption.fold(MissingInt)(_.toInt))
      .setStartLine(position.startLine.toOption.fold(MissingInt)(_.toInt))
      .setStartColumn(position.startColumn.toOption.fold(MissingInt)(_.toInt))
      .setEndLine(position.endLine.toOption.fold(MissingInt)(_.toInt))
      .setEndColumn(position.endColumn.toOption.fold(MissingInt)(_.toInt))
      .build

  def toSeverity(severity: Severity): Schema.Severity =
    severity match {
      case Severity.Info  => Schema.Severity.INFO
      case Severity.Warn  => Schema.Severity.WARN
      case Severity.Error => Schema.Severity.ERROR
    }

  def toProblem(problem: Problem): Schema.Problem = {
    val category = problem.category()
    val message = problem.message()
    val position = toPosition(problem.position())
    val severity = toSeverity(problem.severity())
    Schema.Problem.newBuilder
      .setCategory(category)
      .setMessage(message)
      .setPosition(position)
      .setSeverity(severity)
      .build
  }

  def toSourceInfo(sourceInfo: SourceInfo): Schema.SourceInfo = {
    val builder = Schema.SourceInfo.newBuilder
    sourceInfo.getMainClasses.foreach(c => builder.addMainClasses(c))
    sourceInfo.getReportedProblems.foreach(p => builder.addReportedProblems(toProblem(p)))
    sourceInfo.getUnreportedProblems.foreach(p => builder.addUnreportedProblems(toProblem(p)))
    builder.build
  }

  def toSourceInfos(sourceInfos0: SourceInfos): Schema.SourceInfos = {
    val sourceInfos = sourceInfos0.allInfos.iterator.map {
      case (file, sourceInfo0) =>
        toStringPathV(mapper.mapSourceFile(file)) -> toSourceInfo(sourceInfo0)
    }
    val builder = Schema.SourceInfos.newBuilder
    sourceInfos.foreach { case (k, v) => builder.putSourceInfos(k, v) }
    builder.build
  }

  def toClasspathFileHash(fileHash: FileHash): Schema.FileHash = {
    val newClasspathEntry = mapper.mapClasspathEntry(fileHash.file())
    val path = toStringPath(newClasspathEntry)
    val hash = fileHash.hash()
    Schema.FileHash.newBuilder
      .setPath(path)
      .setHash(hash)
      .build
  }

  def toMiniOptions(miniOptions: MiniOptions): Schema.MiniOptions = {
    val builder = Schema.MiniOptions.newBuilder
    miniOptions.classpathHash.foreach(h => builder.addClasspathHash(toClasspathFileHash(h)))
    miniOptions.javacOptions().foreach(o => builder.addJavacOptions(mapper.mapJavacOption(o)))
    miniOptions.scalacOptions().foreach(o => builder.addScalacOptions(mapper.mapScalacOption(o)))
    builder.build
  }

  @silent
  def toCompileOrder(compileOrder: CompileOrder): Schema.CompileOrder = {
    compileOrder match {
      case CompileOrder.Mixed         => Schema.CompileOrder.MIXED
      case CompileOrder.JavaThenScala => Schema.CompileOrder.JAVATHENSCALA
      case CompileOrder.ScalaThenJava => Schema.CompileOrder.SCALATHENJAVA
    }
  }

  def toStringTuple(tuple: T2[String, String]): Schema.Tuple = {
    Schema.Tuple.newBuilder
      .setFirst(tuple.get1())
      .setSecond(tuple.get2())
      .build
  }

  def setMiniSetupOutput(
      output: Output,
      builder: Schema.MiniSetup.Builder
  ): Schema.MiniSetup.Builder =
    output match {
      case single0: SingleOutput =>
        val newOutputDir = mapper.mapOutputDir(single0.getOutputDirectoryAsPath)
        val targetPath = toStringPath(newOutputDir)
        val single = Schema.SingleOutput.newBuilder.setTarget(targetPath).build
        builder.setSingleOutput(single)
      case multiple0: MultipleOutput =>
        val multipleBuilder = Schema.MultipleOutput.newBuilder
        multiple0.getOutputGroups.foreach(g => multipleBuilder.addOutputGroups(toOutputGroup(g)))
        builder.setMultipleOutput(multipleBuilder.build)
      case CompileOutput.empty =>
        val dummy =
          Schema.SingleOutput.newBuilder.setTarget(toStringPath(Analysis.dummyOutputPath)).build
        builder.setSingleOutput(dummy)
    }

  def toMiniSetup(miniSetup0: MiniSetup): Schema.MiniSetup = {
    val builder = Schema.MiniSetup.newBuilder
    val miniSetup = mapper.mapMiniSetup(miniSetup0)
    val miniOptions = toMiniOptions(miniSetup.options())
    val compilerVersion = miniSetup.compilerVersion()
    val compileOrder = toCompileOrder(miniSetup.order())
    val storeApis = miniSetup.storeApis()
    val miniBuilder = setMiniSetupOutput(CompileOutput.empty, builder)
      .setMiniOptions(miniOptions)
      .setCompilerVersion(compilerVersion)
      .setCompileOrder(compileOrder)
      .setStoreApis(storeApis)
    miniSetup.extra().foreach(e => builder.addExtra(toStringTuple(e)))
    miniBuilder.build
  }

  def toPath(path: xsbti.api.Path): Schema.Path = {
    def toPathComponent(pathComponent: PathComponent): Schema.Path.PathComponent = {
      val builder0 = Schema.Path.PathComponent.newBuilder
      val builder = pathComponent match {
        case c: Id =>
          builder0.setId(Schema.Id.newBuilder.setId(c.id).build)
        case c: Super =>
          builder0.setSuper(Schema.Super.newBuilder.setQualifier(toPath(c.qualifier)))
        case _: This =>
          builder0.setThis(WritersConstants.This)
      }
      builder.build
    }
    val builder = Schema.Path.newBuilder
    path.components().foreach(c => builder.addComponents(toPathComponent(c)))
    builder.build
  }

  def toAnnotation(annotation: Annotation): Schema.Annotation = {
    def toAnnotationArgument(annotationArgument: AnnotationArgument): Schema.AnnotationArgument =
      Schema.AnnotationArgument.newBuilder
        .setName(annotationArgument.name())
        .setValue(annotationArgument.value())
        .build

    val base = toType(annotation.base())
    val builder = Schema.Annotation.newBuilder.setBase(base)
    annotation.arguments().foreach(a => builder.addArguments(toAnnotationArgument(a)))
    builder.build
  }

  def toStructure(tpe: Structure): Schema.Type.Structure = {
    val builder = Schema.Type.Structure.newBuilder
    tpe.declared().foreach(t => builder.addDeclared(toClassDefinition(t)))
    tpe.inherited().foreach(i => builder.addInherited(toClassDefinition(i)))
    tpe.parents().foreach(p => builder.addParents(toType(p)))
    builder.build
  }

  def toType(`type`: Type): Schema.Type = {
    def toExistential(tpe: Existential): Schema.Type.Existential = {
      val baseType = toType(tpe.baseType())
      val builder = Schema.Type.Existential.newBuilder.setBaseType(baseType)
      tpe.clause().foreach(c => builder.addClause(toTypeParameter(c)))
      builder.build
    }

    def toProjection(tpe: Projection): Schema.Type.Projection = {
      val id = tpe.id()
      val prefix = toType(tpe.prefix())
      Schema.Type.Projection.newBuilder
        .setId(id)
        .setPrefix(prefix)
        .build
    }

    def toPolymorphic(tpe: Polymorphic): Schema.Type.Polymorphic = {
      val baseType = toType(tpe.baseType())
      val builder = Schema.Type.Polymorphic.newBuilder.setBaseType(baseType)
      tpe.parameters().foreach(p => builder.addTypeParameters(toTypeParameter(p)))
      builder.build
    }

    def toParameterRef(tpe: ParameterRef): Schema.Type.ParameterRef =
      Schema.Type.ParameterRef.newBuilder
        .setId(tpe.id())
        .build

    def toParameterized(tpe: Parameterized): Schema.Type.Parameterized = {
      val baseType = toType(tpe.baseType())
      val builder = Schema.Type.Parameterized.newBuilder.setBaseType(baseType)
      tpe.typeArguments().foreach(a => builder.addTypeArguments(toType(a)))
      builder.build
    }

    def toSingleton(tpe: Singleton): Schema.Type.Singleton =
      Schema.Type.Singleton.newBuilder
        .setPath(toPath(tpe.path()))
        .build

    def toConstant(tpe: Constant): Schema.Type.Constant = {
      val baseType = toType(tpe.baseType())
      val value = tpe.value()
      Schema.Type.Constant.newBuilder
        .setBaseType(baseType)
        .setValue(value)
        .build
    }

    def toAnnotated(tpe: Annotated): Schema.Type.Annotated = {
      val baseType = toType(tpe.baseType())
      val builder = Schema.Type.Annotated.newBuilder.setBaseType(baseType)
      tpe.annotations().foreach(a => builder.addAnnotations(toAnnotation(a)))
      builder.build
    }

    val builder = Schema.Type.newBuilder
    `type` match {
      case tpe: ParameterRef  => builder.setParameterRef(toParameterRef(tpe))
      case tpe: Parameterized => builder.setParameterized(toParameterized(tpe))
      case tpe: Structure     => builder.setStructure(toStructure(tpe))
      case tpe: Polymorphic   => builder.setPolymorphic(toPolymorphic(tpe))
      case tpe: Constant      => builder.setConstant(toConstant(tpe))
      case tpe: Existential   => builder.setExistential(toExistential(tpe))
      case tpe: Singleton     => builder.setSingleton(toSingleton(tpe))
      case tpe: Projection    => builder.setProjection(toProjection(tpe))
      case tpe: Annotated     => builder.setAnnotated(toAnnotated(tpe))
      case _: EmptyType       => builder.setEmptyType(WritersConstants.EmptyType)
    }
    builder.build
  }

  def toDefinitionType(definitionType: DefinitionType): Schema.DefinitionType = {
    definitionType match {
      case DefinitionType.ClassDef      => Schema.DefinitionType.CLASSDEF
      case DefinitionType.Module        => Schema.DefinitionType.MODULE
      case DefinitionType.Trait         => Schema.DefinitionType.TRAIT
      case DefinitionType.PackageModule => Schema.DefinitionType.PACKAGEMODULE
    }
  }

  def toAccess(access: Access): Schema.Access = {
    def toQualifier(qualifier: Qualifier): Schema.Qualifier = {
      import WritersConstants.{ ThisQualifier, Unqualified }
      val builder = Schema.Qualifier.newBuilder
      qualifier match {
        case q: IdQualifier =>
          builder.setIdQualifier(Schema.IdQualifier.newBuilder.setValue(q.value()).build)
        case _: ThisQualifier => builder.setThisQualifier(ThisQualifier)
        case _: Unqualified   => builder.setUnqualified(Unqualified)
      }
      builder.build
    }
    import WritersConstants.PublicAccess
    val builder = Schema.Access.newBuilder
    access match {
      case _: Public => builder.setPublic(PublicAccess)
      case qualified: Qualified =>
        val qualifier = toQualifier(qualified.qualifier())
        qualified match {
          case _: Private =>
            builder.setPrivate(Schema.Private.newBuilder.setQualifier(qualifier).build)
          case _: Protected =>
            builder.setProtected(Schema.Protected.newBuilder.setQualifier(qualifier).build)
        }
    }
    builder.build
  }

  def toModifiers(modifiers: Modifiers): Schema.Modifiers =
    Schema.Modifiers.newBuilder
      .setFlags(modifiers.raw().toInt)
      .build

  def toClassDefinition(classDefinition: ClassDefinition): Schema.ClassDefinition = {

    def toParameterList(parameterList: ParameterList): Schema.ParameterList = {
      def toMethodParameter(methodParameter: MethodParameter): Schema.MethodParameter = {
        def toParameterModifier(modifier: ParameterModifier): Schema.ParameterModifier = {
          modifier match {
            case ParameterModifier.Plain    => Schema.ParameterModifier.PLAIN
            case ParameterModifier.ByName   => Schema.ParameterModifier.BYNAME
            case ParameterModifier.Repeated => Schema.ParameterModifier.REPEATED
          }
        }
        val name = methodParameter.name()
        val hasDefault = methodParameter.hasDefault()
        val `type` = toType(methodParameter.tpe())
        val modifier = toParameterModifier(methodParameter.modifier())
        Schema.MethodParameter.newBuilder
          .setName(name)
          .setType(`type`)
          .setHasDefault(hasDefault)
          .setModifier(modifier)
          .build
      }

      val isImplicit = parameterList.isImplicit()
      val builder = Schema.ParameterList.newBuilder.setIsImplicit(isImplicit)
      parameterList.parameters().foreach(p => builder.addParameters(toMethodParameter(p)))
      builder.build
    }

    def toClassLikeDef(classLikeDef: ClassLikeDef): Schema.ClassDefinition.ClassLikeDef = {
      val definitionType = toDefinitionType(classLikeDef.definitionType())
      val builder = Schema.ClassDefinition.ClassLikeDef.newBuilder.setDefinitionType(definitionType)
      classLikeDef.typeParameters().foreach(p => builder.addTypeParameters(toTypeParameter(p)))
      builder.build
    }

    def toValDef(valDef: Val): Schema.ClassDefinition.Val =
      Schema.ClassDefinition.Val.newBuilder
        .setType(toType(valDef.tpe))
        .build

    def toVarDef(varDef: Var): Schema.ClassDefinition.Var =
      Schema.ClassDefinition.Var.newBuilder
        .setType(toType(varDef.tpe))
        .build

    def toDefDef(defDef: Def): Schema.ClassDefinition.Def = {
      val returnType = toType(defDef.returnType)
      val builder = Schema.ClassDefinition.Def.newBuilder.setReturnType(returnType)
      defDef.typeParameters().foreach(p => builder.addTypeParameters(toTypeParameter(p)))
      defDef.valueParameters().foreach(p => builder.addValueParameters(toParameterList(p)))
      builder.build
    }

    def toTypeAlias(typeAlias: TypeAlias): Schema.ClassDefinition.TypeAlias = {
      val `type` = toType(typeAlias.tpe())
      val builder = Schema.ClassDefinition.TypeAlias.newBuilder.setType(`type`)
      typeAlias.typeParameters().foreach(p => builder.addTypeParameters(toTypeParameter(p)))
      builder.build
    }

    def toTypeDeclaration(
        typeDeclaration: TypeDeclaration
    ): Schema.ClassDefinition.TypeDeclaration = {
      val lowerBound = toType(typeDeclaration.lowerBound())
      val upperBound = toType(typeDeclaration.upperBound())
      val builder = Schema.ClassDefinition.TypeDeclaration.newBuilder
        .setLowerBound(lowerBound)
        .setUpperBound(upperBound)
      typeDeclaration.typeParameters().foreach(p => builder.addTypeParameters(toTypeParameter(p)))
      builder.build
    }

    val builder = Schema.ClassDefinition.newBuilder
    val name = classDefinition.name()
    val access = toAccess(classDefinition.access())
    val modifiers = toModifiers(classDefinition.modifiers())
    classDefinition match {
      case classLikeDef: ClassLikeDef => builder.setClassLikeDef(toClassLikeDef(classLikeDef))
      case valDef: Val                => builder.setValDef(toValDef(valDef))
      case varDef: Var                => builder.setVarDef(toVarDef(varDef))
      case defDef: Def                => builder.setDefDef(toDefDef(defDef))
      case typeAlias: TypeAlias       => builder.setTypeAlias(toTypeAlias(typeAlias))
      case typeDeclaration: TypeDeclaration =>
        builder.setTypeDeclaration(toTypeDeclaration(typeDeclaration))
    }
    val resultBuilder = builder
      .setName(name)
      .setAccess(access)
      .setModifiers(modifiers)
    classDefinition.annotations().foreach(a => builder.addAnnotations(toAnnotation(a)))
    resultBuilder.build
  }

  def toTypeParameter(typeParameter: TypeParameter): Schema.TypeParameter = {
    def toVariance(variance: Variance): Schema.Variance = {
      variance match {
        case Variance.Invariant     => Schema.Variance.INVARIANT
        case Variance.Covariant     => Schema.Variance.COVARIANT
        case Variance.Contravariant => Schema.Variance.CONTRAVARIANT
      }
    }

    val id = typeParameter.id()
    val variance = toVariance(typeParameter.variance())
    val lowerBound = toType(typeParameter.lowerBound())
    val upperBound = toType(typeParameter.upperBound())
    val builder = Schema.TypeParameter.newBuilder
      .setId(id)
      .setVariance(variance)
      .setLowerBound(lowerBound)
      .setUpperBound(upperBound)
    typeParameter.annotations().foreach(a => builder.addAnnotations(toAnnotation(a)))
    typeParameter.typeParameters().foreach(p => builder.addTypeParameters(toTypeParameter(p)))
    builder.build
  }

  def toClassLike(classLike: ClassLike): Schema.ClassLike = {
    val name = classLike.name()
    val access = toAccess(classLike.access())
    val modifiers = toModifiers(classLike.modifiers())

    val definitionType = toDefinitionType(classLike.definitionType())
    val selfType = toType(classLike.selfType())
    val structure = toStructure(classLike.structure())
    val savedAnnotations = classLike.savedAnnotations()
    val topLevel = classLike.topLevel()
    val builder = Schema.ClassLike.newBuilder
      .setName(name)
      .setAccess(access)
      .setModifiers(modifiers)
      .setDefinitionType(definitionType)
      .setSelfType(selfType)
      .setStructure(structure)
      .setTopLevel(topLevel)
    classLike.annotations().foreach(a => builder.addAnnotations(toAnnotation(a)))
    savedAnnotations.foreach(a => builder.addSavedAnnotations(a))
    classLike.childrenOfSealedClass().foreach(c => builder.addChildrenOfSealedClass(toType(c)))
    classLike.typeParameters().foreach(p => builder.addTypeParameters(toTypeParameter(p)))
    builder.build
  }

  def toUseScope(useScope: UseScope): Schema.UseScope = {
    useScope match {
      case UseScope.Default      => Schema.UseScope.DEFAULT
      case UseScope.Implicit     => Schema.UseScope.IMPLICIT
      case UseScope.PatMatTarget => Schema.UseScope.PATMAT
    }
  }

  def toAnalyzedClass(
      shouldStoreApis: Boolean
  )(analyzedClass: AnalyzedClass): Schema.AnalyzedClass = {
    def toCompanions(companions: Companions): Schema.Companions = {
      val classApi = toClassLike(companions.classApi())
      val objectApi = toClassLike(companions.objectApi())
      Schema.Companions.newBuilder
        .setClassApi(classApi)
        .setObjectApi(objectApi)
        .build
    }

    def toNameHash(nameHash: NameHash): Schema.NameHash = {
      val name = nameHash.name()
      val hash = nameHash.hash()
      val scope = toUseScope(nameHash.scope())
      Schema.NameHash.newBuilder
        .setName(name)
        .setScope(scope)
        .setHash(hash)
        .build
    }

    val apiHash = analyzedClass.apiHash()
    val extraHash = analyzedClass.extraHash()
    val compilationTimestamp = analyzedClass.compilationTimestamp()
    val hasMacro = analyzedClass.hasMacro
    val name = analyzedClass.name()
    val provenance = analyzedClass.provenance

    val builder1 = Schema.AnalyzedClass.newBuilder
    val builder0 =
      if (shouldStoreApis) builder1.setApi(toCompanions(analyzedClass.api()))
      else builder1
    val builder = builder0
      .setCompilationTimestamp(compilationTimestamp)
      .setName(name)
      .setApiHash(apiHash)
      .setHasMacro(hasMacro)
      .setExtraHash(extraHash)
      .setProvenance(provenance)

    analyzedClass.nameHashes().foreach(h => builder.addNameHashes(toNameHash(h)))
    builder.build
  }

  private final val sourceToString = (f: VirtualFileRef) => toStringPathV(mapper.mapSourceFile(f))
  private final val libraryToString = (f: VirtualFileRef) => toStringPathV(mapper.mapBinaryFile(f))
  private final val prodToString = (f: VirtualFileRef) => toStringPathV(mapper.mapProductFile(f))

  private final val stringId = identity[String] _
  def toRelations(relations: Relations): Schema.Relations = {
    import sbt.internal.util.Relation

    def toUsedName(usedName: UsedName): Schema.UsedName = {
      val name = usedName.name
      val builder = Schema.UsedName.newBuilder
        .setName(name)
      val it = usedName.scopes.iterator
      while (it.hasNext) builder.addScopes(toUseScope(it.next))
      builder.build
    }

    def toUsedNamesMap(
        relation: Relation[String, UsedName]
    ): Iterator[(String, Schema.UsedNames)] = {
      relation.forwardMap.iterator.map {
        case (k, names) =>
          val builder = Schema.UsedNames.newBuilder
          names.foreach(name => builder.addUsedNames(toUsedName(name)))
          k -> builder.build
      }
    }

    def toMap[K, V](
        relation: Relation[K, V],
        fk: K => String,
        fv: V => String
    ): Iterator[(String, Schema.Values)] = {
      relation.forwardMap.iterator.map {
        case (k, vs) =>
          val builder = Schema.Values.newBuilder
          vs.foreach(v => builder.addValues(fv(v)))
          fk(k) -> builder.build
      }
    }

    val srcProd = toMap(relations.srcProd, sourceToString, prodToString)
    val libraryDep = toMap(relations.libraryDep, sourceToString, libraryToString)
    val libraryClassName = toMap(relations.libraryClassName, libraryToString, stringId)
    val memberRefInternal = toMap(relations.memberRef.internal, stringId, stringId)
    val memberRefExternal = toMap(relations.memberRef.external, stringId, stringId)
    val inheritanceInternal = toMap(relations.inheritance.internal, stringId, stringId)
    val inheritanceExternal = toMap(relations.inheritance.external, stringId, stringId)
    val localInheritanceInternal = toMap(relations.localInheritance.internal, stringId, stringId)
    val localInheritanceExternal = toMap(relations.localInheritance.external, stringId, stringId)
    val classes = toMap(relations.classes, sourceToString, stringId)
    val productClassName = toMap(relations.productClassName, stringId, stringId)
    val names = toUsedNamesMap(relations.names)
    val memberRef = {
      val builder = Schema.ClassDependencies.newBuilder
      memberRefInternal.foreach { case (k, v) => builder.putInternal(k, v) }
      memberRefExternal.foreach { case (k, v) => builder.putExternal(k, v) }
      builder.build
    }

    val inheritance = {
      val builder = Schema.ClassDependencies.newBuilder
      inheritanceInternal.foreach { case (k, v) => builder.putInternal(k, v) }
      inheritanceExternal.foreach { case (k, v) => builder.putExternal(k, v) }
      builder.build
    }

    val localInheritance = {
      val builder = Schema.ClassDependencies.newBuilder
      localInheritanceInternal.foreach { case (k, v) => builder.putInternal(k, v) }
      localInheritanceExternal.foreach { case (k, v) => builder.putExternal(k, v) }
      builder.build
    }

    val builder = Schema.Relations.newBuilder
    srcProd.foreach { case (k, v)          => builder.putSrcProd(k, v) }
    libraryDep.foreach { case (k, v)       => builder.putLibraryDep(k, v) }
    libraryClassName.foreach { case (k, v) => builder.putLibraryClassName(k, v) }
    classes.foreach { case (k, v)          => builder.putClasses(k, v) }
    productClassName.foreach { case (k, v) => builder.putProductClassName(k, v) }
    names.foreach { case (k, v)            => builder.putNames(k, v) }
    builder
      .setMemberRef(memberRef)
      .setInheritance(inheritance)
      .setLocalInheritance(localInheritance)
      .build
  }

  def toApis(apis: APIs, shouldStoreApis: Boolean): Schema.APIs = {
    val toAnalyzedClassSchema = toAnalyzedClass(shouldStoreApis) _
    val builder = Schema.APIs.newBuilder
    apis.internal.foreach { case (k, v) => builder.putInternal(k, toAnalyzedClassSchema(v)) }
    apis.external.foreach { case (k, v) => builder.putExternal(k, toAnalyzedClassSchema(v)) }
    builder.build
  }

  def toApisFile(
      apis0: APIs,
      version: Schema.Version,
      shouldStoreApis: Boolean
  ): Schema.APIsFile =
    Schema.APIsFile.newBuilder
      .setVersion(version)
      .setApis(toApis(apis0, shouldStoreApis))
      .build

  def toAnalysis(analysis: Analysis): Schema.Analysis =
    Schema.Analysis.newBuilder
      .setStamps(toStamps(analysis.stamps))
      .setRelations(toRelations(analysis.relations))
      .setSourceInfos(toSourceInfos(analysis.infos))
      .setCompilations(toCompilations(analysis.compilations))
      .build

  def toAnalysisFile(
      analysis0: Analysis,
      miniSetup0: MiniSetup,
      version: Schema.Version
  ): Schema.AnalysisFile =
    Schema.AnalysisFile.newBuilder
      .setVersion(version)
      .setAnalysis(toAnalysis(analysis0))
      .setMiniSetup(toMiniSetup(miniSetup0))
      .build
}
