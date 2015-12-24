package sbt.internal.inc

import java.io.File
import java.net.URLClassLoader
import java.util.Properties
import java.util.concurrent.Callable

import sbt.internal.inc.classpath.ClasspathUtilities
import sbt.internal.librarymanagement.{ JsonUtil, ComponentManager, BaseIvySpecification }
import sbt.io.IO
import sbt.io.Path._
import sbt.librarymanagement.{ ModuleID, UpdateOptions, Resolver, Patterns, FileRepository }
import sbt.util.{ Logger, Level }
import xsbti.{ ComponentProvider, GlobalLock }

/**
 * Base class for test suites that must be able to fetch and compile the compiler bridge.
 */
abstract class BridgeProviderSpecification extends BaseIvySpecification {
  log.setLevel(Level.Warn)

  def realLocal: Resolver =
    {
      val pList = s"$${user.home}/.ivy2/local/${Resolver.localBasePattern}" :: Nil
      FileRepository("local", Resolver.defaultFileConfiguration, Patterns(pList, pList, false))
    }
  override def resolvers: Seq[Resolver] = Seq(realLocal, Resolver.jcenterRepo)
  private val ivyConfiguration = mkIvyConfiguration(UpdateOptions())

  def getCompilerBridge(targetDir: File, log: Logger, scalaVersion: String): File = {
    val instance = scalaInstance(scalaVersion)
    val bridgeId = compilerBridgeId(scalaVersion)
    val sourceModule = ModuleID(xsbti.ArtifactInfo.SbtOrganization, bridgeId, ComponentCompiler.incrementalVersion, Some("component"))

    val raw = new RawCompiler(instance, ClasspathOptions.auto, log)
    val manager = new ComponentManager(lock, provider(targetDir), None, log)
    val componentCompiler = new IvyComponentCompiler(raw, manager, ivyConfiguration, sourceModule, log)

    val bridge = componentCompiler.apply()
    val target = targetDir / s"target-bridge-$scalaVersion.jar"
    IO.copyFile(bridge, target)
    target
  }

  def scalaInstance(scalaVersion: String): ScalaInstance = {
    val scalaModule = {
      val dummyModule = ModuleID(JsonUtil.sbtOrgTemp, "tmp-scala-" + scalaVersion, scalaVersion, Some("compile"))
      val scalaLibrary = ModuleID(xsbti.ArtifactInfo.ScalaOrganization, xsbti.ArtifactInfo.ScalaLibraryID, scalaVersion, Some("compile"))
      val scalaCompiler = ModuleID(xsbti.ArtifactInfo.ScalaOrganization, xsbti.ArtifactInfo.ScalaCompilerID, scalaVersion, Some("compile"))

      module(dummyModule, Seq(scalaLibrary, scalaCompiler), None)
    }

    val allArtifacts =
      for {
        conf <- ivyUpdate(scalaModule).configurations
        m <- conf.modules
        (_, f) <- m.artifacts
      } yield f

    def isCompiler(f: File) = f.getName startsWith "scala-compiler-"
    def isLibrary(f: File) = f.getName startsWith "scala-library-"

    val scalaCompilerJar = allArtifacts find isCompiler getOrElse (throw new RuntimeException("Not found: scala-compiler"))
    val scalaLibraryJar = allArtifacts find isLibrary getOrElse (throw new RuntimeException("Not found: scala-library"))
    val others = allArtifacts filterNot (a => isCompiler(a) || isLibrary(a))

    scalaInstance(scalaCompilerJar, scalaLibraryJar, others)
  }

  def scalaInstance(scalaCompiler: File, scalaLibrary: File, scalaExtra: Seq[File]): ScalaInstance = {
    val loader = scalaLoader(scalaLibrary +: scalaCompiler +: scalaExtra)
    val version = scalaVersion(loader)
    val allJars = (scalaLibrary +: scalaCompiler +: scalaExtra).toArray
    new ScalaInstance(version.getOrElse("unknown"), loader, scalaLibrary, scalaCompiler, allJars, version)
  }

  def compilerBridgeId(scalaVersion: String) =
    scalaVersion match {
      case sc if sc startsWith "2.11" => "compiler-bridge_2.11"
      case _                          => "compiler-bridge_2.10"
    }

  def scalaLoader(jars: Seq[File]) = new URLClassLoader(sbt.io.Path.toURLs(jars), ClasspathUtilities.rootLoader)
  def scalaVersion(scalaLoader: ClassLoader): Option[String] =
    propertyFromResource("compiler.properties", "version.number", scalaLoader)

  /**
   * Get a property from a properties file resource in the classloader.
   */
  def propertyFromResource(resource: String, property: String, classLoader: ClassLoader): Option[String] = {
    val props = propertiesFromResource(resource, classLoader)
    Option(props.getProperty(property))
  }

  /**
   * Get all properties from a properties file resource in the classloader.
   */
  def propertiesFromResource(resource: String, classLoader: ClassLoader): Properties = {
    val props = new Properties
    val stream = classLoader.getResourceAsStream(resource)
    try { props.load(stream) }
    catch { case _: Exception => }
    finally { if (stream ne null) stream.close() }
    props
  }

  private val lock: GlobalLock = new GlobalLock {
    override def apply[T](file: File, callable: Callable[T]): T = callable.call()
  }

  private def provider(targetDir: File): ComponentProvider = new ComponentProvider {

    override def lockFile(): File = targetDir / "lock"

    override def defineComponent(componentID: String, files: Array[File]): Unit =
      files foreach { f => IO.copyFile(f, targetDir / componentID / f.getName) }

    override def addToComponent(componentID: String, files: Array[File]): Boolean = {
      defineComponent(componentID, files)
      true
    }

    override def component(componentID: String): Array[File] =
      IO.listFiles(targetDir / componentID)

    override def componentLocation(id: String): File = throw new UnsupportedOperationException
  }

}
