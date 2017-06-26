// import Project.Initialize
import Util._
import Dependencies._
import Scripted._
// import StringUtilities.normalize
import com.typesafe.tools.mima.core._, ProblemFilters._

def baseVersion = "1.0.0-X16-SNAPSHOT"
def internalPath = file("internal")

lazy val compilerBridgeScalaVersions = Seq(scala212, scala211, scala210)

val scalafmtCheck = Command.command("scalafmtCheck") { state =>
  sys.process.Process("git diff --name-only --exit-code").! match {
    case 0 => // ok
    case x => sys.error("git diff detected! Did you compile before committing?")
  }
  state
}

def commonSettings: Seq[Setting[_]] = Seq(
  scalaVersion := scala212,
  // publishArtifact in packageDoc := false,
  resolvers += Resolver.typesafeIvyRepo("releases"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += "bintray-sbt-maven-releases" at "https://dl.bintray.com/sbt/maven-releases/",
  resolvers += Resolver.url(
    "bintray-sbt-ivy-snapshots",
    new URL("https://dl.bintray.com/sbt/ivy-snapshots/"))(Resolver.ivyStylePatterns),
  // concurrentRestrictions in Global += Util.testExclusiveRestriction,
  testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-w", "1"),
  javacOptions in compile ++= Seq("-Xlint", "-Xlint:-serial"),
  incOptions := incOptions.value.withNameHashing(true),
  crossScalaVersions := Seq(scala211, scala212),
  mimaPreviousArtifacts := Set(), // Some(organization.value %% moduleName.value % "1.0.0"),
  publishArtifact in Test := false,
  commands ++= Seq(publishBridgesAndTest, publishBridgesAndSet, crossTestBridges, scalafmtCheck),
  scalacOptions += "-YdisableFlatCpCaching"
)

def relaxNon212: Seq[Setting[_]] = Seq(
  scalacOptions := {
    val old = scalacOptions.value
    scalaBinaryVersion.value match {
      case "2.12" => old
      case _ =>
        old filterNot Set(
          "-Xfatal-warnings",
          "-deprecation",
          "-Ywarn-unused",
          "-Ywarn-unused-import",
          "-YdisableFlatCpCaching"
        )
    }
  }
)

def minimalSettings: Seq[Setting[_]] = commonSettings

// def minimalSettings: Seq[Setting[_]] =
//   commonSettings ++ customCommands ++
//   publishPomSettings ++ Release.javaVersionCheckSettings

def baseSettings: Seq[Setting[_]] =
  minimalSettings
//   minimalSettings ++ baseScalacOptions ++ Licensed.settings ++ Formatting.settings

def addBaseSettingsAndTestDeps(p: Project): Project =
  p.settings(baseSettings).configure(addTestDependencies)

val altLocalRepoName = "alternative-local"
val altLocalRepoPath = sys.props("user.home") + "/.ivy2/sbt-alternative"
lazy val altLocalResolver = Resolver.file(
  altLocalRepoName,
  file(sys.props("user.home") + "/.ivy2/sbt-alternative"))(Resolver.ivyStylePatterns)
lazy val altLocalPublish =
  TaskKey[Unit]("alt-local-publish", "Publishes an artifact locally to an alternative location.")
def altPublishSettings: Seq[Setting[_]] =
  Seq(
    resolvers += altLocalResolver,
    altLocalPublish := {
      val config = (Keys.publishLocalConfiguration).value
      val moduleSettings = (Keys.moduleSettings).value
      val ivy = new IvySbt((ivyConfiguration.value))

      val module =
        new ivy.Module(moduleSettings)
      val newConfig =
        new PublishConfiguration(
          config.ivyFile,
          altLocalRepoName,
          config.artifacts,
          config.checksums,
          config.logging
        )
      streams.value.log.info("Publishing " + module + " to local repo: " + altLocalRepoName)
      IvyActions.publish(module, newConfig, streams.value.log)
    }
  )

lazy val zincRoot: Project = (project in file("."))
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .
  // configs(Sxr.sxrConf).
  aggregate(
    zinc,
    zincTesting,
    zincPersist,
    zincCore,
    zincIvyIntegration,
    zincCompile,
    zincCompileCore,
    compilerInterface,
    compilerBridge,
    zincBenchmarks,
    zincApiInfo,
    zincClasspath,
    zincClassfile,
    zincScripted
  )
  .settings(
    inThisBuild(
      Seq(
        git.baseVersion := baseVersion,
        // https://github.com/sbt/sbt-git/issues/109
        // Workaround from https://github.com/sbt/sbt-git/issues/92#issuecomment-161853239
        git.gitUncommittedChanges := {
          val statusCommands = Seq(
            Seq("diff-index", "--cached", "HEAD"),
            Seq("diff-index", "HEAD"),
            Seq("diff-files"),
            Seq("ls-files", "--exclude-standard", "--others")
          )
          // can't use git.runner.value because it's a task
          val runner = com.typesafe.sbt.git.ConsoleGitRunner
          val dir = baseDirectory.value
          val uncommittedChanges = statusCommands.map { c =>
            runner(c: _*)(dir, com.typesafe.sbt.git.NullLogger)
          }

          uncommittedChanges.exists(_.nonEmpty)
        },
        version := {
          val v = version.value
          if (v contains "SNAPSHOT") git.baseVersion.value
          else v
        },
        bintrayPackage := "zinc",
        scmInfo := Some(
          ScmInfo(url("https://github.com/sbt/zinc"), "git@github.com:sbt/zinc.git")),
        description := "Incremental compiler of Scala",
        homepage := Some(url("https://github.com/sbt/zinc"))
      )),
    minimalSettings,
    otherRootSettings,
    name := "zinc Root",
    publish := {},
    publishLocal := {},
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
    publishArtifact := false,
    customCommands
  )

lazy val zinc = (project in file("zinc"))
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .dependsOn(
    zincCore,
    zincPersist,
    zincCompileCore,
    zincClassfile,
    zincIvyIntegration % "compile->compile;test->test",
    zincTesting % Test
  )
  .configure(addBaseSettingsAndTestDeps)
  .settings(
    name := "zinc"
  )

lazy val zincTesting = (project in internalPath / "zinc-testing")
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .settings(
    minimalSettings,
    name := "zinc Testing",
    publish := {},
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
    publishArtifact := false,
    libraryDependencies ++= Seq(scalaCheck, scalatest, junit, sjsonnewScalaJson)
  )
  .configure(addSbtLm)

lazy val zincCompile = (project in file("zinc-compile"))
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .dependsOn(zincCompileCore, zincCompileCore % "test->test")
  .configure(addBaseSettingsAndTestDeps)
  .settings(
    name := "zinc Compile"
  )
  .configure(addSbtUtilTracking)

// Persists the incremental data structures using SBinary
lazy val zincPersist = (project in internalPath / "zinc-persist")
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .dependsOn(zincCore, zincCore % "test->test")
  .configure(addBaseSettingsAndTestDeps)
  .settings(
    name := "zinc Persist",
    libraryDependencies += sbinary
  )

// Implements the core functionality of detecting and propagating changes incrementally.
//   Defines the data structures for representing file fingerprints and relationships and the overall source analysis
lazy val zincCore = (project in internalPath / "zinc-core")
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .dependsOn(zincApiInfo, zincClasspath, compilerInterface, compilerBridge % Test)
  .configure(addBaseSettingsAndTestDeps)
  .settings(
    // we need to fork because in unit tests we set usejavacp = true which means
    // we are expecting all of our dependencies to be on classpath so Scala compiler
    // can use them while constructing its own classpath for compilation
    fork in Test := true,
    // needed because we fork tests and tests are ran in parallel so we have multiple Scala
    // compiler instances that are memory hungry
    javaOptions in Test += "-Xmx1G",
    name := "zinc Core",
    compileOrder := sbt.CompileOrder.Mixed
  )
  .configure(addSbtIO, addSbtUtilLogging, addSbtUtilRelation)

lazy val zincBenchmarks = (project in internalPath / "zinc-benchmarks")
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .dependsOn(compilerInterface % "compile->compile;compile->test")
  .dependsOn(compilerBridge, zincCore, zincTesting % Test)
  .enablePlugins(JmhPlugin)
  .settings(
    name := "Benchmarks of Zinc and the compiler bridge",
    libraryDependencies ++= Seq(
      "org.eclipse.jgit" % "org.eclipse.jgit" % "4.6.0.201612231935-r",
      "net.openhft" % "affinity" % "3.0.6"
    ),
    scalaVersion := scala212,
    crossScalaVersions := Seq(scala211, scala212),
    javaOptions in Test += "-Xmx600M -Xms600M",
    publish := {},
    publishLocal := {}
  )

lazy val zincIvyIntegration = (project in internalPath / "zinc-ivy-integration")
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .dependsOn(zincCompileCore, zincTesting % Test)
  .settings(
    baseSettings,
    name := "zinc Ivy Integration",
    compileOrder := sbt.CompileOrder.ScalaThenJava
  )
  .configure(addSbtLm)

// sbt-side interface to compiler.  Calls compiler-side interface reflectively
lazy val zincCompileCore = (project in internalPath / "zinc-compile-core")
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .dependsOn(
    compilerInterface % "compile;test->test",
    zincClasspath,
    zincApiInfo,
    zincClassfile,
    zincTesting % Test
  )
  .configure(addBaseSettingsAndTestDeps)
  .settings(
    name := "zinc Compile Core",
    libraryDependencies ++= Seq(scalaCompiler.value % Test, launcherInterface, parserCombinator),
    unmanagedJars in Test := Seq(packageSrc in compilerBridge in Compile value).classpath
  )
  .configure(addSbtUtilLogging, addSbtIO, addSbtUtilControl)

// defines Java structures used across Scala versions, such as the API structures and relationships extracted by
//   the analysis compiler phases and passed back to sbt.  The API structures are defined in a simple
//   format from which Java sources are generated by the sbt-contraband plugin.
lazy val compilerInterface = (project in internalPath / "compiler-interface")
  .enablePlugins(ContrabandPlugin)
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .settings(
    minimalSettings,
    // javaOnlySettings,
    name := "Compiler Interface",
    crossScalaVersions := Seq(scala212),
    relaxNon212,
    libraryDependencies ++= Seq(scalaLibrary.value % Test),
    exportJars := true,
    watchSources ++= apiDefinitions.value,
    resourceGenerators in Compile += Def
      .task(
        generateVersionFile(
          version.value,
          resourceManaged.value,
          streams.value,
          compile in Compile value)
        )
      .taskValue,
    apiDefinitions := List(
      (baseDirectory.value / "definition"),
      (baseDirectory.value / "other"),
      (baseDirectory.value / "type")
    ),
    crossPaths := false,
    autoScalaLibrary := false,
    altPublishSettings
  )
  .configure(addSbtUtilInterface)

// Compiler-side interface to compiler that is compiled against the compiler being used either in advance or on the fly.
//   Includes API and Analyzer phases that extract source API and relationships.
lazy val compilerBridge: Project = (project in internalPath / "compiler-bridge")
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .dependsOn(
    compilerInterface % "compile;test->test",
//  launchProj % "test->test",
    zincApiInfo % "test->test"
  )
  .settings(
    baseSettings,
    crossScalaVersions := compilerBridgeScalaVersions,
    relaxNon212,
    libraryDependencies += scalaCompiler.value % "provided",
    autoScalaLibrary := false,
    // precompiledSettings,
    name := "Compiler Bridge",
    exportJars := true,
    // we need to fork because in unit tests we set usejavacp = true which means
    // we are expecting all of our dependencies to be on classpath so Scala compiler
    // can use them while constructing its own classpath for compilation
    fork in Test := true,
    // needed because we fork tests and tests are ran in parallel so we have multiple Scala
    // compiler instances that are memory hungry
    javaOptions in Test += "-Xmx1G",
    inBoth(unmanagedSourceDirectories ++= scalaPartialVersion.value.collect {
      case (2, y) if y == 10 => new File(scalaSource.value.getPath + "_2.10")
      case (2, y) if y >= 11 => new File(scalaSource.value.getPath + "_2.11+")
    }.toList),
    altPublishSettings
  )
  .configure(addSbtIO, addSbtUtilLogging)

val scalaPartialVersion = Def setting (CrossVersion partialVersion scalaVersion.value)

def inBoth(ss: Setting[_]*): Seq[Setting[_]] = Seq(Compile, Test) flatMap (inConfig(_)(ss))

// defines operations on the API of a source, including determining whether it has changed and converting it to a string
//   and discovery of Projclasses and annotations
lazy val zincApiInfo = (project in internalPath / "zinc-apiinfo")
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .dependsOn(compilerInterface, zincClassfile % "compile;test->test")
  .configure(addBaseSettingsAndTestDeps)
  .settings(
    name := "zinc ApiInfo",
    crossScalaVersions := compilerBridgeScalaVersions,
    relaxNon212
  )

// Utilities related to reflection, managing Scala versions, and custom class loaders
lazy val zincClasspath = (project in internalPath / "zinc-classpath")
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .dependsOn(compilerInterface)
  .configure(addBaseSettingsAndTestDeps)
  .settings(
    name := "zinc Classpath",
    crossScalaVersions := compilerBridgeScalaVersions,
    relaxNon212,
    libraryDependencies ++= Seq(scalaCompiler.value, launcherInterface)
  )
  .configure(addSbtIO)

// class file reader and analyzer
lazy val zincClassfile = (project in internalPath / "zinc-classfile")
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .dependsOn(compilerInterface % "compile;test->test", zincTesting % Test)
  .configure(addBaseSettingsAndTestDeps)
  .settings(
    name := "zinc Classfile",
    crossScalaVersions := compilerBridgeScalaVersions,
    relaxNon212
  )
  .configure(addSbtIO, addSbtUtilLogging)

// re-implementation of scripted engine
lazy val zincScripted = (project in internalPath / "zinc-scripted")
  .enablePlugins(ContrabandPlugin, JsonCodecPlugin)
  .disablePlugins(com.typesafe.sbt.SbtScalariform)
  .dependsOn(zinc, zincIvyIntegration % "test->test")
  .settings(
    minimalSettings,
    name := "zinc Scripted",
    publish := (),
    publishLocal := (),
    sourceManaged in (Compile, generateContrabands) := baseDirectory.value / "src" / "main" / "contraband-scala"
  )
  .configure(addSbtUtilScripted)

lazy val crossTestBridges = {
  Command.command("crossTestBridges") { state =>
    compilerBridgeScalaVersions.map { (bridgeVersion: String) =>
      s"plz $bridgeVersion ${compilerBridge.id}/test"
    } ::: state
  }
}

lazy val publishBridgesAndSet = {
  Command.args("publishBridgesAndSet", "<version>") { (state, args) =>
    require(args.nonEmpty, "Missing Scala version argument.")
    val userScalaVersion = args.mkString("")
    s"${compilerInterface.id}/publishLocal" ::
      compilerBridgeScalaVersions.flatMap { (bridgeVersion: String) =>
      s"wow $bridgeVersion" ::
        s"${zincApiInfo.id}/publishLocal" ::
        s"${compilerBridge.id}/publishLocal" :: Nil
    } :::
      s"wow $userScalaVersion" ::
      state
  }
}

lazy val publishBridgesAndTest = Command.args("publishBridgesAndTest", "<version>") {
  (state, args) =>
    require(args.nonEmpty,
            "Missing arguments to publishBridgesAndTest. Maybe quotes are missing around command?")
    val version = args mkString ""
    s"${compilerInterface.id}/publishLocal" ::
      // using plz here causes: java.lang.OutOfMemoryError: GC overhead limit exceeded
      (compilerBridgeScalaVersions flatMap { v =>
      s"wow $v" ::
        s";${zincApiInfo.id}/publishLocal;${compilerBridge.id}/test;${compilerBridge.id}/publishLocal" ::
        Nil
    }) :::
      s"plz $version zincRoot/test" ::
      s"plz $version zincRoot/scripted" ::
      state
}

val dir = IO.createTemporaryDirectory
val dirPath = dir.getAbsolutePath
lazy val tearDownBenchmarkResources = taskKey[Unit]("Remove benchmark resources.")
tearDownBenchmarkResources in ThisBuild := { IO.delete(dir) }

addCommandAlias(
  "runBenchmarks",
  s""";zincBenchmarks/run $dirPath
     |;zincBenchmarks/jmh:run -p _tempDir=$dirPath -prof gc
     |;tearDownBenchmarkResources
   """.stripMargin
)

lazy val otherRootSettings = Seq(
  Scripted.scriptedPrescripted := { addSbtAlternateResolver _ },
  Scripted.scripted := scriptedTask.evaluated,
  Scripted.scriptedUnpublished := scriptedUnpublishedTask.evaluated,
  Scripted.scriptedSource := (sourceDirectory in zinc).value / "sbt-test",
  publishAll := {
    val _ = (publishLocal).all(ScopeFilter(inAnyProject)).value
  }
)

def scriptedTask: Def.Initialize[InputTask[Unit]] = Def.inputTask {
  val result = scriptedSource(dir => (s: State) => scriptedParser(dir)).parsed
  publishAll.value
  // These two projects need to be visible in a repo even if the default
  // local repository is hidden, so we publish them to an alternate location and add
  // that alternate repo to the running scripted test (in Scripted.scriptedpreScripted).
  (altLocalPublish in compilerInterface).value
  (altLocalPublish in compilerBridge).value
  doScripted(
    (fullClasspath in zincScripted in Test).value,
    (scalaInstance in zincScripted).value,
    scriptedSource.value,
    result,
    scriptedPrescripted.value
  )
}

def addSbtAlternateResolver(scriptedRoot: File) = {
  val resolver = scriptedRoot / "project" / "AddResolverPlugin.scala"
  if (!resolver.exists) {
    IO.write(
      resolver,
      s"""import sbt._
         |import Keys._
         |
         |object AddResolverPlugin extends AutoPlugin {
         |  override def requires = sbt.plugins.JvmPlugin
         |  override def trigger = allRequirements
         |
         |  override lazy val projectSettings = Seq(resolvers += alternativeLocalResolver)
         |  lazy val alternativeLocalResolver = Resolver.file("$altLocalRepoName", file("$altLocalRepoPath"))(Resolver.ivyStylePatterns)
         |}
         |""".stripMargin
    )
  }
}

def scriptedUnpublishedTask: Def.Initialize[InputTask[Unit]] = Def.inputTask {
  val result = scriptedSource(dir => (s: State) => scriptedParser(dir)).parsed
  doScripted(
    (fullClasspath in zincScripted in Test).value,
    (scalaInstance in zincScripted).value,
    scriptedSource.value,
    result,
    scriptedPrescripted.value
  )
}

lazy val publishAll = TaskKey[Unit]("publish-all")
lazy val publishLauncher = TaskKey[Unit]("publish-launcher")

def customCommands: Seq[Setting[_]] = Seq(
  commands += Command.command("release") { state =>
    "clean" :: // This is required since version number is generated in properties file.
      "so compile" ::
      "so publishSigned" ::
      "reload" ::
      state
  }
)
