/*
 *   To change tsmOptions options while Compile add corresponding env. var or system property (system property has higher priority):
 * in sbt cmd to add default `sugar-tms` options sys prop call sbt with '-DdefaultTmsOptions="D;T"' or execute 'eval System.setProperty("defaultTmsOptions", "D;T")'
 * in sbt cmd to clear default `sugar-tms` options sys prop execute 'eval System.clearProperty("defaultTmsOptions")'
 * in sbt cmd to add override `sugar-tms` options sys prop call sbt with '-DoverrideTmsOptions="D;T"'or execute 'eval System.setProperty("overrideTmsOptions", "D;T")'
 * in sbt cmd to clear override `sugar-tms` options sys prop execute 'eval System.clearProperty("overrideTmsOptions")'
 *   Tms option "Embedded Fors Code View" (or "EFCV" shortly) may be used along with standard scalacOptions "-Ymacro-debug-lite" to see
 * for-like view macros output that will be embedded to output code as a local value string before the resulting code
 *
 *   To change `sugar-tms` Test options tmsTestDebug and/or tmsTestTrace while IDEA tests run :
 * In the IDEA test configuration add VM Options -DtmsTestDebug=true/false and/or -DtmsTestTrace=true/false (by default tmsTestDebug=false and tmsTestTrace=tmsTestDebug)
 *   To change tmsTestDebug and/or tmsTestTrace in Tms Tests SBT run add corresponding env. var or system property (system property has higher priority).
 *   At the project level in build sbt:
 * ThisBuild / javaOptions += "-DtmsTestDebug=true" // javaOptions: Options passed to a new JVM when forking.
 *   To pass this option to test run from sbt cmd add ';set javaOptions += "-DtmsTestDebug=true"' or ';eval System.setProperty("tmsTestDebug", "true")'
 * or call sbt with '-DtmsTestDebug=true' and/or '-DtmsTestTrace=true' like 'sbt -DtmsTestDebug=true <sbtCommands>'
 *
 * to run `sugar-tms` sbt tests for all Scala versions with non default JDK: sbt --java-home "%JAVA8_HOME%" +sugar-tms/test
 * to run `sugar-tms` sbt tests for all Scala versions with alternative parallelism N (default is 6 but always limited to 3/4 of CPU threads): sbt -DtmsTestJVMs=N +sugar-tms/test
 *
 *   Use of sbt-header plugin for the code license:
 * sbt> sugar-tms/headerCheckAll // to check all configurations (Compile & Test) sources headers (including non active Scala version sources)
 * sbt> sugar-tms/headerCreateAll // to update or create all configurations (Compile & Test) sources headers (including non active Scala version sources)
 * it looks like plugin fails to Create headers for tms-test-base subproject due to "test-internal->test" configuration, do it MANUALLY for 2 files.
 * The root project with Main may be not licensed.
 */

import com.jsuereth.sbtpgp.PgpKeys.publishSigned
import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport.headerLicense
import sbt.Def
import sbt.Keys.{aggregate, autoScalaLibrary, crossScalaVersions, homepage, mappings, packageDoc, publishMavenStyle, publishTo, testGrouping, testOptions}
import sbt.io.Path.basic
import xerial.sbt.Sonatype.autoImport.{sonatypeProjectHosting, sonatypePublishToBundle, sonatypeRepository}

import scala.util.Try
import scala.util.control.NoStackTrace

val scala211 = "2.11.12"
val scala21212 = "2.12.12"
val scala21213p = "2.12.14"
val scala213 = "2.13.6"
lazy val supportedScalaVersions = List(scala211, scala21212, scala21213p, scala213)

ThisBuild / version := "0.2.4"
ThisBuild / scalaVersion := supportedScalaVersions.last
ThisBuild / description := "A macro library with built-in implicits set that implements the Transparent Monads syntax and Monadic Flow Control interpretation in Scala"
ThisBuild / homepage := Some(url("https://github.com/SerhiyShamshetdinov/sugar-tms"))
ThisBuild / licenses += "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
ThisBuild / startYear := Some(2021)
ThisBuild / organization := "ua.org.sands"
ThisBuild / organizationName := "PE Shamshetdinov Serhiy (Kyiv, Ukraine)"
ThisBuild / organizationHomepage := Some(url("http://www.sands.org.ua/"))
ThisBuild / developers := List(Developer("SerhiyShamshetdinov", "Serhiy Shamshetdinov", "serhiy@sands.org.ua", url("https://github.com/SerhiyShamshetdinov")))
ThisBuild / resolvers += "Sonatype S01 OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots"

// Release locally (this require Sonatype credentials for ua.org.sands):
// - push latest changes of 'main' or: fetch the repo, checkout the 'main' branch, ensure it is up to date
// - verify the version in build.sbt is actual release non-SNAPSHOT one (commit if you change it)
// - if committed: push changes to GitHub and wait for the tests to finish (when failed: correct the code & start from the beginning)
// - if not committed: ensure tests were passed for the last commit in the main otherwise do any change or fix tests errors, commit & wait tests results
// - to publish all cross versions do (till staged on Sonatype):
//   >;project sugar-tms ;sonatypeBundleClean ;+publishSigned ;sonatypePrepare ;sonatypeBundleUpload
//   and do the rest at Sonatype, or see https://github.com/xerial/sbt-sonatype for other commands.
//   !Do not publish from the root: it is blocked now! Only local publishing of sugar-tms will work from the root: >publishLocal or >publishLocalSigned
// - On GitHub! add the release with _tag_ & _release title_ = 'v0.x.x' of a done release version
// - fetch repo
// - inc patch number and add '-SNAPSHOT' suffix to version in build.sbt
// - push the commit to GitHub & skip run tests!
lazy val sonatypePublishSettings = Seq[Setting[_]]( // some of settings do not work when done in ThisBuild / ...
  sonatypeCredentialHost := "s01.oss.sonatype.org", // For all Sonatype accounts created on or after February 2021
  sonatypeRepository := "https://s01.oss.sonatype.org/service/local", // For all Sonatype accounts created on or after February 2021
  sonatypeProjectHosting := Some(xerial.sbt.Sonatype.GitHubHosting("SerhiyShamshetdinov", "sugar-tms", "serhiy@sands.org.ua")),
  publishTo := sonatypePublishToBundle.value,
  publishMavenStyle := true, // To sync with Maven central, you need to supply the following information
  versionScheme := Some("early-semver"),
)

lazy val commonSettings = Seq[Setting[_]](
  crossScalaVersions := supportedScalaVersions,
  autoScalaLibrary := false,
  Compile / unmanagedSourceDirectories ++= versionedSourceDirs((Compile / sourceDirectory).value, scalaVersion.value),
  Test / unmanagedSourceDirectories ++= versionedSourceDirs((Test / sourceDirectory).value, scalaVersion.value),
  versionedScalacOptions,
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"), // -Pn does not work with SBT
  // for sbt-header plugin to add all Scala version source files
  Compile / headerSources := ((Compile / sourceDirectory).value ** "*.scala").get,
  Test / headerSources := ((Test / sourceDirectory).value ** "*.scala").get,
  headerLicense := Some(HeaderLicense.Custom( // don't move to ThisBuild! it'll not work
    """Transparent Monads syntax and Monadic Flow Control interpretation
      |
      |Copyright (c) 2021 Serhiy Shamshetdinov (Kyiv, Ukraine)
      |
      |Licensed under the Apache License, Version 2.0 (the "License");
      |you may not use this file except in compliance with the License.
      |You may obtain a copy of the License at
      |
      |    http://www.apache.org/licenses/LICENSE-2.0
      |
      |Unless required by applicable law or agreed to in writing, software
      |distributed under the License is distributed on an "AS IS" BASIS,
      |WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
      |See the License for the specific language governing permissions and
      |limitations under the License.
      |
      |See the NOTICE file distributed with this work for
      |additional information regarding copyright ownership and used works.
      |""".stripMargin
  ))
)

// source dirs sample is here: https://docs.scala-lang.org/overviews/core/collections-migration-213.html#how-do-i-cross-build-my-project-against-scala-212-and-scala-213
def versionedSourceDirs(rootDir: File, scalaVersionValue: String): Seq[File] =
  scalaVersionValue match {
    case ver if ver >= "3.0" =>
      Seq()
    case ver if ver >= "2.13" =>
      Seq(rootDir / "scala-2@13+") // @ here means: only Scala 2 starting 2.13 inclusive and higher till Scala 3 (excluding). Contraryly 2.13+ means all version above 2.13 including 3.0, etc.
    case ver if ifScala212at13p(ver, whenScala212at13p = true, otherwise = false) =>
      Seq(rootDir / "scala-2.12@13+", rootDir / "scala-2.12-") // "2.12-" means: any 2.12.x and less: 2.11.x, etc.
    case _ =>
      Seq(rootDir / "scala-2.12.12-", rootDir / "scala-2.12-")
  }

//ThisBuild / Test / parallelExecution := false // true is default
//ThisBuild / Test / fork := true
//ThisBuild / Test / testForkedParallel := true
// With "fork := true & testForkedParallel := true" & without tests grouping all Test Suites are run in parallel in 8 (CPU log.) threads in 1 separate JVM (1 common Test Group).
// 8 threads = at CPU - I'm not able to change this parallelism level with SBT Tags: good reading is https://www.yannmoisan.com/sbt-parallel-test.html
// But the random Exceptions (when several macros are executed in parallel in 1 process) occur (with different random packages, not only `sands`):
//[info]   java.lang.AssertionError: assertion failed: sands package sands <none>

// for Tests grouping there are no such Exception. To set max number of separate JVMs:
//Global / concurrentRestrictions := Seq(Tags.limitAll(8), Tags.limit(Tags.ForkedTestGroup, 4)) // WORKS! but it looses 1 Custom(?) def: default is List(Limit all to 8, Limit forked-test-group to 1, sbt.Tags$Custom@5653429e)!!!
//Global / concurrentRestrictions += Tags.limit(Tags.ForkedTestGroup, 4) // does NOT work because of 2 defs: List(Limit all to 8, Limit forked-test-group to 1, sbt.Tags$Custom@5653429e, Limit forked-test-group to 4)

def getSysPropOrEnvVal(key: String): Option[String] = scala.sys.props.get(key).orElse(scala.sys.env.get(key))

val defaultMemLimitedTmsTestJVMs = 6 // the limit is mostly due to memory errors, not a CPU load. But each JVM actually uses more that 1 thread)
lazy val tmsTestJVMs: Int = {
  val extTmsTestJVMs = getSysPropOrEnvVal("tmsTestJVMs")
  extTmsTestJVMs.fold(defaultMemLimitedTmsTestJVMs) { extJVMs =>
    Try(extJVMs.toInt.ensuring(_ >= 1)).getOrElse {
      println(s"[warn] * sugar-tms: system property or environment variable 'tmsTestJVMs=$extJVMs' is ignored. It should be a valid positive number")
      defaultMemLimitedTmsTestJVMs
    }
  }
}
val cpuThreads = java.lang.Runtime.getRuntime.availableProcessors
lazy val forkedTestGroup = { // when Tags.ForkedTestGroup > 1 then tests are forked even if fork := true is not set
  val ftg = tmsTestJVMs.min((cpuThreads * 3 + 2)/ 4) // 1 -> 1, 2 -> 2, 3 -> 2, 4 -> 3, ..., 8 -> 6
  println(s"[info] * sugar-tms: sbt tests will run in parallel in $ftg JVMs each with 1 test suite. Default $defaultMemLimitedTmsTestJVMs JVMs may be overridden by 'tmsTestJVMs' sys prop. JVMs number is limited up to 3/4 of $cpuThreads CPU threads detected")
  ftg
}

Global / concurrentRestrictions := // This is necessarily a global set of rules, so it must be scoped as Global / https://www.scala-sbt.org/1.x/docs/Parallel-Execution.html#Adjustments+to+Defaults
  (Global / concurrentRestrictions).value.filterNot { tagsRule: Tags.Rule =>
    tagsRule.toString.contains(Tags.ForkedTestGroup.name) // dropping default rule
  } :+ Tags.limit(Tags.ForkedTestGroup, forkedTestGroup) // sets the number of parallel JVMs for tests (each JMV will run 1 TestGroup set to exactly 1 test Suite later in Test / testGrouping)
  // OK: 6 threads in sbt shell with sbt -Xmx1024M without -XX:HeapBaseMinAddress=32g
  // 6 (at CPU 8=2x4) is limited due to memory limit: with 6 JVMs & -Xmx2048M no native mem bugs at 32Gb PC RAM in native sbt (not a shell) without -XX:HeapBaseMinAddress=32g, but actually 6 threads is 10% faster than at CPUthreads=8 (2x4)
  // 8 (at CPU 8=2x4) fails with native mem error in 2.11 from sbtShell under Windows even with -XX:HeapBaseMinAddress=32g at 32Gb RAM
  // when tests fail with
  //# There is insufficient memory for the Java Runtime Environment to continue.
  //# Native memory ...
  // set sbt JVM option -XX:HeapBaseMinAddress=32g does NOT help

def ifScala212at13p[T](scalaVersionString: String, whenScala212at13p: T, otherwise: T): T =
  if ("""^2\.12\.(\d+).*""".r.findFirstMatchIn(scalaVersionString).exists(_.group(1).toInt >= 13)) whenScala212at13p else otherwise

def scalaTestDependencies(configurations: String*) = libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.3", // 3.2.9 fails with strange error for 2.12.12 in sbt +sugar-tms/test: framework does not start due to an "illegal override" error
  "org.scala-lang" % "scala-compiler" % scalaVersion.value // for the scala.tools.reflect.{ToolBox, ...}
).map(configurations.foldLeft(_)(_ % _))

// root defines the project to test published sugar-tms jars or sources for calling macro variants directly & in the toolbox
// sands.sugar.tms.Main may be used as "worksheet" for playing with macros
// if the following dependency version is None then root will depend on sugar-tms project sources directly
lazy val root = {
  val rootWithoutTms = Project("root", file("."))
    .settings(commonSettings)
    .settings(
      publish / skip := true,
      scalacOptions ++= Seq(
        //      "-Ymacro-debug-lite"
      )
    )
    .dependsOn(`tms-test-base` % "test->test")

  getSysPropOrEnvVal("rootTmsArtifactDependencyVersion").fold({
    println("[info] * root: root test project depends directly on sugar-tms subproject")
    rootWithoutTms
      .dependsOn(`sugar-tms`)
      .aggregate(`sugar-tms`)
      .settings(
        Test / aggregate := false,
        publish / aggregate := false,
        publishSigned / aggregate := false
      )
  }) { tmsArtifactVersion =>
    println(s"[info] * root: root test project depends on sugar-tms '$tmsArtifactVersion' artifact")
    rootWithoutTms
      .settings(
        libraryDependencies += ifScala212at13p(scalaVersion.value,
          whenScala212at13p = "ua.org.sands" % "sugar-tms_2.12at13+" % tmsArtifactVersion withSources() withJavadoc(),
          otherwise         = "ua.org.sands" %% "sugar-tms" % tmsArtifactVersion withSources() withJavadoc()
        )
      )
      .aggregate(`tms-test-base`)
  }
}

// helper test base project
lazy val `tms-test-base` = project.in(file("tms-test-base"))
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    scalaTestDependencies()
  )

// helper macro-annotations project for sugar-tms
lazy val `tms-annotations` = project.in(file("tms-annotations"))
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    macroLibraryDependencies
  )

// primary project
lazy val `sugar-tms` = project.in(file("sugar-tms"))
  .settings(commonSettings)
  .settings(
    crossVersion := ifScala212at13p(scalaVersion.value,
      whenScala212at13p = CrossVersion.binaryWith(prefix = "", suffix = "at13+"),
      otherwise = crossVersion.value
    ),
    macroLibraryDependencies,

    // for tests grouping: to run each 1 test suite in separate JVM (1 group contains exactly 1 test suite):
    Test / testGrouping := {
      val runPolicy = Tests.SubProcess(ForkOptions( // sample is here https://stackoverflow.com/questions/15798341/how-to-fork-the-jvm-for-each-test-in-sbt but corrected for sbt 1.4.4
        javaHome.value,
        outputStrategy.value,
        Vector(),
        Some(baseDirectory.value),
        javaOptions.value.toVector,
        connectInput.value,
        envVars.value
      ))

      (Test / definedTests).value
        .sortBy { test => // Sort does not helps: it loads JVMs in random order from this tasks list
          if (HeavyTests.exists(test.name.contains))
            0 // Heavy - first
          else if (LightTests.exists(test.name.contains))
            20 + LightTests.indexWhere(test.name.contains) // Light - last (to better distribute tasks between JVMs at the end)
          else
            10 // others - in the middle
        }
        .map { test =>
          Tests.Group(test.name, Seq(test), runPolicy) // exactly 1 test Suite per TestGroup (JVM)
        }
    },
    // include the macro-annotation classes and resources in the `sugar-tms` jar
    Compile / packageBin / mappings ++= (`tms-annotations` / Compile / packageBin / mappings).value ++ packageFileMappings,
    // include the macro-annotation sources in the `sugar-tms` source jar
    Compile / packageSrc / mappings ++= (`tms-annotations` / Compile / packageSrc / mappings).value ++ packageFileMappings,
    // include the macro-annotation docs in the `sugar-tms` source jar: with dependency "compile-internal" below it fails: 2 index.html :)
    Compile / packageDoc / mappings ++= /*(`tms-annotations` / Compile / packageDoc / mappings).value ++*/ packageFileMappings
  )
  .settings(sonatypePublishSettings)
  .dependsOn(`tms-annotations` % "compile-internal, test-internal") // see https://www.scala-sbt.org/1.x/docs/Macro-Projects.html skips dependency on it in .pom
  .dependsOn(`tms-test-base` % "test-internal->test") // see https://www.scala-sbt.org/1.x/docs/Macro-Projects.html skips dependency on it in .pom
  .aggregate(`tms-annotations`, `tms-test-base`)

lazy val packageFileMappings: Seq[(File, String)] = List(
  file("LICENSE"),
  file("NOTICE"),
  file("tms_and_mfc.md"),
  file("readme.md"),
  file("CONTRIBUTING.md")
).pair(basic, errorIfNone = true)

val HeavyTests = Seq(
  "TmsForGeneralTest",
  "TmsForDefScopeUsageTest",
  "TmsForStringImplicitMethodsTest",
  "TmsMonadicFlowTest"
)
val LightTests = Seq(
  "TmsForCharIndexedSeqTest",
  "TmsForReachableSyntaxTest",
  "TmsForPrimitivesTest",
  "TmsForStringNativeMethodsTest",
  "TmsForGeneralPredefTest"
)

val allVersionsScalacOptions = Seq(
  "-encoding", "UTF-8",
  "-target:jvm-1.8", // jvm-1.8 is default for 2.11-2.13: for jar compatibility with all JDK starting 8. Keep in mind that util.Properties.javaVersion returns version of JVM run, not this target version. So used Java methods set depends on run JDK, not on this target version :)
  "-unchecked",
// "-Ymacro-debug-lite",
//  "-deprecation", // tests test deprecated staff too: it makes no sense to twitch
  "-language:_"
)

def versionedScalacOptions: Def.Setting[Task[Seq[String]]] = scalacOptions := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    // if scala 2.13+ is used, quasiquotes are merged into scala-reflect, Macro annotations are available in Scala 2.13 with the -Ymacro-annotations
    case Some((2, minor)) if minor >= 13 =>
      scalacOptions.value ++ allVersionsScalacOptions ++ Seq("-Ymacro-annotations")
    case _ =>
      scalacOptions.value ++ allVersionsScalacOptions
  }
}

def macroLibraryDependencies: Def.Setting[Seq[ModuleID]] = libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    // if scala 2.13+ is used, quasiquotes are merged into scala-reflect, Macro annotations are available in Scala 2.13 with the -Ymacro-annotations
    case Some((2, minor)) if minor >= 13 =>
      Seq(
        // scala-reflect is required only for macro execution: compile time on target project
        // to skip scala-reflect & `sugar-tms` artifacts propagating to target project's .pom add '% "compile-internal, test-internal"' to `sugar-tms` dependency
        "org.scala-lang" % "scala-reflect" % scalaVersion.value
      )
    // if scala 2.11-2.12 is used, quasiquotes are merged into scala-reflect, Macro annotations are available with the macro paradise plugin from Scala 2.10.x to Scala 2.12.x
    case Some((2, minor)) if minor >= 11 =>
      Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
      )
    case _ =>
      throw new IllegalArgumentException("Scala versions < 2.11 are not supported by the project") with NoStackTrace
  }
}
