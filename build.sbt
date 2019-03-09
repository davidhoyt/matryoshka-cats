import scoverage._
import sbt._, Keys._
import sbtcrossproject.CrossPlugin.autoImport.crossProject

lazy val scala2_12 = "2.12.8"
lazy val scala2_11 = "2.11.12"

lazy val catsVersion = "1.6.0"
lazy val monocleVersion = "1.5.1-cats"

lazy val supportedScalaVersions = List(scala2_12) //, scala2_11)

ThisBuild / scalaVersion := scala2_12

ThisBuild / crossScalaVersions := supportedScalaVersions

lazy val standardSettings = commonBuildSettings ++ Seq(
  Compile / logBuffered := false,
  Test / logBuffered := false,
  updateOptions := updateOptions.value.withCachedResolution(true),
  exportJars := true,
  organization := "com.slamdata",
  ScoverageKeys.coverageHighlighting := true,
  Compile / doc / scalacOptions ++= Seq("-groups", "-implicits"),
  Compile / compile / wartremoverWarnings -= Wart.ImplicitParameter, // see wartremover/wartremover#350 & #351
  libraryDependencies ++= Seq(
    "com.slamdata"                %% "slamdata-predef" % "0.0.7",
    "com.github.julien-truffaut" %%% "monocle-core"    % monocleVersion % "compile, test",
    "org.typelevel"              %%% "cats-core"       % catsVersion    % "compile, test",
    "org.typelevel"              %%% "cats-free"       % catsVersion    % "compile, test",
    "com.github.mpilquist"       %%% "simulacrum"      % "0.15.0"       % "compile, test"))

lazy val publishSettings = commonPublishSettings ++ Seq(
  organizationName := "SlamData Inc.",
  organizationHomepage := Some(url("http://slamdata.com")),
  homepage := Some(url("https://github.com/slamdata/matryoshka")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/slamdata/matryoshka"),
      "scm:git@github.com:slamdata/matryoshka.git")))

lazy val root = project.in(file("."))
  .settings(name := "matryoshka-cats")
  .settings(standardSettings ++ noPublishSettings: _*)
  .settings(
    console := (console in replJVM).value,
    crossScalaVersions := Nil,
    releaseCrossBuild := false
  )
  .aggregate(
    //coreJS, monocleJS, scalacheckJS,  testsJS,
    coreJVM, monocleJVM, scalacheckJVM, testsJVM
    //docs
  )
  .enablePlugins(AutomateHeaderPlugin)

lazy val core = crossProject(JVMPlatform, JSPlatform).in(file("core"))
  .settings(name := "matryoshka-core")
  .settings(standardSettings ++ publishSettings: _*)
  .enablePlugins(AutomateHeaderPlugin)

lazy val monocle = crossProject(JVMPlatform, JSPlatform).in(file("monocle"))
  .dependsOn(core)
  .settings(name := "matryoshka-monocle")
  .settings(standardSettings ++ publishSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "com.github.julien-truffaut" %%% "monocle-core" % monocleVersion % "compile, test"
  ))
  .enablePlugins(AutomateHeaderPlugin)

lazy val scalacheck = crossProject(JVMPlatform, JSPlatform)
  .dependsOn(core, monocle)
  .settings(name := "matryoshka-scalacheck")
  .settings(standardSettings ++ publishSettings: _*)
  .settings(libraryDependencies ++= Seq(
    // NB: Needs a version of Scalacheck with rickynils/scalacheck#301.
    "org.scalacheck" %% "scalacheck" % "1.14.0"))
  .enablePlugins(AutomateHeaderPlugin)

lazy val tests = crossProject(JVMPlatform, JSPlatform)
  .settings(name := "matryoshka-tests")
  .dependsOn(core, monocle, scalacheck)
  .settings(standardSettings ++ noPublishSettings: _*)
  .settings(libraryDependencies ++= Seq(
    //"com.github.julien-truffaut" %% "monocle-law"   % monocleVersion % Test,
    "org.specs2"                 %% "specs2-core"   % "4.3.5"        % Test))
  .enablePlugins(AutomateHeaderPlugin)

/** A project just for the console.
  * Applies only the settings necessary for that purpose.
  */
lazy val repl = crossProject(JVMPlatform, JSPlatform) dependsOn (tests % "compile->test") settings standardSettings settings (
  console := (console in Test).value,
  scalacOptions --= Seq("-Yno-imports", "-Ywarn-unused-import"),
  initialCommands in console += """
    |import cats._
    |import cats.implicits._
    |import matryoshka._
    |import matryoshka.data._
    |import matryoshka.implicits._
    |import matryoshka.patterns._
  """.stripMargin.trim
)

lazy val replJVM = repl.jvm
//lazy val coreJS  = core.js
lazy val coreJVM = core.jvm
lazy val monocleJVM = monocle.jvm
//lazy val scalacheckJS  = scalacheck.js
lazy val scalacheckJVM = scalacheck.jvm
//lazy val testsJS  = tests.js
lazy val testsJVM = tests.jvm
