import sbt._
import sbt.Keys._

// format: off
ThisBuild / organization      := "net.chwthewke"
ThisBuild / scalaOrganization := "org.scala-lang"
ThisBuild / scalaVersion      := "2.13.10"
// TODO when I can make sense of lm-coursier
ThisBuild / conflictManager                        := ConflictManager.strict
ThisBuild / updateSbtClassifiers / conflictManager := ConflictManager.default
// format: on

enablePlugins( FormatPlugin, DependenciesPlugin )

val core = project
  .settings( libraryDependencies ++= cats ++ catsEffect )
  .enablePlugins( SbtBuildInfo, ScalacPlugin )

val tests = project
  .settings( libraryDependencies ++= (scalatest ++ scalacheck).map( _ % "test" ) )
  .dependsOn( core )
  .enablePlugins( ScalacPlugin )

val `aoc22` = project
  .in( file( "." ) )
  .aggregate(
    core,
    tests
  )
