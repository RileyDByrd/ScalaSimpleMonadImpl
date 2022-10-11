ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.0"
ThisBuild / scalacOptions += "-Ykind-projector:underscores"

lazy val root = (project in file("."))
  .settings(
    name := "Scala Simple Monad Impl"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"
