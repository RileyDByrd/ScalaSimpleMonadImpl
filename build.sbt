ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.3"
ThisBuild / scalacOptions ++= Seq("-Ykind-projector:underscores", "-Yexplicit-nulls", "-new-syntax")

lazy val root = (project in file("."))
  .settings(
    name := "Scala Simple Monad Impl"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"
