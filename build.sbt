ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.6.1"
ThisBuild / scalacOptions ++= Seq(
  "-Yexplicit-nulls",  // Divorce `null`s from the rest of the type system.  Only the `Null` type may have a value of `null`.
  "-Xkind-projector:underscores",  // An underscore for a parameterized type indicates kind projection.  Unknown types use question marks.

  //  "-new-syntax",  // Rewrite control flow with `then` and `do`.  Must be used in combination with "-rewrite".
  "-deprecation",  // Warn for use of deprecated methods.
  "-feature",  // Warn for use of features without importing features.

  "-language:experimental.modularity",  // Enable experimental syntax for Type Classes, which use `type Self`.
  "-language:noAutoTupling",  // Disallow auto tupling, even where imported.  Auto tupling can result in odd behavior.
  "-language:strictEquality",  // Force `==` to rely on `CanEqual`, which makes comparing apples to oranges an issue of compilation.  Add `derives CanEqual` to case classes to get a sensible automatic implementation.

  "-Wunused:all",  // Warn for unused imports, values, etc.
  "-Wsafe-init",  // Warn when access to uninitialized objects is detected.
  "-Wvalue-discard",  // Warn when a value is discarded because the type is set to Unit.
  "-Wnonunit-statement"  // Warn when the returned value of a method is not used, as if the method is used for a side effect.  Available in 3.4+.
)

lazy val root = (project in file("."))
  .settings(
    name := "Scala Simple Monad Impl"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"
