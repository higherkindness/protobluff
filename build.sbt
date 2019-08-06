import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.pepegar"
ThisBuild / organizationName := "protobluff"

lazy val root = (project in file("."))
  .settings(
    name := "protobluff",
    libraryDependencies ++= Seq(
      atto,
      specs2 % Test,
      catsScalacheck % Test
      ),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits"),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )
