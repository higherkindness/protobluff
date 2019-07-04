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
      scalaTest % Test
    ),
    scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits")
  )
