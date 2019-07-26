import sbt._

object Dependencies {
  lazy val atto = "org.tpolecat" %% "atto-core" % "0.6.5"
  lazy val specs2 = "org.specs2" %% "specs2-scalacheck" % "4.6.0"
  lazy val catsScalacheck = "io.chrisdavenport" %% "cats-scalacheck" % "0.1.1"
}
