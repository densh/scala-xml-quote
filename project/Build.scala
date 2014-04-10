import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalamacros",
    version := "1.0.0",
    scalaVersion := "2.11.0-RC4",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq()
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in tests
    )
  ) aggregate(xmlquote, tests)

  lazy val xmlquote: Project = Project(
    "xmlquote",
    file("xmlquote"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies += "org.scalamacros" %% "xml" % "1.0.0-M1"
    )
  )

  lazy val tests: Project = Project(
    "tests",
    file("tests"),
    settings = buildSettings ++ Seq(
      libraryDependencies += "org.scalamacros" %% "xml" % "1.0.0-M1"
    )
  ) dependsOn(xmlquote)
}
