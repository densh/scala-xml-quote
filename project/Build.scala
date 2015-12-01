import sbt._, Keys._

object XmlquoteBuild extends Build {
  val sharedSettings = Defaults.defaultSettings ++ Seq(
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.7",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq()
  )

  lazy val xmlquote: Project = Project(
    "xmlquote",
    file("."),
    settings = sharedSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3",
      libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
    )
  )
}
