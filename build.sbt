lazy val commonSettings = Seq(
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.8",
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += Resolver.sonatypeRepo("releases"),
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlint"
  ),
  // convenient when working with macros
  clean in Test := IO.delete((classDirectory in Test).value)
)

lazy val xmlquote = (project in file(".")).
  settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
      "com.lihaoyi" %% "fastparse" % "0.4.2",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    )
  )
