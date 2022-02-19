val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Kraken tax",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.11",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test",
    libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.10"
  )
