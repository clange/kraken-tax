val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Kraken tax",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.10"
  )