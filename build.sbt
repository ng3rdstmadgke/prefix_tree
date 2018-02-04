import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    test in assembly := {},
    mainClass in assembly := Some("example.Main"),
    assemblyJarName in assembly := "prefix_tree.jar",
    name := "prefix_tree",
    libraryDependencies += scalaTest % Test
  )
