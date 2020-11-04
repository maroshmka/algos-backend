ThisBuild / scalaVersion := "2.12.7"
ThisBuild / organization := "com.example"

lazy val hello = (project in file("."))
  .settings(
    name := "Hello",
  	libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  	libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.10"
  )

