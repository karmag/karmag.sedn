organization := "karmag"
name := "sedn"
version := "0.2.0-SNAPSHOT"

scalaVersion := "2.12.1"

crossScalaVersions := Seq("2.11.8", "2.12.1")

scalacOptions in ThisBuild ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:existentials"
)

libraryDependencies += "org.clojure" % "clojure" % "1.9.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
