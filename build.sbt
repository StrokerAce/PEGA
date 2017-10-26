name := "PEGA"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies ++=
  Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.6",
  "org.jfree" % "jfreechart" % "1.0.19",
  "org.scalatest" %% "scalatest" % "3.0.4",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.slf4j" % "slf4j-api" % "1.7.25",
  "org.slf4j" % "slf4j-simple" % "1.7.25",
  "org.scala-lang.modules" %% "scala-swing" % "2.0.1"
)