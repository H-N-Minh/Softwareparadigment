// DO NOT change this file!
name := "SWP Interpreter"

version := "1.0"

scalaVersion := "2.13.11"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")
