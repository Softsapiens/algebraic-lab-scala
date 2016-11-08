name := "algelab"

scalaVersion in ThisBuild := "2.12.0-RC2"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.2")

import sbt._

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.8.0"
  , "org.scalaz" %% "scalaz-core" % "7.3.0-M6"
  , "org.scalatest" %% "scalatest" % "3.0.0" % Test
  , "com.lihaoyi" %% "sourcecode" % "0.1.3"
 )

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Ypartial-unification",
  //"-Ylog-classpath",
  // "-Xprint:typer",
  // "-Xlog-implicit-conversions",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:higherKinds")

initialCommands in console := """
  |import algelab._
  """.stripMargin

