name := "algelab"

scalaVersion in ThisBuild := "2.12.1"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)


import sbt._

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.8.1"
  , "org.scalaz" %% "scalaz-core" % "7.3.0-M7"
  , "org.scalatest" %% "scalatest" % "3.0.1" % Test
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

