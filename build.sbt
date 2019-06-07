scalaVersion := "2.12.8"

name := "rmp"
organization := "aaarne.tum"
version := "1.0"

enablePlugins(JavaAppPackaging)

val breezeVersion = "0.13.2"

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % breezeVersion,
  "org.scalanlp" %% "breeze-natives" % breezeVersion,
  "org.scalanlp" %% "breeze-viz" % breezeVersion,
)


libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

mainClass in Compile := Some("aaarne.tum.rmp.Main")

scalacOptions in ThisBuild ++= Seq("-language:implicitConversions", "-feature")
