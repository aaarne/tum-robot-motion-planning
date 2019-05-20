scalaVersion := "2.12.8"

name := "tum-rmp"
organization := "de.tum.in"
version := "1.0"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"

val breezeVersion = "0.13.2"

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % breezeVersion,
  "org.scalanlp" %% "breeze-natives" % breezeVersion,
  "org.scalanlp" %% "breeze-viz" % breezeVersion,
)


libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

mainClass in (Compile, packageBin) := Some("tumrmp.Main")
mainClass in (Compile, run) := Some("tumrmp.Main")
