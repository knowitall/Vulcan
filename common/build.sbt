name := "vulcan-common"

organization := "edu.washington.cs.knowitall"

version := "0.1-SNAPSHOT"

crossScalaVersions := Seq("2.10.2", "2.9.3")

scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head }

resolvers += "Sonatype SNAPSHOTS" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
  // for play.api.libs.json serialization
  // play 2.2 is supposed to break-out play-json as a separate pacakge.
  // until then we take the whole thing
  "play" %% "play" % "2.1.3"
)

scalacOptions ++= Seq("-unchecked", "-deprecation")

// custom options for high memory usage

licenses := Seq("Ollie Software License Agreement" -> url("https://raw.github.com/knowitall/ollie/master/LICENSE"))

homepage := Some(url("https://github.com/knowitall/Vulcan"))
