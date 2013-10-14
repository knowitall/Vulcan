import aether.Aether._

credentials += Credentials("Sonatype Nexus Repository Manager",
                           "trusty.cs.washington.edu",
                           "deployment",
                           "knowit!")

name := "vulcan-common"

organization := "edu.washington.cs.knowitall"

version := "0.5-SNAPSHOT"

scalaVersion := "2.10.2"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  // for play.api.libs.json serialization
  // play 2.2 is supposed to break-out play-json as a separate pacakge.
  // until then we take the whole thing
  "play" %% "play" % "2.1.3",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-tokenize-clear" % "2.4.2"
)

scalacOptions ++= Seq("-unchecked", "-deprecation")

homepage := Some(url("https://github.com/knowitall/Vulcan"))

publishTo <<= version { (v: String) =>
  val nexus = "http://trusty.cs.washington.edu:8082/nexus/content/repositories/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "snapshots")
  else
    Some("releases" at nexus + "releases")
}

seq(aetherSettings: _*)

