import aether.Aether._

credentials += Credentials("Sonatype Nexus Repository Manager",
                           "trusty.cs.washington.edu",
                           "deployment",
                           "knowit!")

name := "vulcan-te-client"

organization := "edu.washington.cs.knowitall"

version := "0.2"

scalaVersion := "2.10.2"

resolvers ++= Seq(
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
  "Trusty Snapshots" at "http://trusty.cs.washington.edu:8082/nexus/content/repositories/snapshots",
  "Trusty Releases" at "http://trusty.cs.washington.edu:8082/nexus/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "play" %% "play" % "2.1.3",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
  "edu.washington.cs.knowitall" %% "vulcan-common" % "0.3-SNAPSHOT",
  "com.github.scopt" %% "scopt" % "2.1"
) 

scalacOptions ++= Seq("-unchecked", "-deprecation")

connectInput in run := true // forward stdin/out to fork

homepage := Some(url("https://github.com/knowitall/Vulcan"))

publishTo <<= version { (v: String) =>
  val nexus = "http://trusty.cs.washington.edu:8082/nexus/content/repositories/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "snapshots")
  else
    Some("releases" at nexus + "releases")
}

seq(aetherSettings: _*)
