name := "vulcan-te-client"

organization := "edu.washington.cs.knowitall"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "play" %% "play" % "2.1.3",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
  "edu.washington.cs.knowitall" %% "vulcan-common" % "0.2-SNAPSHOT",
  "com.github.scopt" %% "scopt" % "2.1"
) 

scalacOptions ++= Seq("-unchecked", "-deprecation")

connectInput in run := true // forward stdin/out to fork

homepage := Some(url("https://github.com/knowitall/Vulcan"))
