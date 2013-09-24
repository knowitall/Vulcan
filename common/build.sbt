name := "vulcan-common"

organization := "edu.washington.cs.knowitall"

version := "0.2-SNAPSHOT"

scalaVersion := 2.10.2

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

// TODO change this to use the new local repo
publishTo <<= version { (v: String) =>
  if (v.trim.endsWith("SNAPSHOT"))
    Some(Resolver.file("file", new File("/remote/knowitall-maven/maven2-snapshots")))
  else
    Some(Resolver.file("file", new File("/remote/knowitall-maven/maven2")))
}

