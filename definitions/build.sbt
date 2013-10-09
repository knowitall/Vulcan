credentials += Credentials("Sonatype Nexus Repository Manager",
                           "trusty.cs.washington.edu",
                           "deployment",
                           "knowit!")

name := "vulcan-definitions"

organization := "edu.washington.cs.knowitall"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"


resolvers ++= Seq(
  		"Trusty Snapshots" at "http://trusty.cs.washington.edu:8082/nexus/content/repositories/snapshots",
  		"Trusty Releases" at "http://trusty.cs.washington.edu:8082/nexus/content/repositories/releases"
		)

resolvers += "Sonatype SNAPSHOTS" at "https://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
    "edu.washington.cs.knowitall.taggers" %% "taggers-core" % "0.3"
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

