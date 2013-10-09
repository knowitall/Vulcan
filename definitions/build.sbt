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
  // for play.api.libs.json serialization
  // play 2.2 is supposed to break-out play-json as a separate pacakge.
  // until then we take the whole thing
  "play" %% "play" % "2.1.3",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-tokenize-clear" % "2.4.2",
    "com.google.guava" % "guava" % "13.0.1",
    "edu.washington.cs.knowitall.nlptools" %% "nlptools-core" % "2.4.3",
    "edu.washington.cs.knowitall.nlptools" %% "nlptools-chunk-opennlp" % "2.4.3",
    "edu.washington.cs.knowitall.nlptools" %% "nlptools-stem-morpha" % "2.4.3",
    "edu.washington.cs.knowitall.nlptools" %% "nlptools-typer-stanford" % "2.4.3",
    "edu.washington.cs.knowitall" %% "openregex-scala" % "1.0.4",
    "org.apache.commons" % "commons-lang3" % "3.1",
    "org.jdom" % "jdom2" % "2.0.5",
    "junit" % "junit" % "4.11" % "test",
    "org.apache.commons" % "commons-io" % "1.3.2",
    "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
    "org.specs2" % "specs2" % "1.12.3" % "test" cross CrossVersion.binaryMapped {
      case "2.9.3" => "2.9.2"
      case "2.10.1" => "2.10"
      case x => x
   }
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

