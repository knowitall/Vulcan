name := "vulcan-te-backend"

organization := "edu.washington.cs.knowitall"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

resolvers ++= Seq(
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
  "Sonatype SNAPSHOTS" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Trusty Releases" at "http://trusty.cs.washington.edu:8082/nexus/content/repositories/releases",
  "Trusty Snapshots" at "http://trusty.cs.washington.edu:8082/nexus/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  // openie extraction dependencies
  "edu.washington.cs.knowitall.openie" %% "openie-models" % "1.1-SNAPSHOT",
  "edu.washington.cs.knowitall.srlie" %% "srlie" % "1.0.1",
  "edu.washington.cs.knowitall.chunkedextractor" %% "chunkedextractor" % "1.0.5",
  "edu.washington.cs.knowitall" %% "vulcan-common" % "0.3-SNAPSHOT",
  // for cli
  "com.github.scopt" %% "scopt" % "2.1",
  // solr indexing
  "org.apache.solr" % "solr-solrj" % "4.3.1",
  "org.slf4j" % "slf4j-api" % "1.7.2",
  "org.slf4j" % "slf4j-simple" % "1.7.5",
  "commons-logging" % "commons-logging-api" % "1.0.4",
  "commons-io" % "commons-io" % "2.4",
  //
  "edu.washington.cs.knowitall.common-scala" %% "common-scala" % "1.1.2",
  // headword dependendcies
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-headword-uw" % "2.4.4-SNAPSHOT",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-core" % "2.4.4-SNAPSHOT",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-sentence-opennlp" % "2.4.3"
) 

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

// custom options for high memory usage

javaOptions += "-Xmx8G"

javaOptions += "-XX:+UseConcMarkSweepGC"

fork in run := true

connectInput in run := true // forward stdin/out to fork

homepage := Some(url("https://github.com/knowitall/Vulcan"))
