name := "vulcan-textual-evidence"

organization := "edu.washington.cs.knowitall"

version := "0.1-SNAPSHOT"

crossScalaVersions := Seq("2.10.2", "2.9.3")

scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head }

resolvers += "Sonatype SNAPSHOTS" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
  // - openie-4.0 extraction
  //   we copy/pasted openie-4 source so we could make some small changes, from
  //     "edu.washington.cs.knowitall.openie" %% "srlie" % "4.0-SNAPSHOT",
  //   These are it's dependencies
  "edu.washington.cs.knowitall.openie" %% "openie-models" % "1.1-SNAPSHOT",
  "edu.washington.cs.knowitall.srlie" %% "srlie" % "1.0.0-RC2",
  "edu.washington.cs.knowitall.chunkedextractor" %% "chunkedextractor" % "1.0.5",
  // solr indexing
  "org.apache.solr" % "solr-solrj" % "4.0.0",
  "com.github.scopt" %% "scopt" % "2.1.0",
  "org.slf4j" % "slf4j-api" % "1.7.2",
  "org.slf4j" % "slf4j-simple" % "1.7.5",
  "commons-logging" % "commons-logging-api" % "1.0.4" 
)

scalacOptions ++= Seq("-unchecked", "-deprecation")

// custom options for high memory usage

javaOptions += "-Xmx4G"

javaOptions += "-XX:+UseConcMarkSweepGC"

fork in run := true

connectInput in run := true // forward stdin/out to fork

licenses := Seq("Ollie Software License Agreement" -> url("https://raw.github.com/knowitall/ollie/master/LICENSE"))

homepage := Some(url("https://github.com/knowitall/Vulcan"))
