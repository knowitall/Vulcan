name := "vulcan-inference"

organization := "edu.washington.cs.knowitall"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

resolvers += "Sonatype SNAPSHOTS" at "https://oss.sonatype.org/content/repositories/snapshots/"

resolvers ++= Seq("scala-tools.org" at "http://scala-tools.org/repo-releases",
                  "Sonatype SNAPSHOTS" at "https://oss.sonatype.org/content/repositories/snapshots/",
                  "conjars" at "http://conjars.org/repo",
                  "apache.releases" at "https://repository.apache.org/content/repositories/releases",
                  "knowitall-public" at "http://knowitall.cs.washington.edu/maven2",
                  "knowitall-snapshots" at "http://knowitall.cs.washington.edu/maven2-snapshot",
                  "Local Maven Repository" at "file://" + Path.userHome + "/.m2/repository"
                )

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-library" % "2.10.2",
    //
    // Open IE and NLP Tools.
    //
    "edu.washington.cs.knowitall.openie" %% "srlie" % "4.0-SNAPSHOT",
    "edu.washington.cs.knowitall.nlptools" %% "nlptools-sentence-opennlp" % "2.4.2",
    //
    //Vulcan common lib
    //
    "edu.washington.cs.knowitall" %% "vulcan-common" % "0.2-SNAPSHOT",
    //
    //Netty, Jetty, unfiltered.
    //
    "net.databinder" %% "unfiltered-netty" % "0.6.8",
    "net.databinder" %% "unfiltered-netty-server" % "0.6.8",
    "net.databinder" %% "unfiltered-jetty" % "0.6.5",
    "net.databinder" %% "unfiltered-filter" % "0.6.5",
    //
    //Jena inference.
    "com.hp.hpl.jena" % "sdb" % "1.3.4",
    "org.apache.jena" % "apache-jena-libs" % "2.10.1",
    "mysql" % "mysql-connector-java" % "5.0.2",
    //
    //Tuffy inference
    "edu.wisc.cs.hazy" % "tuffy" % "0.3",
    "postgresql" % "postgresql" % "8.4-701.jdbc4",
    "org.antlr" % "antlr" % "3.2",
    "args4j" % "args4j" % "2.0.12",
    "org.apache.commons" % "commons-lang3" % "3.0",
    "thirdparty" % "jgrapht-jdk1.6" % "0.8.2",
    "org.apache.commons" % "commons-math" % "2.2",
    "junit" % "junit" % "4.9",
    //
    //Solr
    "org.apache.solr" % "solr-solrj" % "4.2.0",
    "commons-io" % "commons-io" % "2.4",
    "com.github.scopt" %% "scopt" % "2.1.0",
    //
    //Wordnet
    "net.sf.extjwnl" % "extjwnl" % "1.6.9"
)

scalacOptions ++= Seq("-unchecked", "-deprecation")

// custom options for high memory usage

licenses := Seq("Ollie Software License Agreement" -> url("https://raw.github.com/knowitall/ollie/master/LICENSE"))

homepage := Some(url("https://github.com/knowitall/Vulcan/inference"))