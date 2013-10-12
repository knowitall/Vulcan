import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "vulcan-te-frontend"
  val appVersion      = "0.1-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    "edu.washington.cs.knowitall" %% "vulcan-common" % "0.5-SNAPSHOT",
    "edu.washington.cs.knowitall" %% "vulcan-te-client" % "0.5-SNAPSHOT",
    "org.apache.solr" % "solr-solrj" % "4.3.1"
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(

    organization := "edu.washington.cs.knowitall",

    resolvers ++= Seq(
      "Trusty Snapshots" at 
        "http://trusty.cs.washington.edu:8082/nexus/content/repositories/snapshots",
      "Trusty Releases" at 
        "http://trusty.cs.washington.edu:8082/nexus/content/repositories/releases"
    )
  )
}
