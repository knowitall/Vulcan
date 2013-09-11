import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "vulcan-te-frontend"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    "edu.washington.cs.knowitall" %% "vulcan-textual-evidence" % "0.1-SNAPSHOT"
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
