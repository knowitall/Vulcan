package edu.knowitall.vulcan.inference.mln

/**
 *
 */

import org.slf4j.LoggerFactory
import java.io.File
import scala.io.Source
import tuffy.parse.CommandOptions
import tuffy.util.{Config, UIMan}
import tuffy.main.NonPartInfer

object TuffyWrapper {
  val logger = LoggerFactory.getLogger(this.getClass)
  def instance(path:String) = new TuffyWrapper(path)
  def main(args:Array[String]) = {
    val tuffy = new TuffyWrapper("")
    tuffy.runTuffyNative(args)
  }
}

/**
 * Wrapper methods for running Tuffy.
 * External and native.
 */
class TuffyWrapper(path:String){

  def runTuffyNative(args:Array[String])  {
    val options: CommandOptions = UIMan.parseCommand(args)
    UIMan.println("Running native: " + Config.product_name + "!")
    if (options == null) {
       println("No valid args specified.")
    }
    (options.isDLearningMode, options.disablePartition) match {
      case (false, false) => new PartInferDebug().run(options)
      case (false, true) => new NonPartInfer().run(options)
      case (true, _) => println("Learning mode not supported in native tuffy.")
    }

  }

  def runTuffy(directory:String):String = {
    val dirPath = new File(directory).getAbsolutePath
    val efile = new File(dirPath + "/evidence.db")
    val pfile = new File(dirPath + "/prog.mln")
    val qfile = new File(dirPath + "/query.db")
    val outfile = new File(dirPath + "/output.txt")
    import edu.knowitall.vulcan.inference.utils.RunCommandUtil._
    val args = path + "/tuffy.sh"::
      "-conf " + path + "/tuffy.conf"::
      "-marginal" ::
      "-i %s".format(pfile.getAbsolutePath) ::
      "-e %s".format(efile.getAbsolutePath) ::
      "-queryFile %s".format(qfile.getAbsolutePath) ::
      "-r %s".format(outfile.getAbsolutePath) ::
      Nil
    println("Command: " + args.toArray.mkString(" "))
    val (stdout, stderr) = run(args.toArray)
    outfile.exists() match {
      case true => Source.fromFile(outfile).getLines.mkString("\n")
      case false => stdout + "\n" + stderr
    }
  }


}
