package edu.knowitall.vulcan.inference.mln.tuffyimpl

import org.slf4j.LoggerFactory
import java.io.File
import tuffy.parse.CommandOptions
import tuffy.util.{Config, UIMan}
object TuffyWrapper {

  val logger = LoggerFactory.getLogger(this.getClass)
  def main(args:Array[String])  {
    val tuffy = new TuffyWrapper("")
    tuffy.runTuffyNative(args)
  }
}
/**
 * Wrapper methods for running Tuffy.
 * External and native.
 */
class TuffyWrapper(confFile:String){

  val logger = LoggerFactory.getLogger(this.getClass)

  def toTuffyArgs(tempDirectory:String) = {
    val cfile = new File(confFile)
    val pfile = new File(tempDirectory + File.separator + "prog.mln")
    val efile = new File(tempDirectory + File.separator + "evidence.db")
    val qfile = new File(tempDirectory + File.separator + "query.db")
    val outfile = new File(tempDirectory + File.separator + "output.txt")
      "-conf":: cfile.getAbsolutePath ::
      "-marginal" ::
      "-i"::pfile.getAbsolutePath ::
      "-e"::efile.getAbsolutePath ::
      "-queryFile"::qfile.getAbsolutePath ::
      "-r"::outfile.getAbsolutePath ::
      "-printResultsAsPrologFacts"::
      "-mcsatSamples"::"1000"::
      "-numInfIters":: "1"::
      Nil
  }
  def runTuffyNative(tempDirectory:String): Option[InferenceResults] = {
    val args = toTuffyArgs(tempDirectory)
    logger.info("Args: " + args.toArray.mkString(" "))
    runTuffyNative(args.toArray)
  }
  def runTuffyNative(args:Array[String]): Option[InferenceResults] = {
    UIMan.println("Running native: " + Config.product_name + "!")

    val options: CommandOptions = UIMan.parseCommand(args)
    if (options == null) {
       println("No valid args specified.")
    }
    (options.isDLearningMode, options.disablePartition) match {
      case (false, false) => Some(new PartInferDebug().run(options))
      case (false, true) => {
        logger.error("Non-partition inference mode not supported.")
        None
      }
      case (true, _) => {
        logger.error("Learning mode not supported in native tuffy.")
        None
      }


    }

  }

  /**def runTuffy(directory:String) = {
    val dirPath = new File(directory).getAbsolutePath
    val efile = new File(dirPath + "/evidence.db")
    val pfile = new File(dirPath + "/prog.mln")
    val qfile = new File(dirPath + "/query.db")
    val outfile = new File(dirPath + "/output.txt")
    val args = //path + "/tuffy.sh"::
      "-conf " + confFile ::
      "-marginal" ::
      "-i %s".format(pfile.getAbsolutePath) ::
      "-e %s".format(efile.getAbsolutePath) ::
      "-queryFile %s".format(qfile.getAbsolutePath) ::
      "-r %s".format(outfile.getAbsolutePath) ::
      Nil
    println("Command: " + args.toArray.mkString(" "))
    runTuffyNative(args.toArray)
  }  */
}


//val (stdout, stderr) = run(args.toArray)
/**outfile.exists() match {
      case true => Source.fromFile(outfile).getLines.mkString("\n")
      case false => stdout + "\n" + stderr
}  */