package edu.knowitall.vulcan.inference.utils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 8/14/13
 * Time: 4:19 PM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import java.io.{InputStreamReader, BufferedReader}

object RunCommandUtil {
  val logger = LoggerFactory.getLogger(this.getClass)

  def run(args:Array[String]): (String, String) = {
    val rt = Runtime.getRuntime()
    val p = rt.exec(args.toArray)
    val stdInput = new BufferedReader(new InputStreamReader(p.getInputStream))
    val stdError = new BufferedReader(new InputStreamReader(p.getErrorStream))
    // read the output from the command
    System.err.println("Here is the standard output of the command:\n");

    val stdout = new StringBuilder
    val stderr = new StringBuilder
    var s = stdInput.readLine
    while (s != null) {
      stdout.append(s).append("\n")
      System.err.println(s)
      s = stdInput.readLine()
    }

    System.err.println("Here is the standard error of the command (if any):\n");


    s = stdError.readLine
    while (s != null) {
      stderr.append(s).append("\n")
      System.err.println(s)
      s = stdError.readLine()
    }
    p.waitFor()
    (stdout.toString, stderr.toString)
  }

}
