package edu.knowitall.vulcan.inference.matching

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/11/13
 * Time: 12:05 PM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import edu.washington.cs.knowitall.morpha.MorphaStemmer

class Stemmer{

  def stem(string:String) = string.split(" ").map(MorphaStemmer.stem(_)).mkString(" ")
}