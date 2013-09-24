package edu.knowitall.vulcan.inference.kb

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/23/13
 * Time: 3:02 PM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import java.io.File
import scala.io.Source

object LogicRules {
  val logger = LoggerFactory.getLogger(this.getClass)

  def fromString(string:String) = WeightedRule.fromMLNString(string)

  def fromFile(file: File) = Source.fromFile(file).getLines().flatMap(fromString).toSeq

}
