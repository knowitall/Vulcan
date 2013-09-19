package edu.knowitall.vulcan.inference.kb

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 8/13/13
 * Time: 3:22 PM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import edu.knowitall.openie.Extraction

object BinaryRelationTuple{

  def fromExtraction(extraction:Extraction) = {
    new BinaryRelationTuple(extraction.arg1.text, extraction.rel.text, extraction.arg2s.map(x => x.text).mkString(" "))
  }

}

class BinaryRelationTuple(val arg1:String, val relation:String, val arg2:String) {

  def text = "%s %s %s".format(arg1, relation, arg2)

  def tripleFormat() = "(%s, %s, %s)".format(arg1, relation, arg2)

  def relationText(): String = relation

  def args: Seq[String] = Seq(arg2)

}

trait RelationTuple{

  def text() = "%s %s %s".format(arg1Text, relationText, arg2Text)
  def relationText():String
  def args:Seq[String]
  def arg1Text = args(0)
  def arg2Text = args.drop(1).mkString(" ")
}

class NestedTuple(arg1:String, relation:String, arg2:BinaryRelationTuple) extends BinaryRelationTuple(arg1, relation, arg2.text)
