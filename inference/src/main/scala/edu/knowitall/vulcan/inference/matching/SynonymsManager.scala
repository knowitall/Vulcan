package edu.knowitall.vulcan.inference.matching

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/11/13
 * Time: 11:33 AM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import java.io.File
import edu.knowitall.tool.stem.MorphaStemmer

class SynonymsManager {

  val logger = LoggerFactory.getLogger(this.getClass)

  def synonyms(word:String) = word::Nil

  def stemPhrase(phrase:String) = phrase.split(" ").map(MorphaStemmer.stem(_)).mkString(" ")

  def areSynonyms(x:String, y:String) = {
    val xstem = stemPhrase(x)
    val ystem = stemPhrase(y)
    xstem.equals(ystem)
  }

}
