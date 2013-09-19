package edu.knowitall.vulcan.inference.matching

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/11/13
 * Time: 11:59 AM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._

class Wordnet {

  val logger = LoggerFactory.getLogger(this.getClass)

  def synonyms(word:String) = word::Nil

  def areSynonyms(x:String, y:String) = x.equalsIgnoreCase(y)

}
