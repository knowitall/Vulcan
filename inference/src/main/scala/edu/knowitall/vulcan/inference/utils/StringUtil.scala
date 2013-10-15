package edu.knowitall.vulcan.inference.utils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/28/13
 * Time: 10:23 AM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._

object StringUtil {
  val notAlphaRe = """[^a-zA-Z ]""".r
  def isNotAlphabetic(text: String) = notAlphaRe.findFirstMatchIn(text) != None
  def removeContentWithinParentheses(text:String) = text.replaceAll("""\(.*?\)""", " ")
  def removeNonAlphaNumeric(text:String) = text.replaceAll("""[^0-9a-zA-Z ?]""", " ")
}
