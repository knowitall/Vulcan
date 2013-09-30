package edu.knowitall.vulcan.inference.utils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/28/13
 * Time: 10:22 AM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import org.slf4j.LoggerFactory
import scala.collection.immutable.HashSet
import scala.io.Source

object DefaultContentWordsFilter {

  def getFilter = {
    new ContentWordsFilter(this.getClass.getResource("/stopwords.txt").getPath)
  }

  def main(args:Array[String]){
    val filter = DefaultContentWordsFilter.getFilter
    var words = "a"::"an"::"at"::"where"::"Niranjan"::Nil
    words.foreach(word => println("%s is stopword = %s".format(word, filter.isStopword(word))))
    val sentence = "This is a test of a stopword removal tool."
    words = sentence.split(" ").toList
    val rwords = filter.removeStopwords(words)
    println("sentence: " + sentence)
    println("stopped : " + rwords.mkString(" "))


  }

}

class ContentWordsFilter {

  val logger = LoggerFactory.getLogger(this.getClass)

  var stopwords = new HashSet[String]

  def loadStopwords(stopwordsFile: String){
    try{
      Source.fromFile(stopwordsFile)
        .getLines()
        .foreach(line => stopwords += line.trim)
    }catch{
      case e:Exception => logger.error("Failed to load stopwords from file: " + stopwordsFile)
    }
    stopwords += "<ANSWER>"

  }

  def this(stopwordsFile:String) {
    this()
    loadStopwords(stopwordsFile)
  }

  def isStopword(word:String) = stopwords.contains(word)

  def removeStopwords(words:Iterable[String]) = words.filter(word => !isStopword(word))

}
