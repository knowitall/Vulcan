package edu.knowitall.vulcan.inference.utils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/28/13
 * Time: 10:21 AM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._

import edu.washington.cs.knowitall.morpha.MorphaStemmer
import scala.collection.mutable
import scala.util.Random



object Tokenizer {

  val filter = DefaultContentWordsFilter.getFilter

  val stemmer = new MorphaStemmer

  def stem(string:String) = if(string.equals("thicker")) "thick" else if(string.equals("warmer")) "warm" else MorphaStemmer.stem(string)

  val tokenizationCache = new mutable.HashMap[String, Seq[String]]
  def tokenize(string:String) = {
    if (tokenizationCache.size > 10000){
      Random.shuffle(tokenizationCache.keys.seq).take(1000).foreach(string => tokenizationCache.remove(string))
    }
    tokenizationCache.getOrElseUpdate(string, tokenizeNoCache(string))
  }
  def tokenizeNoCache(string:String):Seq[String] = {
    StringUtil.removeNonAlphaNumeric(string)
      .toLowerCase
      .split(" ")
      .filter(word => !filter.isStopword(word)) // Note stemmed version might be a stopped word
      .map(word => stem(word))//Stemmer.normalize(word))
      .filter(word => !filter.isStopword(word) && word.trim.size > 0)
  } //Doing check again.
  //.filter(word => !StopwordsFilter.isStopword(word))

  def lemmatizeString(string:String) = tokenize(string).mkString(" ")
}
