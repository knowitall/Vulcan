package edu.knowitall.vulcan.inference.openie

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 8/15/13
 * Time: 9:31 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._

import scala.collection.JavaConversions._
import org.slf4j.LoggerFactory
import org.apache.solr.client.solrj.impl.XMLResponseParser
import org.apache.solr.client.solrj.impl.HttpSolrServer

import org.apache.solr.client.solrj.response.QueryResponse
import scala.Some
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrDocument
import edu.knowitall.vulcan.inference.kb.BinaryRelationTuple
import scala.io.Source




object SolrSearchWrapper {

  val logger = LoggerFactory.getLogger(this.getClass)
  def getInstance(url:String) = {
    println("Solr Url: " + url)
    val server = new HttpSolrServer(url)
    server.setParser(new XMLResponseParser)
    println("Initialized tuples server: " + server.ping())
    new SolrSearchWrapper(server)
  }

  def main(args:Array[String]){
    val wrapper = getInstance(args(0))
    println("Enter Sentence: ")
    for(ln <- Source.fromInputStream(System.in).getLines()){
      println("Searching: " + ln)
      val out = wrapper.searchSentence(ln)
      println("Out: " + out.map(_.tripleFormat()).mkString("\n"))
      println("Enter Sentence: ")
    }
  }
}
class SolrSearchWrapper(server:HttpSolrServer) {

  val logger = LoggerFactory.getLogger(this.getClass)


  def toSentenceQuery(text:String) = {
    val query = new SolrQuery
    query.setQuery("*:*")
    def scrub(string:String) = string//RegexUtils.replaceSpecialCharsExcept(string, ":", " ")
    text.split(" ").map(_.trim).filter(_.size > 0).foreach(word => {
      query.addTermsField("""%s""".format(scrub(word)))
    })
    println("Solr query: " + query.toString)
    query
  }

  def searchSentence(text:String) = {
    toTuples(server.query(toSentenceQuery(text)))
  }


  def toSolrQuery(arg1:String, rel:String, arg2:String):Option[SolrQuery] = {
    val query = new SolrQuery()
    query.setQuery("*:*")
    def scrub(string:String) = string//RegexUtils.replaceSpecialCharsExcept(string, ":", " ")
    if (!arg1.isEmpty) query.addFilterQuery("""arg1:"%s"""".format(scrub(arg1)))
    if (!rel.isEmpty) query.addFilterQuery("""rel:"%s"""".format(scrub(rel)))
    if (!arg2.isEmpty) query.addFilterQuery("""arg2:"%s"""".format(scrub(arg2)))
    if (query.getFilterQueries.size > 0){
      logger.info("Solr Query: " + query.toString)
      Some(query)
    }else{
      None
    }
  }


  def toBinaryRelationTuple(document: SolrDocument) = {
    try {
      val arg1 = document.getFieldValue("arg1").toString
      val rel = document.getFieldValue("rel").toString
      val arg2 = document.getFieldValue("arg2s").toString
      Some(new BinaryRelationTuple(arg1, rel, arg2))
    } catch {
      case e:Exception => {
        logger.error("Failed to convert document to fact: " + document.toString)
        logger.error(e.getStackTrace.map(_.toString).mkString(" "))
        None
      }
    }
  }


  def toTuples(response: QueryResponse) = {
    response.getResults.flatMap(result => toBinaryRelationTuple(result))
  }


  def search(arg1:String, rel:String, arg2:String) = {
    solrQueryResponse(arg1, rel, arg2) match {
      case Some(response:QueryResponse) => {
        Some(toTuples(response))
      }
      case None => {
        logger.info("No response from server.")
        None
      }
    }
  }


  def solrQueryResponse(arg1: String, rel: String, arg2: String)  = {
    toSolrQuery(arg1, rel, arg2) match {
      case Some(query: SolrQuery) => Some(server.query(query))
      case None => {
        logger.error("Failed to create solr query for (%s, %s, %s)".format(arg1, rel, arg2))
        None
      }
    }
  }
}
