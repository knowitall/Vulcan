package edu.knowitall.vulcan.query

import edu.knowitall.vulcan.query.QueryBuilder._

import scala.collection.JavaConversions.iterableAsScalaIterable

import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrDocument

import scopt.immutable.OptionParser

import java.lang.{Float => JFloat}
import java.lang.{Double => JDouble}
import java.lang.{Iterable => JIterable}

import org.slf4j.LoggerFactory

object QueryCLI extends App {

  val logger = LoggerFactory.getLogger(this.getClass)

  case class Config(val solrUrl: String = "",
                    val arg1: String = "",
                    val rel: String = "",
                    val arg2: String = "",
                    val keywords: String = "")

  def parseArgs(args: Array[String]) : Config = {

    val parser = new OptionParser[Config]("QueryCLI") {
      def options = Seq(
        opt("solr-url", "(required) solr endpoint to query against") { 
          (s: String, c: Config) => c.copy(solrUrl = s) },
        opt("arg1", "arg1") { 
          (s: String, c: Config) => c.copy(arg1 = s) },
        opt("rel", "rel") { 
          (s: String, c: Config) => c.copy(rel = s) },
        opt("arg2", "arg2") { 
          (s: String, c: Config) => c.copy(arg2 = s) },
        opt("keywords", "keywords") { 
          (s: String, c: Config) => c.copy(keywords = s) }
      )}

    val config = parser.parse(args, Config())
    config match {
      case Some(c) =>
        if(c.solrUrl.equals("")) {
          parser.showUsage
          sys.exit()
        } else {
          if(Seq(c.arg1, c.rel, c.arg2, c.keywords).mkString.isEmpty()) {
            System.err.println("At least one of --arg1, --rel, --arg2, and --keywords is required")
            parser.showUsage
            sys.exit()
          } 
          c
        }
      case None => sys.exit()
    }
  }

  val c = parseArgs(args) 
  val (solrUrl: String, 
       arg1: String, 
       rel: String, 
       arg2: String, 
       keywords: String) = 
    (c.solrUrl, c.arg1, c.rel, c.arg2, c.keywords)

  val executor = new QueryExecutor(c.solrUrl, new Scorer())

  println("keywords: " + keywords)
  val matchQ = and(matchQuery(arg1, rel, arg2), keywordQuery(keywords))
  val partialMatchQ = and(partialMatchQuery(arg1, rel, arg2), keywordQuery(keywords))
  val keywordQ = keywordQuery(s"$arg1 $rel $arg2 $keywords")

  println(s"Queries for ( $arg1 ; $rel ; $arg2 )")

/*
  for(q <- Seq(matchQ, partialMatchQ, keywordQ);
      result <- { println(s"\n $q\n Results:") ; 
                  executor.e(q).results
                })
  {
*/
  for(result <- executor.mergeExecute(Seq(matchQ, partialMatchQ, keywordQ), 0, 100).results) {
    println(s"  $result")
  }
}
