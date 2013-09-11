package edu.knowitall.vulcan.query

import scala.collection.mutable.MutableList

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.concurrent.duration.SECONDS
import ExecutionContext.Implicits.global

import scala.collection.JavaConversions.iterableAsScalaIterable

import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrDocument

import scopt.immutable.OptionParser

import java.lang.{Float => JFloat}
import java.lang.{Double => JDouble}
import java.lang.{Iterable => JIterable}

import org.slf4j.LoggerFactory

/**
 * For now just treat the whole SolrDocument as the result.
 */
class QueryExecutor(solrUrl: String, scorer : Scorer = Scorer()) {

  val solr = new HttpSolrServer(solrUrl)

  /**
   * Execute the given query and return the results
   */
  def execute(query: Query) : QueryResult = {

    val results = solr.query(query).getResults() 

    val scoredResults = (for(result <- results) yield scorer.score(query, result))

    QueryResult(scoredResults.toList.sortWith((x,y) => x.score > y.score), 
                results.getNumFound())
  }

  /**
   * Execute all the provided queries and combine their results linearly, i.e.
   * all results from the first query will be returned before results from the
   * second query, etc., regardless of the relative scores of the results across
   * the different queries.  
   * 
   * This supports the notion of a single logical query whose results are the
   * concatenated results of a hierarchy of other queries.
   *
   * Note that doing this may give duplicate results.
   *
   * We currently execute all queries in parallel, reading back many more
   * results than we need.  The alternative is to execute queries serially, only
   * reading back exactly the results we know we need.  Not sure which trade off
   * is better to make.
   *
   * Note that pagination parameters set on the given subqueries are ignored.
   */
  def mergeExecute(queries: Seq[Query], start: Int, pageSize: Int) : QueryResult = {

    // launch solr queries in parallel
    val fResults = 
      for(query <- queries) yield {
          query.setStart(0).setRows(start + pageSize)
          future { execute(query) }
        }

    var numResults : Long = 0
    var results = List[ScoredResult]()

    // wait for each result, sum counts, and select the first pageSize results found

    var pos = 0 // read position in the result stream
    for(fResult <- fResults) {

      // wait for the query future to return
      val partial = Await.result(fResult, Duration(10, SECONDS))

      numResults += partial.numResults
      var index = 0

      while(results.size < pageSize && index < partial.results.size) {
        if(pos >= start) {
          results = results :+ partial.results(index)
        }
        pos += 1
        index +=1
      }
    }

    QueryResult(results, numResults)
  }
}

/**
 * Wrap up query result details.  
 * Initially some page of scored results and a count of total results.
 */
case class QueryResult(results: List[ScoredResult], numResults: Long) { }
object QueryResult {
  val EMPTY = QueryResult(List[ScoredResult](), 0)
}

/*
 * Do dumb scoring.  
 * This will get a whole lot more complex; probably end up in it's own package.
 */
case class Scorer() {

  def score(query: Query, result: SolrDocument) : ScoredResult = {
    val score = result.get("confidence").asInstanceOf[JDouble]
    new ScoredResult(result, score)
  }
}

/**
 * Ultimately the result will be a Tuple, but until that's well defined just use the Solr document
 */
case class ScoredResult(result: SolrDocument, score: Double)  {
  
  val corpus = result.get("corpus").toString
  val sentence = result.get("sentence_text").toString
  val arg1 = result.get("arg1").toString
  val rel  = result.get("rel").toString
  val arg2s = {
    val arg2sCollection = result.get("arg2s").asInstanceOf[JIterable[String]]
    if(arg2sCollection == null) {
      ""
    } else {
      arg2sCollection.mkString(" ")
    }
  }

  override def toString() = {

    s"$arg1 / $rel / $arg2s" // + : $score"
  }
}
