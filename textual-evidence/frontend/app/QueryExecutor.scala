package app

import java.util.Collection

import edu.knowitall.vulcan.common.Tuple
import edu.knowitall.vulcan.common.Term
import edu.knowitall.vulcan.common.TermsArg
import edu.knowitall.vulcan.common.Relation
import edu.knowitall.vulcan.common.Extraction

import edu.knowitall.vulcan.common.serialization.TupleSerialization._

import edu.knowitall.vulcan.evidence.query.Query
import edu.knowitall.vulcan.evidence.query.FieldQuery
import edu.knowitall.vulcan.evidence.query.AndQuery
import edu.knowitall.vulcan.evidence.query.OrQuery
import edu.knowitall.vulcan.evidence.query.NotQuery
import edu.knowitall.vulcan.evidence.query.EmptyQuery
import edu.knowitall.vulcan.evidence.query.QueryResult
import edu.knowitall.vulcan.evidence.query.QueryResultPage

import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import scala.collection.JavaConverters.collectionAsScalaIterableConverter

import scala.collection.mutable.MutableList

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.concurrent.duration.SECONDS
import ExecutionContext.Implicits.global

import scala.collection.JavaConversions.iterableAsScalaIterable

import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrDocument

import play.api.libs.json.Json
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsError

import java.lang.{Float => JFloat}
import java.lang.{Double => JDouble}
import java.lang.{Iterable => JIterable}

import play.Logger

/**
 * For now just treat the whole SolrDocument as the result.
 * If we ever have a non-Solr backing store, break out this interface, 
 * and define an implementing SolrQueryExecutor.
 */
class QueryExecutor(solrUrl: String) {

  val solr = new HttpSolrServer(solrUrl)

  private def queryToSolrQuery(query: Query) : SolrQuery = {

    def getSolrQueryString(query: Query) : String = {
      query match {
        case FieldQuery(field, text) => s"${field}:${text}"
        case AndQuery(Nil) => ""
        case AndQuery(clauses: Seq[Query]) => 
          clauses map { q => getSolrQueryString(q) } mkString("( ", " AND ", " )")
        case OrQuery(Nil) => ""
        case OrQuery(clauses: Seq[Query]) => 
          clauses map { q => getSolrQueryString(q) } mkString("( ", " OR ", " )")
        case NotQuery(clause: Query) => "NOT ( " + getSolrQueryString(clause) + " )"
        case EmptyQuery => ""
        case other => throw new MatchError("Unknown Query type for " + other)
      }
    }

    val sq = new SolrQuery()
    val queryString = getSolrQueryString(query)
    sq.setQuery(queryString)
    sq.setIncludeScore(true)

    sq
  }

  private def solrDocToQueryResult(doc: SolrDocument) : QueryResult = {

    val tupleJson = doc.getFieldValue("tuple").asInstanceOf[String]
    val tuple = Json.fromJson[Tuple](Json.parse(tupleJson) \ "tuple") match {
      case JsSuccess(tuple, _) => tuple 
      case JsError(error) => sys.error("Failed to parse: " + tupleJson + ": " + JsError.toFlatJson(error))
    }

    val sentence = doc.getFieldValue("sentence").asInstanceOf[String]
    val sentenceDetailsJson = doc.getFieldValue("sentence_details").asInstanceOf[String]
    val sentenceDetails : Seq[Term] = 
      Json.fromJson[Seq[Term]](Json.parse(sentenceDetailsJson)) match {
        case JsSuccess(terms, _) => terms
        case JsError(error) => sys.error("Failed to parse: " + sentenceDetailsJson + ": " + JsError.toFlatJson(error))
      }

    val confidence = doc.getFieldValue("confidence").asInstanceOf[JDouble]
    val id = doc.getFieldValue("id").asInstanceOf[String]
    val corpus = doc.getFieldValue("corpus").asInstanceOf[String]

    val extraction = Extraction(tuple, sentence, sentenceDetails, confidence, id, corpus)

    // right now scoring uses configdence.  This isn't good, but we have to use something as
    val score = doc.getFieldValue("score").asInstanceOf[JFloat].doubleValue()
    
    QueryResult(extraction, score)
  }

  /**
   * Execute the given query and return the results
   */
  def execute(query: Query, start: Int = 0, rows: Int = 10) : QueryResultPage = {

    val solrQuery = queryToSolrQuery(query)
    solrQuery.setStart(start).setRows(rows)
    val solrResults = solr.query(solrQuery).getResults() 

    val results = solrResults.map( 
      doc => solrDocToQueryResult(doc) 
    ).toList.sortWith(
      (x,y) => x.score > y.score
    ) 

    QueryResultPage(results, start, solrResults.getNumFound())
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
  def mergeExecute(queries: Seq[Query], start: Int, rows: Int) : QueryResultPage = {

    // launch solr queries in parallel
    val fResults = 
      for(query <- queries) yield {
          future { execute(query, 0, start + rows) }
        }

    var numResults : Long = 0
    var results = List[QueryResult]()

    // wait for each result, sum counts, and select the first rows of results found

    var pos = 0 // read position in the result stream
    for(fResult <- fResults) {

      // wait for the query future to return
      // TODO handle timeouts
      val page = Await.result(fResult, Duration(10, SECONDS))

      numResults += page.queryTotal
      var index = 0

      while(results.size < rows && index < page.results.length) {
        if(pos >= start) {
          results = results :+ page.results(index)
        }
        pos += 1
        index +=1
      }
    }
    QueryResultPage(results, start, numResults)
  }
}
