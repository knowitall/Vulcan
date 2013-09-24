package edu.knowitall.vulcan.evidence.query

/**
 * A Seq of QueryResults returned from a query, with pagination information.
 */
case class QueryResultPage(results: Seq[QueryResult], index: Int, queryTotal: Long)

/*
 * Define json serialization for type QueryResultPage, and a predefined empty page
 */
object QueryResultPage {

  import play.api.libs.json._

  val EMPTY = QueryResultPage(Seq[QueryResult](), 0, 0)

  implicit val reader = Json.reads[QueryResultPage]
  implicit val writer = Json.writes[QueryResultPage]
}
