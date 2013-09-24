package edu.knowitall.vulcan.evidence.query

/*
 * Basic request type for query api: a query, and pagination parameters
 */
case class QueryRequest(query: Query, start: Int, rows: Int)

/*
 * Define json serialization for type QueryRequest
 */
object QueryRequest {

  import play.api.libs.json._

  implicit val reader = Json.reads[QueryRequest]
  implicit val writer = Json.writes[QueryRequest]
}
