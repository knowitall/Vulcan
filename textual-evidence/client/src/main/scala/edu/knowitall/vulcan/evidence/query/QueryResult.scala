package edu.knowitall.vulcan.evidence.query

import edu.knowitall.vulcan.common.Extraction

/**
 * A QueryResult is pretty minimal, just an Extraction with a score.
 */
case class QueryResult(extraction: Extraction, score: Double)

/**
 * Define json serialization for type QueryResult
 */
object QueryResult {

  import play.api.libs.json._
  import edu.knowitall.vulcan.common.serialization.TupleSerialization._
  import edu.knowitall.vulcan.common.serialization.ExtractionSerialization._

  implicit val reader = Json.reads[QueryResult]
  implicit val writer = Json.writes[QueryResult]
}
