package edu.knowitall.vulcan.evidence

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.Await

import edu.knowitall.vulcan.evidence.query._

import dispatch._

import play.api.libs.json._

class TextualEvidenceClient(endpoint: String) {

  def query(query: Query, start: Int = 0, rows: Int = 10) : QueryResultPage = {
    
    val queryRequest = QueryRequest(query, start, rows)

    val requestJson = Json.stringify(Json.toJson(queryRequest))

    val post = url(endpoint).POST.
                addHeader("Content-Type", "application/json").
                setBody(requestJson)
    
    val req = Http(post)
    val resp = Await.result(req, Duration.Inf)
    
    val body = resp.getResponseBody()

    val json = Json.parse(body)

    val resultPage = Json.fromJson[QueryResultPage](json) match {
      case JsSuccess(parsed, _) => parsed
      case JsError(errors) => sys.error("Invalid response: " + JsError.toFlatJson(errors))
    }

    resultPage
  }
}
