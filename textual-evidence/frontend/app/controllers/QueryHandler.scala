package controllers

import app.QueryExecutor
import edu.knowitall.vulcan.evidence.query.QueryRequest

import play.Logger
import play.api._
import play.api.libs.json._
import play.api.mvc._

object QueryHandler extends Controller {

  val executor : QueryExecutor = 
    Play.current.configuration.getString("source.solr.url") match {
      case Some(url) => new QueryExecutor(url)
      case None => {
        Logger.error("Configuration key source.solr.url missing")
        null // ?
      }
    }

  def query = Action(parse.json) { request =>

    val query = request.body.validate[QueryRequest] match {
      case JsSuccess(queryRequest, _) => queryRequest
      case JsError(errors) => sys.error("Not ok: " + JsError.toFlatJson(errors))
    }

    // TODO: check pagination params

    val resultPage = executor.execute(query.query, query.start, query.rows)

    Ok(Json.toJson(resultPage))
  }
}
