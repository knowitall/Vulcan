package controllers

import edu.knowitall.vulcan.query.QueryExecutor
import edu.knowitall.vulcan.query.QueryBuilder._
import edu.knowitall.vulcan.query.EmptyQuery
import edu.knowitall.vulcan.query.QueryResult
import edu.knowitall.vulcan.query.ScoredResult

import play.Logger
import play.api._
import play.api.mvc._

object QueryHandler extends Controller {

  final val PAGE_SIZE = 20

  val executor : QueryExecutor = 
    Play.current.configuration.getString("source.solr.url") match {
      case Some(url) => new QueryExecutor(url)
      case None => {
        Logger.error("Configuration key source.solr.url missing")
        null // ?
      }
    }

  def query = Action { request =>
    val (arg1, rel, arg2, keywords, corpus) = 
       (request.queryString("arg1").mkString(" "),
        request.queryString("rel").mkString(" "),
        request.queryString("arg2").mkString(" "),
        request.queryString("kws").mkString(" "),
        request.queryString("corpus").mkString(" "))

    val page: Int = request.queryString("page").head.toInt

    var queries = Seq(
      and(matchQuery(arg1, rel, arg2), keywordQuery(keywords)),
      and(partialMatchQuery(arg1, rel, arg2), keywordQuery(keywords)),
      keywordQuery(s"$arg1 $rel $arg2 $keywords"))

    val results : QueryResult = s"$arg1 $rel $arg2 $keywords".trim match {
      case "" => QueryResult.EMPTY
      case _ => {
        val corpusFilter = fq("corpus", corpus)
        executor.mergeExecute(queries map { q => and(q, corpusFilter)}, page * PAGE_SIZE, PAGE_SIZE)
      }
    }
/*
    val results : QueryResult = q match {
      case EmptyQuery => QueryResult.EMPTY
      case _ => {
        q = and(q, fq("corpus", corpus))
        q.setStart(page * PAGE_SIZE)
        q.setRows(PAGE_SIZE)

        Logger.info("executing " + q.getQuery())

        executor.execute(q)
      }
    }
*/

    Ok(views.html.queryResults(results))
  }
}
