package controllers

import edu.knowitall.vulcan.common.Tuple
import edu.knowitall.vulcan.evidence.query.Query
import edu.knowitall.vulcan.evidence.query.TupleQuery
import edu.knowitall.vulcan.evidence.query.EmptyQuery
import edu.knowitall.vulcan.evidence.query.QueryResultPage
import edu.knowitall.vulcan.evidence.query.QueryBuilder._

import play.Logger
import play.api._
import play.api.mvc._

object WebQueryHandler extends Controller {

  final val PAGE_SIZE = 20

  def query = Action { request =>
    
    val (arg1, rel, arg2, keywords, corpus) = 
       (request.queryString("arg1").mkString(" "),
        request.queryString("rel").mkString(" "),
        request.queryString("arg2").mkString(" "),
        request.queryString("kws").mkString(" "),
        request.queryString("corpus").mkString(" "))

    val page: Int = request.queryString("page").head.toInt
    val tuple: Tuple = Tuple.makeTuple(arg1, rel, arg2)
    val keywordQuery = all("tuple", keywords.split("\\s+"))
    val corpusFilter = fq("corpus", corpus)

    var queries = Seq(
      and(TupleQuery.matchQuery(tuple), keywordQuery),
      and(TupleQuery.partialMatchQuery(tuple), keywordQuery),
      and(TupleQuery.matchAnyQuery(tuple), keywordQuery),
      keywordQuery) .
        filter { _ != EmptyQuery } . distinct .
        map { q => and(q, corpusFilter) }

    val results : QueryResultPage = queries match {
      case Seq(EmptyQuery) => QueryResultPage.EMPTY
      case _ => QueryHandler.executor.mergeExecute(queries,
                                                   page * PAGE_SIZE, 
                                                   PAGE_SIZE)
    }

    Ok(views.html.queryResults(results))
  }
}
