package edu.knowitall.vulcan.query

import edu.knowitall.vulcan.query.Stopwords.isStopword

import scala.collection.immutable.StringOps

import org.apache.solr.client.solrj.SolrQuery

/**
 * QueryBuilder provides functionality for constructing boolean queries that can be converted to
 * SolrQuerys to execute.  It defines basic and/or/not building blocks, as well as some helper query
 * types particular to querying triples from our schema.
 */
object QueryBuilder {

  /**
   * Genehates a Query that matches all terms in each parameter (split on spaces) against the
   * corresponding field in the indexed tuples in the solr index.  
   *
   * Providing an empty String for an paramter means that field is unconstrained, limiting
   * the query to match against only the non-empty arguments.
   */
  def matchQuery(arg1: String, rel: String, arg2: String) = {
    and(and((for(t <- getTerms(arg1)) yield fq("arg1", t)) : _*),
        and((for(t <- getTerms(rel)) yield fq("rel", t)):_*),
        and((for(t <- getTerms(arg2)) yield fq("arg2s", t)):_*))
  }

  /**
   * Generates a Query that matches any term in each parameter (split on spaces) against the
   * corresponding field in the indexed tuples in the solr index.
   * 
   * Providing an empty String for an paramter means that field is unconstrained, limiting
   * the query to match against only the non-empty arguments.
   */
  def partialMatchQuery(arg1: String, rel: String, arg2: String) = {
    and(or((for(t <- getTerms(arg1)) yield fq("arg1", t)):_*),
        or((for(t <- getTerms(rel)) yield fq("rel", t)):_*),
        or((for(t <- getTerms(arg2)) yield fq("arg2s", t)):_*))
  }

  /**
   * Generates a Query that matches any given term (split on spaces) against all fields
   * in the indexed tuples in the solr index.
   */
  def keywordQuery(keywords: String) = {
    or((for(t <- getTerms(keywords)) yield fq("text", t)):_*)  
  }

  /**
   * Generates an or-conjunction of the given Query terms, or an EmptyQuery if the given terms are empty
   */
  def or(terms: Query*) : Query = {

    val filtered = terms filter (t => t != EmptyQuery) 
    filtered.size match {
      case 0 => EmptyQuery
      case 1 => filtered.head
      case _ => OrQuery(filtered)
    }
  }

  /**
   * Generates an and-conjunction of the given Query terms, or an EmptyQuery if the given terms are empty
   */
  def and(terms: Query*) : Query = {

    val filtered = terms filter (t => t != EmptyQuery) 
    filtered.size match {
      case 0 => EmptyQuery
      case 1 => filtered.head
      case _ => AndQuery(filtered)
    }
  }

  /**
   * Negates the given Query.  Negating an EmptyQuery results and EmptyQuery
   */
  def not(term: Query) : Query = {
    term match {
      case EmptyQuery => EmptyQuery
      case _ => NotQuery(term)
    }
  }

  /**
   * Generates a Query for the given term on the given field.  This is the basic Query building
   * block --- all Query objects are composed of field queries.
   */
  def fq(field: String, term: String) : Query = {
    if(term.trim.isEmpty) EmptyQuery
    else new FieldQuery(field, term)
  }

  /**
   * Splits the given text fragment on spaces.  
   * This does some basic stopword removal now.  
   * It will probably become fancier.  
   */
  def getTerms(fragment: String) : Seq[String] = {
    val terms = fragment.split(' ') 
    val minusStopwords = for(t <- terms; if !isStopword(t)) yield t.trim
    if(minusStopwords.size > 0) {
      minusStopwords
    } else {
      // if the fragment was entirely stopwords, don't filter down to nothing
      terms
    }
  }
}

/**
 * Query defines an interface for generating query strings in solr syntax, and a helper method for
 * generating an actual SolrQuery instance.
 */
abstract class Query extends SolrQuery {

  def getSolrQueryText() : String

  setQuery(getSolrQueryText())
  setIncludeScore(true)

  /**
   * An annotated query text for human consumption.
   */
  override def toString() = s"${this.getClass.getSimpleName}: ${getSolrQueryText()}"
}

/**
 * Case classes define the Solr syntax for the different kinds of Queries.
 */
case class FieldQuery(field: String, term: String) extends Query {
  def getSolrQueryText() = {
    s"${field}:${term}"
  }
}

case class AndQuery(terms: Seq[Query]) extends Query {
  def getSolrQueryText() = {
    terms.map(_.getSolrQueryText).mkString("( ", " AND ", " )")
  }
}

case class OrQuery(terms: Seq[Query]) extends Query {
  def getSolrQueryText() = {
    terms.map(_.getSolrQueryText).mkString("( ", " OR ", " )")
  }
}

case class NotQuery(term: Query) extends Query {
  def getSolrQueryText() = {
    s"( NOT ${term.getSolrQueryText} )"
  }
}

case object EmptyQuery extends Query {
  def getSolrQueryText = ""
}
