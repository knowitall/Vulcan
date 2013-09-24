package edu.knowitall.vulcan.evidence.query

/**
 * some syntax sugar over Query types to build complex boolean queries
 */
object QueryBuilder {
  
  def fq(field: String, term: String) : Query = {
    term.trim match {
      case "" => EmptyQuery
      case string => FieldQuery(field, string)
    }
  }

  def and(clauses: Query*) : Query = {
    clauses filter { _ != EmptyQuery } match {
      case Nil => EmptyQuery
      case q :: Nil => q
      case qs: Seq[Query] => AndQuery(qs)
    }
  }

  def all(field: String, text: Seq[String]) : Query = {
    and(text map { t => fq(field, t) } : _*)
  }

  def or(clauses: Query*) : Query = {
    clauses filter { _ != EmptyQuery } match {
      case Nil => EmptyQuery
      case q :: Nil => q
      case qs: Seq[Query] => OrQuery(qs)
    }
  }

  def any(field: String, text: Seq[String]) : Query = {
    or(text map { t => fq(field, t) } : _*)
  }

  def not(pred: Query) : Query = {
    NotQuery(pred)
  }
  
  def none(field: String, text: Seq[String]) : Query = {
    and( text map { t => not(fq(field, t)) } : _*)
  }
}
