package edu.knowitall.vulcan.evidence.query

import edu.knowitall.vulcan.common.Tuple
import edu.knowitall.vulcan.common.TermsArg
import edu.knowitall.vulcan.common.Term
import edu.knowitall.vulcan.common.Arg

import QueryBuilder._

/** 
 * Builds a simple query matching the bare strings of the terms in the tuple against
 * fields arg1, rel, and arg2.
 */
object TupleQuery {

  /**
   * A Query that returns Tuples where every term in each field in the query 
   * Tuple exists in the corresponding field in the result Tuple.
   *
   * Empty fields in the query Tuple are unconstrained, i.e. ("fish", "eat", "")
   * will find Tuples where arg1 contains "fish" and rel contains "eat" --- arg2
   * may be anything.
   */
  def matchQuery(tuple: Tuple) : Query = {
    
    and(and(arg1FieldQueries(tuple) : _*),                    // AND across arg1s
        and(fieldQueriesForTerms("rel", tuple.rel.terms) : _*), // AND across rels 
        and(arg2FieldQueries(tuple) : _*))                    // AND across arg2s
  }

  /**
   * A Query that returns Tuples where at least one term in each field in the 
   * query Tuple exists in the corresponding field in the result Tuple.
   */
  def partialMatchQuery(tuple: Tuple) : Query = {

    and(or(arg1FieldQueries(tuple):_*),                    // OR across arg1s
        or(fieldQueriesForTerms("rel", tuple.rel.terms):_*), // OR across rels 
        or(arg2FieldQueries(tuple):_*))                    // OR all arg2s
  }

  /**
   * A Query that returns Tuples where at leat one term from anywhere in the 
   * query Tuple exists in any field in the result Tuple
   */
  def matchAnyQuery(tuple: Tuple) : Query = {
    val terms: Seq[Term] = 
      argTerms(tuple.arg1) ++
      tuple.rel.terms ++
      { tuple.arg2s map { arg => argTerms(arg) } flatten }

    or(fieldQueriesForTerms("text", terms):_*)
  }

  /**
   * A Query that returns Tuples from the given corpora.
   */
  def corpusQuery(corpora: Seq[String]) {
    any("corpus", corpora)
  }

  private def argTerms(arg: Arg) : Seq[Term] = {
    arg match {
      case ta: TermsArg => ta.terms
      case _ => sys.error("Nested tuples aren't supported yet")
    }
  }

  private def arg1FieldQueries(tuple: Tuple) : Seq[Query] = {
    fieldQueriesForTerms("arg1", argTerms(tuple.arg1))
  }

  private def arg2FieldQueries(tuple: Tuple) : Seq[Query] = {
    val terms = tuple.arg2s flatMap { arg => argTerms(arg) }
    fieldQueriesForTerms("arg2s", terms)
  }

  private def fieldQueriesForTerms(field: String, terms: Seq[Term]) : Seq[Query] = {
    terms map { term => fq(field, term.text) }
  }
}
