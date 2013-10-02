package edu.knowitall.vulcan.evidence.query

import edu.knowitall.vulcan.common.Term
import edu.knowitall.vulcan.common.Arg
import edu.knowitall.vulcan.common.TermsArg
import edu.knowitall.vulcan.common.Tuple

import edu.knowitall.vulcan.common.Path

import QueryBuilder._

/** 
 * Simple helper for turning Path's into queries over tuples
 */
object TupleQuery {
  
  /**
   * Returns a query that will look for the tuples that contain the given
   * term under the optional path.  If the path isn't specified, it will
   * match at any position in the Tuple.
   */
  def forTerm(term: Term, path: Path = Path.Empty) : Query = {
    val textQuery = forPath(Path.forTerm(term.text, path))
    term.lemma match {
      case Some(lemma) => or(textQuery, forPath(Path.forTerm(lemma, path)))
      case None => textQuery
    }
  }

  /**
   * Returns a query that will look for the tuples that contain the given
   * text under the optional path.  If the path isn't specified, it will
   * match at any position in the Tuple.
   */
  def forText(text: String, path: Path = Path.Empty) : Query = {
    forTerm(Term(text), path)
  }

  /**
   * Returns a query that will search for Tuples matching the given path.  This
   * is primarily a helper method, you probably want to use forTerm or forText
   */
  def forPath(path: Path) : Query = {
    /*
     * If the path is rooted, adding a * coerces solr into simplified scoring 
     * that we actually prefer.  
     * If the path isn't rooted, then we have to add a star to be able to find 
     * un-rooted matches.  So, we always add a *.
     */
    fq("tuple", (path / Path.Star).pathString) 
  }

  /**
   * Returns a query that will match Tuples extracted from any of the given
   * corpora.
   */
  def corpusQuery(corpora: Seq[String]): Query = {
    any("corpus", corpora)
  }

  /**
   * Returns a query that will search for Tuples matching the given terms under
   * the given path, where all Terms' queries are combined using the given 
   * combine method (and or or).
   */
  def forTerms(terms: Seq[Term], path: Path, combine: (Query*) => Query) : Query = {
    combine(terms.map(term => forTerm(term, path)) : _*)
  }

  /**
   * Query for tuples recursively matching all the terms in the given Tuple.
   */
  def matchQuery(tuple: Tuple) : Query = {
    and(forArg(tuple.arg1, Path.Arg1, and),
        forTerms(tuple.rel.terms, Path.Rel, and),
        and(tuple.arg2s.map(arg2 => forArg(arg2, Path.Arg2, and)) : _*))
  }

  /**
   * Query for tuples whose arg1, rel, arg2s recursively match some of the given 
   * Tuple's arg1, rel, and arg2 respectively.
   */
  def partialMatchQuery(tuple: Tuple) : Query = {

    val arg1q = tuple.arg1 match {
      case terms: TermsArg => forTerms(terms.terms, Path.Empty, or)
      case tupleArg: Tuple => partialMatchQuery(tupleArg)
    }

    val relq = forTerms(tuple.rel.terms, Path.Rel, or)

    val arg2qs = for(arg2 <- tuple.arg2s) yield arg2 match {
      case terms: TermsArg => forTerms(terms.terms, Path.Empty, or)
      case tupleArg: Tuple => partialMatchQuery(tupleArg)
    }

    val arg2q = or(arg2qs: _*)

    and(arg1q, relq, arg2q)
  }

  /**
   * Query for tuples whose terms match anywhere against the given Tuples terms
   */
  def matchAnyQuery(tuple: Tuple) : Query = {
    val arg1q = tuple.arg1 match {
      case terms: TermsArg => forTerms(terms.terms, Path.Empty, or)
      case tupleArg: Tuple => matchAnyQuery(tupleArg)
    }

    val relq = forTerms(tuple.rel.terms, Path.Empty, or)

    val arg2qs = for(arg2 <- tuple.arg2s) yield arg2 match {
      case terms: TermsArg => forTerms(terms.terms, Path.Empty, or)
      case tupleArg: Tuple => matchAnyQuery(tupleArg)
    }

    val arg2q = or(arg2qs: _*)

    or(arg1q, relq, arg2q)
  }

  /**
   * Query for tuples matching the given Tuple, undern the given path, with all
   * subqueries combined using the given combine method, default to or
   *
   * This is a reasonable general purpose query.
   */
  def forTuple(tuple: Tuple, path: Path = Path.Empty, combine: (Query*) => Query = or) : Query = {
    combine(forArg(tuple.arg1, Path.Arg1 / path, combine),
     forTerms(tuple.rel.terms, Path.Rel / path, combine),
     combine(tuple.arg2s.map(arg2 => forArg(arg2, Path.Arg2 / path, combine)) : _*))
  }

  /**
   * Query for tuples matching the given Arg, which may be either a Tuple
   * or a TermsArg, under the given path, combining subqueries with the
   * given combine method.
   */
  def forArg(arg: Arg, path: Path, combine: (Query*) => Query) : Query = {
    arg match {
      case terms: TermsArg => forTerms(terms.terms, path, combine)
      case tuple: Tuple => forTuple(tuple, path, combine)
    }
  }
}
