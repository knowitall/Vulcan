package edu.knowitall.vulcan.inference.evidence

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/24/13
 * Time: 10:40 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import edu.knowitall.vulcan.common.{Arg, Tuple}
import edu.knowitall.vulcan.evidence.TextualEvidenceClient
import edu.knowitall.vulcan.evidence.query.{TupleQuery, QueryBuilder}
import edu.knowitall.vulcan.inference.proposition.Proposition
import edu.knowitall.vulcan.inference.kb.{WeightedRule, Axiom, Predicate}
import org.slf4j.LoggerFactory
import edu.knowitall.vulcan.inference.utils.TupleHelper

object TextualAxiomsFinder{

  def main(args:Array[String]){
    if(args.size > 0){
      val finder = new TextualAxiomsFinder(args(0), numTuples=100)
      finder.find(new Proposition(Seq[Predicate](), Predicate(Tuple.makeTuple("metal", "conductor", "electricity"), 1.0)))
    }else{
      println("Usage: " + TextualAxiomsFinder.getClass.getCanonicalName + " <te client url>")
    }
  }
}

class TextualAxiomsFinder(endpoint:String, numTuples:Int) extends AxiomsFinder{

  val logger = LoggerFactory.getLogger(this.getClass)

  val client = new TextualEvidenceClient(endpoint)

  val corpusQuery = TupleQuery.corpusQuery(Seq("glossary", "studyguide", "clueweb"))

  def fieldQueries(tuple:Tuple) = {
    val arg1 = tuple.arg1.text
    val rel = tuple.rel.text
    val arg2 = tuple.arg2s.map(_.text).mkString(" ")
    tuple::
    TupleHelper.from(arg1, rel, "")::
    TupleHelper.from(arg1, "", arg2)::
    TupleHelper.from("", rel, arg2)::Nil

  }

  def find(proposition:Proposition) = {
    //val queries = fieldQueries(proposition.consequent.tuple).map(QueryBuilder.and(TupleQuery.partialMatchQuery()))
    val propTuple = proposition.consequent.tuple
    val variants = TupleHelper.wildcardVaiants(propTuple)
    find(propTuple) ++ variants.flatMap(variant => find(variant))
    //Axiom.fromTuple(Tuple.makeTuple("metal", "conductor of", "electricity"))::Nil ++
    //predicates.map(new Axiom(Seq[Predicate](), _, 1.0))

  }


  def find(propTuple: Tuple): Seq[Axiom] = {
    val query = QueryBuilder.and(TupleQuery.matchAnyQuery(propTuple), corpusQuery)
    logger.info("Query: " + query)
    val resultsPage = client.query(query, start = 0, rows = numTuples)

    import Axiom._
    resultsPage.results
      .sortBy(-_.score)
      .map(result => fromPredicate(Predicate(result.extraction.tuple, result.score), result.score))
  }
}
