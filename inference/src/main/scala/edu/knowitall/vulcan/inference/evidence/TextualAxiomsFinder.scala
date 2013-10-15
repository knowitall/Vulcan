package edu.knowitall.vulcan.inference.evidence

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/24/13
 * Time: 10:40 PM
 * To change this template use File | Settings | File Templates.
 */


import edu.knowitall.vulcan.common.{Extraction, Tuple}
import edu.knowitall.vulcan.evidence.TextualEvidenceClient
import edu.knowitall.vulcan.evidence.query.TupleQuery
import edu.knowitall.vulcan.inference.proposition.Proposition
import edu.knowitall.vulcan.inference.kb.{Axiom, Predicate}
import org.slf4j.LoggerFactory
import edu.knowitall.vulcan.inference.utils.TupleHelper
import edu.knowitall.vulcan.inference.mln.tuffyimpl.TuffyFormatter
import scala.collection.immutable.HashMap
import edu.knowitall.vulcan.inference.entailment.EntailmentScorer

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



class TextualAxiomsFinder(endpoint:String,
                          numTuples:Int) extends AxiomsFinder{

  val logger = LoggerFactory.getLogger(this.getClass)

  val client = new TextualEvidenceClient(endpoint)

  val corpusQuery = TupleQuery.corpusQuery(Seq("glossary", "studyguide", "clueweb"))

  val scorer = new EntailmentScorer

  def fieldQueries(tuple:Tuple) = {
    val arg1 = tuple.arg1.text
    val rel = tuple.rel.text
    val arg2 = tuple.arg2s.map(_.text).mkString(" ")
    tuple::
    TupleHelper.from(arg1, rel, "")::
    TupleHelper.from(arg1, "", arg2)::
    TupleHelper.from("", rel, arg2)::Nil

  }

  def distinct(axioms:Seq[Axiom]) = {
    var map = new HashMap[String, Axiom]
    axioms.foreach(x => {
      val key = x.consequent.tuple.text
      if(!map.contains(key)){
        map += key -> x
      }
    })
    map.values.toSeq.sortBy(-_.score)
  }

  def find(proposition:Proposition) = {
    //val queries = fieldQueries(proposition.consequent.tuple).map(QueryBuilder.and(TupleQuery.partialMatchQuery()))
    val propTuple = proposition.consequent.tuple
    val variants = TupleHelper.wildcardVaiants(propTuple)
    distinct(find(propTuple))// ++ variants.flatMap(variant => find(variant)))
  }


  def sentenceid(extraction:Extraction) = {
    logger.info("%s\t%s".format(extraction.sentence, extraction.tuple))
    extraction.sentence
  }

  def find(propTuple: Tuple): Seq[Axiom] = {
    val query = TupleQuery.matchAnyQuery(propTuple)//QueryBuilder.and(TupleQuery.matchAnyQuery(propTuple), corpusQuery)
    logger.info("Query: " + query)
    val resultsPage = client.query(query, start = 0, rows = numTuples)
    logger.info("Number of results: " + resultsPage.results.size)
    import Axiom._
    resultsPage.results
      .sortBy(-_.score)
      .groupBy(result => sentenceid(result.extraction))
      .flatMap(resultsGrp => {
      val sid = resultsGrp._1
      val results = resultsGrp._2
      val average = if(results.size > 0) results.map(result => {
        0.25 * scorer.scoreText(result.extraction.tuple, propTuple)
        + 0.75 * scorer.scoreText(result.extraction.sentence, propTuple.text)
      }).sum/results.size.toDouble else 0.0
      results.map(result=> {
        val axiom = fromPredicate(Predicate(result.extraction.tuple, average), average)
        //logger.info("%s\t%s = %.4f".format(sid, TuffyFormatter.exportRule(axiom, withWeights=true, withQuotes = false), axiom.score()))
        axiom
      })
    }).toSeq
  }
}
