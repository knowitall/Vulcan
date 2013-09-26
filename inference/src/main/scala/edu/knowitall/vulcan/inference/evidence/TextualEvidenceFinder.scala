package edu.knowitall.vulcan.inference.evidence

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/24/13
 * Time: 10:40 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import edu.knowitall.vulcan.common.Tuple
import edu.knowitall.vulcan.evidence.TextualEvidenceClient
import edu.knowitall.vulcan.evidence.query.{TupleQuery, QueryBuilder}
import edu.knowitall.vulcan.inference.proposition.Proposition
import edu.knowitall.vulcan.inference.kb.{WeightedRule, Axiom, Predicate}
import org.slf4j.LoggerFactory

object TextualEvidenceFinder{

  def main(args:Array[String]){
    if(args.size > 0){
      val finder = new TextualEvidenceFinder(args(0))
      finder.find(new Proposition(Seq[Predicate](), Predicate(Tuple.makeTuple("metal", "conductor", "electricity"), 1.0)))
    }else{
      println("Usage: " + TextualEvidenceFinder.getClass.getCanonicalName + " <te client url>")
    }
  }
}

class TextualEvidenceFinder(endpoint:String) extends EvidenceFinder{

  val logger = LoggerFactory.getLogger(this.getClass)

  val client = new TextualEvidenceClient(endpoint)

  val corpusQuery = TupleQuery.corpusQuery(Seq("glossary", "studyguide", "clueweb"))



  def find(proposition:Proposition) = {
    val query = QueryBuilder.and(TupleQuery.matchQuery(proposition.consequent.tuple), corpusQuery)
    logger.info("Query: " + query)
    val resultsPage = client.query(query)
    val predicates = resultsPage.results.map(result => Predicate(result.extraction.tuple, 1.0))

    predicates.isEmpty match {
      case false => {
        val axioms = predicates.map(pred => new Axiom(Seq[Predicate](), pred, 1.0))
        println("Number of axioms# " + axioms.size)
        Some(new Evidence(proposition, axioms, Seq[WeightedRule]()))
      }
      case true => {
        logger.error("No matching predicates for proposition: " + proposition.toString())
        None
      }
    }

  }


}
