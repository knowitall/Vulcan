package edu.knowitall.vulcan.inference.evidence

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 8/14/13
 * Time: 10:32 AM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._

import edu.knowitall.vulcan.inference.proposition.Proposition
import edu.knowitall.vulcan.inference.openie.SolrSearchWrapper
import edu.knowitall.vulcan.inference.kb.{Axiom, WeightedRule, BinaryRelationTuple, Predicate}
import scala.collection.mutable
import edu.knowitall.vulcan.inference.utils.TupleHelper

class PatternEvidenceFinder(solr:SolrSearchWrapper) extends EvidenceFinder {

  val logger = LoggerFactory.getLogger(this.getClass)

  def find(proposition: Proposition): Option[Evidence] = {
    val text = proposition.text
    logger.info("Searching for: " + text)
    val predicates = solr.searchSentence(text)
                         .map(x => Predicate(TupleHelper.from(""""%s"""".format(x.arg1),
                                                              """"%s"""".format(x.relationText()),
                                                              """"%s"""".format(x.arg2)), 1.0))
    logger.info("# predicates found: " + predicates.size)
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
