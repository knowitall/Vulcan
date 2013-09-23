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
    println("Searching for: " + text)
    val predicates = solr.searchSentence(text).map(x => Predicate(TupleHelper.from(x.arg1, x.relationText(), x.arg2), 1.0))
    println("Found: " + predicates.size)
    predicates.isEmpty match {
      case false => {
        val kbantecedent = Predicate(TupleHelper.from("KB", "KB", "KB"), 1.0)
        val rules = predicates.map(x => new WeightedRule(Seq[Predicate](kbantecedent), x, 1.0))
        Some(new Evidence(proposition, Seq[Axiom](), rules))
      }
      case true => None
    }

  }

}
