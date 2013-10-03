package edu.knowitall.vulcan.inference.kb

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 8/13/13
 * Time: 2:34 PM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import edu.knowitall.vulcan.inference.proposition.Proposition

import edu.knowitall.vulcan._
import edu.knowitall.vulcan.common.Tuple
import edu.knowitall.vulcan.inference.utils.TupleHelper

trait ScoredItem {
  def score():Double
}

object Predicate{

  val logger = LoggerFactory.getLogger(this.getClass)

  val re = """T\((.*?),(.*?),(.*)\)""".r
  def fromBinaryPredicateString(string:String): Option[Predicate] = {
    try{
      val re(arg1:String, rel:String, arg2:String) = string
      Some(new Predicate(TupleHelper.fromLemmas(arg1.trim, rel.trim, arg2.trim), 1.0))
    } catch{
      case e:Exception => {
        println("Failed to parse binary relation string: " + string)
      }
      None
    }
  }

}

case class Predicate(tuple:Tuple, confidence:Double) extends ScoredItem  {

  def score() = confidence

  override def toString() = "%.2f %s".format(score(), tuple.text)
}

object Axiom{

  def fromPredicate(predicate: Predicate, score: Double)  = new Axiom(Seq[Predicate](), predicate, score)

  def fromRule(rule:Rule, confidence:Double = 1.0) = new Axiom(rule.antecedents, rule.consequent, confidence)

  def fromWeightedRule(rule:WeightedRule) = new Axiom(rule.antecedents, rule.consequent, rule.confidence)

  def fromTuple(tuple:Tuple, confidence:Double = 1.0) = fromPredicate(Predicate(tuple, confidence), confidence)//new Axiom(Seq[Predicate](), Predicate(tuple, 1.0), confidence)

  def fromProposition(prop:Proposition) = new Axiom(prop.antecedents, prop.consequent, confidence = 1.0)

}
class Axiom(ant:Seq[Predicate], con:Predicate, confidence:Double) extends WeightedRule(ant, con, confidence)



