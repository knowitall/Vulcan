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
      Some(new Predicate(TupleHelper.from(arg1, rel, arg2), 1.0))
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

  override def toString() = "%.2f %s".format(score(), tuple.toString())
}

object Axiom{

  def fromProposition(prop:Proposition) = new Axiom(prop.antecedents, prop.consequent, confidence = 1.0)

  def fromRule(rule:Rule, confidence:Double = 1.0) = new Axiom(rule.antecedents, rule.consequent, confidence)

  def fromWeightedRule(rule:WeightedRule) = new Axiom(rule.antecedents, rule.consequent, rule.confidence)

}
class Axiom(ant:Seq[Predicate], con:Predicate, confidence:Double) extends WeightedRule(ant, con, confidence)



