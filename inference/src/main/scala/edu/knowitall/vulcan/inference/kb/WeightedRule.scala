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

abstract class Rule {
  def antecedents:Seq[Predicate]
  def consequent:Predicate

  override def toString() = antecedents.map(_.toString).mkString(",") + " -> " + consequent.toString
}

object WeightedRule{

  val logger = LoggerFactory.getLogger(this.getClass)
  /**
   * 1000 T(a1, rel, a2); T(b, "composed of", a1) => T(b, rel, a2)
   *
   */
  val rule_re = """(.*?) (.*) => (.*)""".r

  def fromMLNString(string:String)  = {
    try{
      val rule_re(wt:String, anteClauses, consequent) = string
      val antecedents = anteClauses.split(";").flatMap(clause => Predicate.fromBinaryPredicateString(clause))
      Predicate.fromBinaryPredicateString(consequent) match {
        case Some(consequent:Predicate) => Some(new WeightedRule(antecedents, consequent, wt.toDouble))
        case _ => None
      }
    }catch{
      case e:Exception => {
        logger.error("Failed to parse rule string: " + string)
        logger.error(e.getStackTraceString)
      }
      None
    }
  }
}
class WeightedRule(aseq:Seq[Predicate], c:Predicate, val confidence:Double) extends Rule with ScoredItem{

  def score() = confidence

  def antecedents: Seq[Predicate] = aseq

  def consequent: Predicate = c

  override def toString() = "%.2f %s".format(score(), super.toString())

}
