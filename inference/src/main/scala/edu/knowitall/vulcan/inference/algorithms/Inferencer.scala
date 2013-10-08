package edu.knowitall.vulcan.inference.algorithms

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 10/3/13
 * Time: 10:52 PM
 * To change this template use File | Settings | File Templates.
 */


import edu.knowitall.vulcan.inference.proposition.Proposition
import edu.knowitall.vulcan.inference.kb._
import edu.knowitall.vulcan.inference.mln.tuffyimpl.{TuffyFormatter, MLNInstanceIO, TuffyWrapper}
import edu.knowitall.vulcan.inference.entailment.EntailmentScorer
import edu.knowitall.vulcan.inference.utils.{Crossable, TupleHelper}
import edu.knowitall.vulcan.common.Tuple
import java.io.File
import scopt.mutable.OptionParser
import scala.Some
import edu.knowitall.vulcan.inference.mln.tuffyimpl.InferenceResults
import edu.knowitall.vulcan.inference.evidence.Evidence
import org.slf4j.LoggerFactory

trait Inferencer {

  def marginal(t:Proposition, evidence:Evidence): Option[InferenceResults]

}


class TuffyInferencer(tuffy:TuffyWrapper, tempDirectory:String) extends Inferencer{

  def marginal(t: Proposition, evidence: Evidence) = {
    val instance = MLNInstanceIO.fromEvidence(evidence)
    TuffyFormatter.exportToDirectory(instance, tempDirectory)
    tuffy.runTuffyNative(tempDirectory)
  }
}

object BidiSearchInferencer{

  val logger = LoggerFactory.getLogger(this.getClass)

  case class Config(){
    var endpoint = ""
    var tuffyConfFile = ""
    var rulesFile = ""
    var tempDir = ""
    var host = ""
    var port = 0
    var numTuples = 10
    var threshold = 0.5

  }
  def main(args:Array[String]){
    val config = Config()
    val parser = new OptionParser() {
      //arg("endpoint", "Textual evidence client endpoint.", {str => config.endpoint = str})
      arg("tuffyConfFile", "Tuffy conf file.", {str => config.tuffyConfFile = str})
      arg("rulesFile", "Rules file.", {str => config.rulesFile = str})
      arg("tempDir", "Temporary directory to store MLN instance files.", {str => config.tempDir = str})
      //arg("host", "CNC Host.", {str => host = str})
      //arg("port", "Port", {str => port = str.toInt})
      //opt("n", "numTuples", "Number of top textual evidence tuples to use.", {str => config.numTuples = str.toInt})
      opt("t", "threshold", "Threshold for determining if an evidence fact supports the target.", {str => config.threshold = str.toDouble})
    }
    if (!parser.parse(args)) return
    val tuffy = new TuffyWrapper(config.tuffyConfFile)
    val inf = new BidiSearchInferencer(new EntailmentScorer, new TuffyInferencer(tuffy, config.tempDir), config.threshold)
    val rules = LogicRules.fromFile(new File(config.rulesFile))
    val proposition = new Proposition(Seq[Predicate](), Predicate(TupleHelper.fromLemmas("iron nail", "be conductor of", "electricity"), 1.0))

    val a1 = Axiom.fromTuple(TupleHelper.fromLemmas("metal", "be excellent conductor of", "electricity"), 1.0)
    val a2 = Axiom.fromTuple(TupleHelper.fromLemmas("iron", "type of", "metal"), 1.0)
    val a3 = Axiom.fromTuple(TupleHelper.fromLemmas("iron nail", "composed of", "iron"), 1.0)
    inf.supportingAxiom(a1.consequent.tuple, a1::a2::a3::Nil, 0.5) match {
      case Some(axiom:Axiom) => logger.info("Axiom supporting: " + TuffyFormatter.exportRule(a1))
      case None => logger.info("Test axiom isnot supported")
    }

    val axioms = a1::a2::a3::Nil
    val evidence = new Evidence(proposition, axioms, rules)
    //Test rule inverter.
    val invRules = inf.invert(rules)
    invRules.foreach(ir => println(TuffyFormatter.exportRule(ir, withWeights = true, withQuotes = false)))
    val basic = new TuffyInferencer(tuffy, config.tempDir)
    inf.marginal(proposition, evidence) match {
      case Some(results:InferenceResults) =>{
        val axioms = inf.toAxioms(results)
        logger.info("Found %d new facts after full inference.".format(axioms.size))
        axioms.foreach(axiom => logger.info(TuffyFormatter.exportRule(axiom, withWeights=true, withQuotes=true)))
        inf.supportingAxiom(proposition, axioms, threshold=0.01) match {
          case Some(axiom:Axiom) => logger.info("Proposition's marginal: " + TuffyFormatter.exportRule(axiom, withWeights=false, withQuotes=true))
          case None => logger.info("Proposition is not supported by inference.")
        }

      }
      case None => logger.error("No results from tuffy for proposition: " + proposition.text)
    }


  }
}
class BidiSearchInferencer(entailment:EntailmentScorer,
                           basic:Inferencer,
                           threshold:Double) extends Inferencer{

  val logger = LoggerFactory.getLogger(this.getClass)

  def supportingAxiom(target:Tuple,
                      axioms:Iterable[Axiom],
                      threshold:Double): Option[Axiom] = {
    logger.info("Target: " + target.text)
    logger.info("Axioms: ")
    axioms.foreach(axiom => logger.info("Axiom: " + TuffyFormatter.exportRule(axiom)))
    axioms.find(e => TupleHelper.identical(target, e.consequent.tuple))
  }

  def supportingAxiom(proposition:Proposition,
                      axioms:Iterable[Axiom],
                      threshold:Double): Option[Axiom] = {
    supportingAxiom(proposition.consequent.tuple, axioms, threshold)
  }


  def supportingAxiom(proposition: Proposition,
                      evidence: Evidence,
                      threshold: Double): Option[Axiom] =  supportingAxiom(proposition.consequent.tuple, evidence.axioms, threshold)


  def toAxioms(results:InferenceResults): Seq[Axiom] = results.marginals.toSeq.flatMap(toAxiom(_))
  def toAxiom(sd: (String, Double)): Iterable[Axiom] = {
    TuffyFormatter.importPredicate(sd._1, sd._2) match {
      case Some(pred: Predicate) => Some(Axiom.fromPredicate(pred, sd._2))
      case None => None
    }
  }


  def marginal(t: Proposition, evidence: Evidence) = {
    def toInferenceResults(x:Axiom) = Some(new InferenceResults(((t.text -> x.confidence)::Nil).toMap, Seq[String]()))
    supportingAxiom(t, evidence, threshold) match {
      case Some(x:Axiom) => toInferenceResults(x)
      case None => {
        logger.info("Proposition not readily supported. Running basic inference...")
        basic.marginal(t, evidence) match {
          case Some(results:InferenceResults) => {
            logger.info("Found %s new facts via inference".format(results.marginals.size))
            val newAxioms = toAxioms(results)
            supportingAxiom(t, newAxioms, threshold) match {
              case Some(x:Axiom) => toInferenceResults(x)
              case None => {
                logger.info("Proposition not supported by new facts. Finding gaps...")
                val expanded = new Evidence(evidence.proposition, evidence.axioms ++ newAxioms, evidence.rules)
                val bridgeRules = find_and_bridge_gaps(t, expanded)
                logger.info("Found %d bridges".format(bridgeRules.size))
                bridgeRules.foreach(rule => logger.info("Bridge: %s".format(TuffyFormatter.exportRule(rule, withWeights = true, withQuotes = true))))
                val bridged = new Evidence(evidence.proposition, expanded.axioms, evidence.rules ++ bridgeRules)
                logger.info("Re-running inference with bridge rules.")
                basic.marginal(t, bridged)
              }
            }
          }
          case None => None
        }
      }
    }
  }

  def invert(rules:Seq[WeightedRule]) = {
    def invert(rule:WeightedRule) = {
      val antecedents = rule.antecedents
      val consequent = rule.consequent
      antecedents.map(a => new WeightedRule(Seq(consequent), a, rule.confidence))
    }
    rules.flatMap(rule => invert(rule))
  }

  /**
   * Find facts (T') which when true can yield t.
   * Run inference using inverted rules with
   * t as evidence and e as query predicate.
   * (E' x T') represents the possible gaps
   * Select a subset of these gaps and add bridges
   * based on their textual entailment score.
   *
   */
  def find_and_bridge_gaps(t:Proposition, evidence:Evidence, topn:Int=10):Seq[WeightedRule] = {
    val invRules = invert(evidence.rules)
    var plausible = Seq[Axiom]()
    def toProposition(axiom:Axiom) = new Proposition(Seq[Predicate](), axiom.consequent)
    val taxiom = Axiom.fromPredicate(t.consequent, 1.0)
    evidence.axioms.foreach(axiom => {
      logger.info("Running axiom: " + TuffyFormatter.exportRule(axiom) + " as query.")
      val aprop = toProposition(axiom)
      val pevidence = new Evidence(aprop, Seq(taxiom), invRules)
      basic.marginal(aprop, pevidence) match {
        case Some(results:InferenceResults) => {
          val plausibleAxioms = toAxioms(results)
          logger.info("Found %d plausible axioms".format(plausibleAxioms.size))
          plausibleAxioms.foreach(pax => logger.info("Plausible: " + TuffyFormatter.exportRule(pax)))
          plausible ++= plausibleAxioms
        }
        case None =>
      }
    })
    logger.info("Found %d plausible facts.".format(plausible.size))
    def findGaps(forward:Seq[Axiom], backward:Seq[Axiom], topn:Int = 100): Seq[Gap] = {
      import Crossable._
      (forward x backward).map(pair => {
        val score = entailment.score(pair._1.consequent.tuple, pair._2.consequent.tuple) * (pair._1.score()  + pair._2.score())/2.0
        Gap(pair._1.consequent, pair._2.consequent, score)
      }).toSeq.sortBy(-_.score).take(topn)
    }
    //evidence.axioms ++ (
    val gaps = findGaps(evidence.axioms ++ (Axiom.fromTuple(t.consequent.tuple)::Nil), plausible)
    //gaps.foreach(gap => logger.info("Gap: " + TuffyFormatter.exportPredicate(gap.f) + " => " + TuffyFormatter.exportPredicate(gap.b) + " = " + gap.score))
    gaps.map(gap => new WeightedRule(Seq(gap.f), gap.b, gap.score))
  }



}

case class Gap(f:Predicate, b:Predicate, val score:Double)