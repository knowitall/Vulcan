package edu.knowitall.vulcan.inference.apps

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 8/14/13
 * Time: 9:56 PM
 * To change this template use File | Settings | File Templates.
 */


import edu.knowitall.vulcan.inference.evidence.{PatternEvidenceFinder, Evidence, EvidenceFinder}
import edu.knowitall.vulcan.inference.proposition.Proposition
import edu.knowitall.vulcan.inference.kb.{LogicRules, WeightedRule, Predicate}
import edu.knowitall.vulcan.inference.openie.SolrSearchWrapper
import edu.knowitall.vulcan.inference.utils.TupleHelper
import edu.knowitall.vulcan.inference.mln.tuffyimpl._
import java.io.File
import org.slf4j.LoggerFactory
import edu.knowitall.vulcan.inference.mln.tuffyimpl.InferenceResults
import edu.knowitall.vulcan.inference.evidence.Evidence
import scala.Some


class PropositionVerifier(finder:EvidenceFinder,
                          tuffy:TuffyWrapper,
                          rules:Seq[WeightedRule],
                          tempDirectory:String){


  val logger = LoggerFactory.getLogger(this.getClass)
  //val conf = new TuffyArgs(tempDirectory, tuffy)

  def verify(proposition:Proposition) = {
    finder.find(proposition) match {
      case Some(evidence:Evidence) => {
        val newEvidence = new Evidence(evidence.proposition, evidence.axioms, evidence.rules ++ rules)
        val instance = MLNInstanceIO.fromEvidence(newEvidence)
        TuffyFormatter.exportToDirectory(instance, tempDirectory)
        tuffy.runTuffyNative(tempDirectory) match {
          case Some(results:InferenceResults) => {
            "Output: " + results.marginals.mkString("\n")
          }
          case None => {
            logger.error("No results from inference.")
            "No results from inference."
          }
        }
      }
      case None => {
        println("No evidence found for: " + proposition.text)
        "No evidence found for: " + proposition.text
      }
    }
  }
}

object PropositionVerifier {



  def main(args:Array[String]){

    val finder = new PatternEvidenceFinder(SolrSearchWrapper.getInstance(args(0)))
    val tuffy = new TuffyWrapper(args(1))
    val verifier = new PropositionVerifier(finder, tuffy, LogicRules.fromFile(new File(args(2))), args(3))
    val tupleRegex = """\((.*?), (.*?), (.*)\)""".r
    println("Enter Prop[(arg1, rel, arg2)]: ")
    import TupleHelper._
    for( ln <- io.Source.stdin.getLines ) {
      val tupleRegex(arg1, rel, arg2) = ln
      val pred = new Predicate(from(arg1, rel, arg2), 1.0)
      verifier.verify(new Proposition(Seq[Predicate](), pred))
      println
      println
      println("Enter Prop[(arg1, rel, arg2)]: ")
    }
  }

}

