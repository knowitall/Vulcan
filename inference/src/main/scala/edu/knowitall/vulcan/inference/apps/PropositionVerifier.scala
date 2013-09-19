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
import edu.knowitall.vulcan.inference.mln.{TuffyWrapper, MLNInstanceIO}
import edu.knowitall.vulcan.inference.kb.{Predicate, BinaryRelationTuple}
import edu.knowitall.vulcan.inference.openie.SolrSearchWrapper

class PropositionVerifier(finder:EvidenceFinder, tuffy:TuffyWrapper, tempDirectory:String){

  def verify(proposition:Proposition) = {
    finder.find(proposition) match {
      case Some(evidence:Evidence) => {
        val instance = MLNInstanceIO.fromEvidence(evidence)
        MLNInstanceIO.TuffyFormatter.exportToDirectory(instance, tempDirectory)
        val output = tuffy.runTuffy(tempDirectory)
        println("Output: " + output)
      }
      case None => {
        println("No evidence found for: " + proposition.text)
      }
    }
  }
}

object PropositionVerifier {

  def main(args:Array[String]){
    val finder = new PatternEvidenceFinder(SolrSearchWrapper.getInstance(args(0)))
     //JenaEvidenceFinder.fromFiles(factsFile=args(0), rulesFile=args(1))
    //val propositions = PropositionIO.fromFile(new File(args(2)))
    val tuffy = TuffyWrapper.instance(args(1))
    val verifier = new PropositionVerifier(finder, tuffy, args(2))
    //propositions.foreach(verifier.verify(_))
    val tupleRegex = """\((.*?), (.*?), (.*)\)""".r
    println("Enter Prop[(arg1, rel, arg2)]: ")
    for( ln <- io.Source.stdin.getLines ) {
      val tupleRegex(arg1, rel, arg2) = ln
      val pred = new Predicate(new BinaryRelationTuple(arg1, rel, arg2), 1.0)
      verifier.verify(new Proposition(Seq[Predicate](), pred))
      println
      println
      println("Enter Prop[(arg1, rel, arg2)]: ")
    }
  }

}

