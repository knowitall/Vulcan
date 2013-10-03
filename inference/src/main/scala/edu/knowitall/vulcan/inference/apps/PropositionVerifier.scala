package edu.knowitall.vulcan.inference.apps

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 8/14/13
 * Time: 9:56 PM
 * To change this template use File | Settings | File Templates.
 */


import edu.knowitall.vulcan.inference.evidence.{TextualAxiomsFinder, AxiomsFinder}
import edu.knowitall.vulcan.inference.proposition.Proposition
import edu.knowitall.vulcan.inference.kb._
import edu.knowitall.vulcan.inference.mln.tuffyimpl._
import org.slf4j.LoggerFactory
import java.io.File
import edu.knowitall.vulcan.inference.mln.tuffyimpl.InferenceResults
import edu.knowitall.vulcan.inference.evidence.Evidence
import scala.Some
import scopt.mutable.OptionParser
import edu.knowitall.vulcan.inference.utils.TupleHelper


class PropositionVerifier(finders:Seq[AxiomsFinder],
                          tuffy:TuffyWrapper,
                          rules:Seq[WeightedRule],
                          tempDirectory:String,
                          host:String,
                          port:Int){


  val logger = LoggerFactory.getLogger(this.getClass)

  def findEvidence(proposition:Proposition)  =  {
    val axioms = finders.flatMap(_.find(proposition))
    new Evidence(proposition, axioms, rules)
  }
  def exportEvidence(evidence:Evidence) = {
    val instance = MLNInstanceIO.fromEvidence(evidence)
    TuffyFormatter.exportToDirectory(instance, tempDirectory)

  }
  def verify(proposition:Proposition) = {
    val evidence = findEvidence(proposition)
    exportEvidence(evidence)
    runTuffy()
  }

  def runTuffy() = tuffy.runTuffyNative(tempDirectory)
}

object PropositionVerifier {


  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args:Array[String]){

    var endpoint = ""
    var tuffyConfFile = ""
    var rulesFile = ""
    var tempDir = ""
    var host = ""
    var port = 0
    var numTuples = 10
    val parser = new OptionParser() {
      arg("endpoint", "Textual evidence client endpoint.", {str => endpoint = str})
      arg("tuffyConfFile", "Tuffy conf file.", {str => tuffyConfFile = str})
      arg("rulesFile", "Rules file.", {str => rulesFile = str})
      arg("tempDir", "Temporary directory to store MLN instance files.", {str => tempDir = str})
      arg("host", "CNC Host.", {str => host = str})
      arg("port", "Port", {str => port = str.toInt})
      opt("n", "numTuples", "Number of top textual evidence tuples to use.", {str => numTuples = str.toInt})
    }

    if (!parser.parse(args)) return

    val verifier = verifierInstance(endpoint, tuffyConfFile, rulesFile, tempDir, host, port, numTuples)
    import TupleHelper._
    val tupleRegex = """\((.*?), (.*?), (.*)\)""".r
    println("Enter Prop[(arg1, rel, arg2)]: ")
    for( ln <- io.Source.stdin.getLines() ) {
      val tupleRegex(arg1, rel, arg2) = ln
      val pred = new Predicate(from(arg1, rel, arg2, addLemmas=true), 1.0)
      val prop = new Proposition(Seq[Predicate](), pred)
      val evidence = verifier.findEvidence(prop)
      verifier.exportEvidence(evidence)
      //verifier.runTuffy()
      println
      println
      println("Enter Prop[(arg1, rel, arg2)]: ")
    }
  }


  def verifierInstance(endpoint: String, tuffyConfFile: String, rulesFile: String, tempDir: String, host:String, port:Int, numTuples:Int) = {
    val taf = new TextualAxiomsFinder(endpoint, numTuples)
    val kbf = new KBAxiomFinder(new WordnetKB :: new CNCategorizerKB(host, port) :: Nil)
    val finders = Seq(taf, kbf)
    val tuffy = new TuffyWrapper(tuffyConfFile)
    val file = new File(rulesFile)
    val rules = LogicRules.fromFile(file)
    logger.info("# of rules loaded = %d".format(rules.size))
    new PropositionVerifier(finders, tuffy, rules, tempDir, host, port)
  }
}

