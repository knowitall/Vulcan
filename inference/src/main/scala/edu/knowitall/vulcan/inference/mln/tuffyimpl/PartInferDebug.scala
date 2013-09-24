package edu.knowitall.vulcan.inference.mln.tuffyimpl

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 8/27/13
 * Time: 9:21 AM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import tuffy.main.Infer
import tuffy.parse.CommandOptions
import tuffy.util.{Config, Settings, UIMan}
import tuffy.infer.InferPartitioned
import scala.collection.mutable
import tuffy.mln.Clause

import scala.collection.JavaConversions._
import edu.knowitall.vulcan.inference.mln.tuffyimpl.TuffyUtils


case class InferenceResults(marginals:Map[String, Double], relevantClauses:Seq[String]){


}

class PartInferDebug extends Infer{

  val logger = LoggerFactory.getLogger(this.getClass)


  def run( opt:CommandOptions) = {

    UIMan.println(">>> Running partition-aware inference WITH DEBUG")
    setUp(opt)
    ground()

    val ip = new InferPartitioned(grounding, dmover)
    if(options.maxFlips == 0){
      options.maxFlips = 100 * grounding.getNumAtoms()
    }
    if(options.maxTries == 0){
      options.maxTries = 3;
    }

    val pmap = ip.getPartitionScheme()
    val ncomp = pmap.numComponents()
    val nbuck = ip.getNumBuckets()
    var sdata = new StringBuilder

    sdata.append(UIMan.comma(ncomp))
    sdata.append(if(ncomp > 1) " components" else " component")
    sdata.append(" (grouped into ")
    sdata.append(UIMan.comma(nbuck)).append(if(nbuck > 1) " buckets"  else " bucket)")

    val settings = new Settings()
    val fpa = options.maxFlips.toDouble/grounding.getNumAtoms().toDouble


    UIMan.println(">>> Running marginal inference on " + sdata)
    var mfout = options.fout
    if(opt.dual) mfout += ".marginal"

    settings.put("task", "MARGINAL")
    settings.put("nsamples", new Integer(options.mcsatSamples))
    settings.put("flipsPerAtom", fpa)

    var relevantClauses = Seq[String]()
    val avgProbs = new mutable.HashMap[String, Double]()
    (0 until options.numInfIters).foreach(i => {
      print("Iteration: %d\r".format(i))
      val (probs, c) = runInference(ip, settings, mfout)
      relevantClauses = c
      probs.foreach(kv => {
        val count = avgProbs.getOrElseUpdate(kv._1, 0) + kv._2/options.numInfIters.toDouble
        avgProbs += kv._1 -> count
      })
    })
    val predR = """(.*?)\(.*\)""".r
    def predicateName(string:String) = {
      val predR(name:String) = string
      name
    }
    avgProbs.groupBy(kv => predicateName(kv._1)).foreach(grp => {
      val name = grp._1
      val grpMap = grp._2
      grpMap.toSeq.sortBy(x => -x._2).foreach(qp => println(qp._1 + "\t" + qp._2))
      println("")
      println("")
      println("")
    })

    if(Config.sampleLog != null){
      Config.sampleLog.close()
      val mfout = options.fout
      dmover.dumpSampleLog(mfout + ".log")
    }
    cleanUp()
    new InferenceResults(avgProbs.toMap, relevantClauses)
  }

  def runInference(ip: InferPartitioned, settings: Settings, mfout: String) = {
    val aveCost = ip.infer(settings)
    val relClauses = ip.mln.getRelevantClauses
    var relClausesStrings = Seq[String]()
    relClauses.foreach(clause => {
      val sb = new mutable.StringBuilder()
      sb.append(clause.toString).append("\n")
      println("Clause string: " + clause.toString)
      if (clause.hasEmbeddedWeight) {
        sb.append(clause.toStringForFunctionClause(clause.getSignature, clause.getWeight)).append("\n")
        println("Clause function string: " + clause.toStringForFunctionClause(clause.getSignature, clause.getWeight))
      } else {
        sb.append("Clause has no embedded weight.").append("\n")
        println("Has no embedded weight.")
      }
      val instances: java.util.ArrayList[Clause#ClauseInstance] = clause.instances
      instances.foreach(instance => {
        println("Instance: " + TuffyUtils.toString(instance))
        sb.append(TuffyUtils.toString(instance)).append("\n")
      })
      relClausesStrings :+= sb.toString()
      println()
      println()
      println()

      /** val predicates = clause.getReferencedPredicates
      predicates.foreach(predicate => {
        println("Predicate: " + predicate.toString)
      })
      val instances: util.ArrayList[Clause#ClauseInstance] = clause.instances

      instances.foreach(instance => {
        println("Instance: " + instance.toString)
      }) */
    })

    //UIMan.println("### Average Cost " + UIMan.decimalRound(2, aveCost))
    //UIMan.println(">>> Writing answer to file: " + mfout)
    //Config.mcsat_output_hidden_atoms = true
    (TuffyUtils.getQueryProbabilities(dmover, mln), relClausesStrings)
   //val output = TuffyUtils.dumpProbsToFile(dmover, mln, mln.relAtoms)
    //println("Output:\n" + output)
  }
}
