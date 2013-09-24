package edu.knowitall.vulcan.inference.mln.tuffyimpl

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/23/13
 * Time: 2:54 PM
 * To change this template use File | Settings | File Templates.
 */

import edu.knowitall.vulcan.inference.kb.{WeightedRule, Predicate}
import edu.knowitall.vulcan.common.Tuple
import java.io.{PrintWriter, File}
import edu.knowitall.vulcan.inference.mln.MLNInstance

object TuffyFormatter{

  def exportPredicate(p:Predicate, score:Boolean = false): String = {

    def prune(text:String) = TuffyUtils.toTuffyLiteral(text)

    def exportTuple(tuple:Tuple): String = "T(%s, %s, %s)".format(prune(tuple.arg1.lemma),prune(tuple.rel.lemma), prune(tuple.arg2s.map(_.lemma).mkString(" ")))

    val out = exportTuple(p.tuple)
    score match {
      case false => out
      case true => "%.2f %s".format(p.score(), out)
    }
  }

  val disjunction = " v "
  val implication = "=>"
  val NL = "\n"

  def exportAntecedents(seq:Seq[Predicate]) = seq.map(exportPredicate(_)).mkString(", ")

  def exportRule(axiom:WeightedRule, withWeights:Boolean = false): String = {
    val sb = new StringBuilder
    if(withWeights){
      sb.append("%.2f ".format(axiom.score))
    }
    if(!axiom.antecedents.isEmpty){
      sb.append(exportAntecedents(axiom.antecedents)).append(" ").append(implication).append(" ")
    }
    sb.append(exportPredicate(axiom.consequent))

    /**axiom.antecedents.isEmpty match {
      case true => "%.2f %s".format(axiom.score(), export(axiom.consequent))
      case false => "%.2f !%s %s %s".format(axiom.score(),export(axiom.consequent),disjunction, exportDisjunctions(axiom.antecedents))

    }*/
    sb.toString()
  }

  def exportPredicateDefinitions(seq:Seq[Predicate]) = seq.map(exportPredicate(_)).mkString("\n")

  def exportRules(seq:Iterator[WeightedRule], useWeights:Boolean=true): String = {
    seq.map(exportRule(_, useWeights)).mkString(NL)
  }

  def exportQueryFile(instance:MLNInstance, file:File) = {
    val output = exportRules(instance.query.iterator, false)   //Don't output weights for query predicates.
    val writer = new PrintWriter(file)
    writer.println(output)
    writer.close
  }

  def exportEvidenceFile(instance:MLNInstance, file:File) = {
    val output = exportRules(instance.evidence)
    val writer = new PrintWriter(file)
    writer.println(output)
    writer.close
  }

  def exportProgramFile(instance:MLNInstance, file:File) = {
    val output = exportPredicateDefinitions(instance.predicateDefinitions) + "\n\n" + exportRules(instance.rules.iterator)
    val writer = new PrintWriter(file)
    writer.println(output)
    writer.close
  }


  def exportToDirectory(instance:MLNInstance, directory:String) = {
    val efile = new File(directory + "/evidence.db")
    val pfile = new File(directory + "/prog.mln")
    val qfile = new File(directory + "/query.db")
    exportEvidenceFile(instance, efile)
    exportProgramFile(instance, pfile)
    exportQueryFile(instance, qfile)
  }

  def export(instance:MLNInstance):String = {
    "//Axioms\n" + exportRules(instance.evidence) + "\n" +
      "//Predicate definitions\n"  + exportPredicateDefinitions(instance.predicateDefinitions) + "\n" +
      "//Program\n" + exportRules(instance.rules.iterator) + "\n" +
      "//Evidence\n" + exportRules(instance.query.iterator)

  }
}
