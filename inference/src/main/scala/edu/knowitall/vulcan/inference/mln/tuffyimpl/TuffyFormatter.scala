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
import edu.knowitall.vulcan.inference.utils.TupleHelper
import scala.util.matching.Regex

object TuffyFormatter{

  val withQuotesString = """T("%s", "%s", "%s")"""
  val withoutQuotesString = "T(%s, %s, %s)"
  val withQuotesRe = """T\("(.*?)", "(.*?)", "(.*)"\)""".r
  val withoutQuotesRe = """T\((.*?), (.*?), (.*)\)""".r

  def importPredicate(string:String, score:Double = 1.0): Option[Predicate] = {
    withoutQuotesRe.findFirstMatchIn(string) match {
      case Some(x:Regex.Match) => {
        val withoutQuotesRe(arg1:String, rel:String, arg2:String) = string
        Some(Predicate(TupleHelper.fromLemmas(arg1, rel, arg2), score))
      }
      case None => None
    }

  }
  def exportPredicate(p:Predicate, score:Boolean = false, withQuotes:Boolean = false): String = {

    def prune(text:String) = TuffyUtils.toTuffyLiteral(text, withQuotes)


    def exportTuple(tuple:Tuple): String = {
      val fmtString = withQuotes match {
        case true => withQuotesString
        case false => withoutQuotesString
      }
      import TupleHelper._
      /**fmtString.format(prune(tuple.arg1.text),
        prune(tuple.rel.text),
        prune(tuple.arg2s.map(_.text).mkString(" ")))*/

      fmtString.format(prune(lemma(tuple.arg1)),
                       prune(lemma(tuple.rel.terms)),
                       prune(tuple.arg2s.map(lemma(_)).mkString(" ")))
     }
    val out = exportTuple(p.tuple)
    score match {
      case false => out
      case true => "%.2f %s".format(p.score(), out)
    }
  }

  val disjunction = " v "
  val implication = "=>"
  val NL = "\n"



  def exportRule(axiom:WeightedRule, withWeights:Boolean = false, withQuotes:Boolean = false): String = {
    val sb = new StringBuilder
    if(withWeights){
      sb.append("%.2f ".format(axiom.score))
    }
    def exportAntecedents(seq:Seq[Predicate]) = seq.map(exportPredicate(_, false, withQuotes)).mkString(", ")
    if(!axiom.antecedents.isEmpty){
      sb.append(exportAntecedents(axiom.antecedents)).append(" ").append(implication).append(" ")
    }
    sb.append(exportPredicate(axiom.consequent, false, withQuotes))

    /**axiom.antecedents.isEmpty match {
      case true => "%.2f %s".format(axiom.score(), export(axiom.consequent))
      case false => "%.2f !%s %s %s".format(axiom.score(),export(axiom.consequent),disjunction, exportDisjunctions(axiom.antecedents))

    }*/
    sb.toString()
  }

  def exportPredicateDefinitions(seq:Seq[Predicate]) = seq.map(exportPredicate(_, false, false)).mkString("\n")

  def exportRules(seq:Iterator[WeightedRule], useWeights:Boolean=true, withQuotes:Boolean = false): String = {
    seq.map(exportRule(_, useWeights, withQuotes)).mkString(NL)
  }

  def exportQueryFile(instance:MLNInstance, file:File) = {
    val output = "//Original query\n" + exportRules(instance.query.iterator, false, true)   //Don't output weights for query predicates.
    val variants = "//Wildcard variants\n" + exportRules(instance.wildcardQueryVariants.iterator, false, true)
    val allTuples = "//All tuples\nT"
    val writer = new PrintWriter(file)
    writer.println(output + "\n" + variants + "\n" + allTuples)
    writer.close
  }

  def exportEvidenceFile(instance:MLNInstance, file:File) = {
    val output = exportRules(instance.evidence, false, true)
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
      "//Query\n" + exportRules(instance.query.iterator)

  }
}
