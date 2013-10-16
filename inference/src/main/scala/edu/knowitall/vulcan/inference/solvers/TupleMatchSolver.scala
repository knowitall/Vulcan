package edu.knowitall.vulcan.inference.solvers

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 10/8/13
 * Time: 10:20 AM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import com.vulcan.halo.core._

import java.util
import edu.knowitall.vulcan.inference.apps.PropositionVerifier
import edu.knowitall.vulcan.inference.proposition.{Proposition, PropositionIO}
import edu.knowitall.vulcan.inference.entailment.EntailmentScorer
import edu.knowitall.vulcan.inference.algorithms.InferenceUtils
import edu.knowitall.vulcan.inference.mln.tuffyimpl.{TuffyFormatter, InferenceResults}
import edu.knowitall.vulcan.inference.kb.Axiom
import edu.knowitall.vulcan.inference.evidence.TextualAxiomsFinder
import edu.knowitall.vulcan.inference.openie.OpenIEWrapper
import edu.knowitall.vulcan.extraction.Extractor


trait Solver {

  def solve(parse:QuestionParse):Answer
}



abstract class TrueFalseSolver extends Solver{

  def answerEntry(score:Float) = {
    val entry = new AnswerEntry()
    entry.setAnswerPhrase("yes")
    entry.setConfidence(score)
    entry
  }

  def answerEntry(score:Float, supports:Seq[AnswerSupport]) = {
    val entry = new AnswerEntry()
    entry.setAnswerPhrase("yes")
    entry.setConfidence(score)
    entry.setSupports(supports.toList)
    entry
  }

  def solve(parse:QuestionParse):Answer = parse.getQuestionType match {
    case QuestionType.IS_IT_TRUE_THAT => {
      val question = parse.getMainQuestion
      val supports: Seq[AnswerSupport] = findSupports(question, topn = 10)
      val score = supports.map(_.getConfidence).headOption match { case Some(x:Float) => x; case None => -1}
      val entry = answerEntry(score, supports)
      val entries = new util.ArrayList[AnswerEntry]
      entries += entry
      val message = "Place holder message."
      val answer = Answer.getEmptyAnswer(this.getClass.getCanonicalName, parse, message)
      answer.setEntries(entries)
      answer
    }
    case _ => throw new Exception("QuestionType %s is not supported by %s.".format(parse.getQuestionType, this.getClass.getCanonicalName))
  }

  def findSupports(question:String, topn:Int):Seq[AnswerSupport]


  def support(axiom:Axiom) = {
    val s = new AnswerSupport
    s.setAnswer_phrase(TuffyFormatter.exportRule(axiom, withWeights = false, withQuotes = false))
    s.setConfidence(axiom.score().toFloat)
    s.setSid("TBD")
    s.setSolver(this.getClass.getCanonicalName)
    s
  }

}

class TupleMatchTrueFalseSolver(taf:TextualAxiomsFinder, extractor:Extractor) extends TrueFalseSolver {

  def findSupports(question: String, topn:Int=5) = {
    val (questionText, proposition) = Proposition.fromTrueThatQuestion(question, extractor)
    taf.find(proposition, Some(questionText)).take(topn).map(axiom => support(axiom))

  }
}

class MLNTrueFalseSolver(verifier:PropositionVerifier, extractor:Extractor) extends TrueFalseSolver {
  def findSupports(question:String, topn:Int = 10) = {
    val (questionText, proposition) = Proposition.fromTrueThatQuestion(question,extractor)
    verifier.verify(proposition) match {
      case Some(results:InferenceResults) => {
        InferenceUtils.toAxioms(results).map(support(_))
      }
      case None => Seq[AnswerSupport]()
    }

  }

}
object MLNTrueFalseSolver {
  def main(args:Array[String]){
    //val parse = com.vulcan.halo.util.XmlUtil.fromXmlString(answerXml, classOf[QuestionParse])
  }
}




abstract class MultipleChoiceSolver extends Solver {

  val logger = LoggerFactory.getLogger(this.getClass)


  def answerEntries(options: Seq[(QuestionOption, Double)]) = {
    def answerEntry(option:QuestionOption, score:Float) = {
      val entry = new AnswerEntry()
      entry.setAnswerPhrase(option.getAnswerOption)
      entry.setConfidence(score)
      entry
    }
    options.map(option => answerEntry(option._1, option._2.toFloat))
  }
  override def solve(parse:QuestionParse):Answer = {
    parse.getQuestionType match {
      case QuestionType.MULTIPLE_CHOICE => {
        val scores = score(parse.getOptions).toSeq.sortBy(-_._2)
        val entries = new util.ArrayList[AnswerEntry]
        entries ++= answerEntries(scores)
        val message = "Empty message."
        val answer = Answer.getEmptyAnswer(this.getClass.getCanonicalName, parse, message)
        answer.setEntries(entries)
        answer
      }
      case _ => {
        throw new Exception("QuestionType %s is not supported by %s.".format(parse.getQuestionType, this.getClass.getCanonicalName))
      }
    }
  }

  def score(options:Seq[QuestionOption]):Map[QuestionOption, Double]

}

class TupleMatchSolver extends MultipleChoiceSolver {

  def score(option:QuestionOption):(QuestionOption, Double) = {
    val question = option.getAnswerOption
    println("question: " + question)
    (option, 1.0)
  }
  def score(options: Seq[QuestionOption]): Map[QuestionOption, Double] = {
    options.map(score(_)).toMap
  }

}



