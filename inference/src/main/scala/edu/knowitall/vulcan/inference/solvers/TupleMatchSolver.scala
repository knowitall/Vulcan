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
import edu.knowitall.vulcan.inference.proposition.PropositionIO


trait Solver {

  def solve(parse:QuestionParse):Answer
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

abstract class TrueFalseSolver extends Solver{

  def answerEntry(score:Float) = {
    val entry = new AnswerEntry()
    entry.setAnswerPhrase("yes")
    entry.setConfidence(score)
    entry
  }

  def solve(parse:QuestionParse):Answer = parse.getQuestionType match {
    case QuestionType.IS_IT_TRUE_THAT => {
      val question = parse.getMainQuestion
      val entry = answerEntry(score(question).toFloat)
      val entries = new util.ArrayList[AnswerEntry]
      entries += entry
      val message = "Place holder message."
      val answer = Answer.getEmptyAnswer(this.getClass.getCanonicalName, parse, message)
      answer.setEntries(entries)
      answer
    }
    case _ => throw new Exception("QuestionType %s is not supported by %s.".format(parse.getQuestionType, this.getClass.getCanonicalName))
  }

  def score(question:String):Double

}

object PropositionHelper{
  def apply(text:String) = {

  }
}
class TupleMatchTrueFalseSolver(verifier:PropositionVerifier) extends TrueFalseSolver {


  def score(question: String): Double = {

    verifier.verify(proposition)
  }

}
object TupleMatchTrueFalseSolver {
  def main(args:Array[String]){
    val solver = new TupleMatchTrueFalseSolver
    //val parse = com.vulcan.halo.util.XmlUtil.fromXmlString(answerXml, classOf[QuestionParse])
  }
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
