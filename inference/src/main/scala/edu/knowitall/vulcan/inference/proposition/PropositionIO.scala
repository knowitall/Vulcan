package edu.knowitall.vulcan.inference.proposition

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 8/14/13
 * Time: 9:49 AM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.io.Source
import edu.knowitall.vulcan.inference.openie.OpenIEWrapper
import edu.knowitall.vulcan.inference.kb._
import java.io.File
import edu.knowitall.vulcan.inference.kb.Predicate
import edu.knowitall.openie.Instance
import scala.Some
import edu.knowitall.vulcan.inference.utils.TupleHelper._


object QuestionIO{

  val X_is_Y_inwhich_Z = """(.*?) (is|the) (.*?) (in which) (.*)""".r

  val X_is_Y_that_Z = """(.*?) (is|are) (.*?) (that|which) (.*)""".r

  val keys = Set[String]("(A)", "(B)", "(C)", "(D)")

  def fromLine(line:String) = {
    import OpenIEWrapper._
    val splits = line.split("\t")
    val qid = splits(0)
    def createQuestion = new Question(splits(0), splits(1), splits(2).toInt, splits(3),
      splits(4).toInt == 1, splits(5).toInt == 1, splits(6),
      splits(7).toInt, splits(8).toInt, splits(9), new ArrayBuffer[Assertion]())

    splits.drop(10).flatMap(x => {
      if(!keys.contains(x)){
        val text = x.replaceAll("(.*?)Is it true that", "").replaceAll("""\?""", ".").trim.capitalize


        var simplifiedText = X_is_Y_that_Z.findFirstMatchIn(text) match {
          case Some(m:Regex.Match) => {
            val X_is_Y_that_Z(x, f1, y, f2, z) = text
            x + " " + z
          }
          case _ => ""
        }
        if(X_is_Y_inwhich_Z.findFirstMatchIn(text) != None){
          simplifiedText = ""
        }
        Some(new Assertion(x, openie.extract(text), openie.extract(simplifiedText)))
      }else{
        None
      }
    })
  }
  def fromFile(file:File)  = Source.fromFile(file).getLines().flatMap(fromLine(_))
}

  case class Question(qdid:String,
                    qid:String,
                    point:Int,
                    answerKey:String,
                    isMultipleChoice:Boolean,
                    includesDiagram:Boolean,
                    examName:String,
                    grade:Int,
                    year:Int,
                    question:String,
                    assertions:ArrayBuffer[Assertion])

  case class Assertion(raw:String, tuples:Seq[Instance], simplified:Seq[Instance]) {
    def serialize() = raw + "\t" + tuples.map("<tuple>" + _.toString  + "</tuple>").mkString("")
  }

class Proposition(aseq:Seq[Predicate], c:Predicate) extends Rule {

  def antecedents: Seq[Predicate] = aseq
  def consequent: Predicate = c
  def text: String = {
    val astring = aseq.isEmpty match {
      case false => aseq.map(a => tupleText(a.tuple)).mkString(" ")
      case true => ""
    }
    astring + " " + tupleText(consequent.tuple)
  }
}

object PropositionIO {

  val logger = LoggerFactory.getLogger(this.getClass)


  def fromAssertion(assertion:Assertion) = {

    def selectInstance(instances:Seq[Instance]) = {
      println("Instance size: " + instances.size)
      instances.maxBy(x => x.extraction.rel.text.size)
    }
    val bestInstance = selectInstance(assertion.tuples)
    val binTuple = BinaryRelationTuple.fromExtraction(bestInstance.extraction)
    val tuple = from(binTuple.arg1, binTuple.relationText(), binTuple.arg2)
    new Proposition(Seq[Predicate](), new Predicate(tuple, 1.0))
  }

  def fromFile(file:File) = QuestionIO.fromFile(file).map(fromAssertion(_))

  def main(args:Array[String]){
    PropositionIO.fromFile(new File(args(0)))
                 .foreach(proposition => println(proposition.toString))

    val testProposition = new Proposition(Seq[Predicate](), new Predicate(from("iron_nail", "conductorOf", "electricity"), 1.0))

  }


}