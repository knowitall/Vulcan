package edu.knowitall.vulcan.inference.utils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/19/13
 * Time: 9:01 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import edu.knowitall.vulcan.common._
import edu.knowitall.vulcan.common.Term
import edu.knowitall.vulcan.common.Tuple
import edu.knowitall.vulcan.common.Relation
import edu.knowitall.vulcan.common.TermsArg

object TupleHelper {

  def toArgText(arg:Arg): String = arg.text
  def toRelText(rel:Relation): String = rel.terms.map(_.text).mkString(" ")
  def tupleText(tuple:Tuple) = toArgText(tuple.arg1) + " " + toRelText(tuple.rel) + " " + tuple.arg2s.map(toArgText(_)).mkString(" ")

  def from(arg1:String, rel:String, arg2:String) = {
    def arg(text:String) = TermsArg(Seq(Term(text)))
    def relation(text:String) = Relation(Seq(Term(text)))
    Tuple(arg(arg1), relation(rel), Seq(arg(arg2)))
  }

}
