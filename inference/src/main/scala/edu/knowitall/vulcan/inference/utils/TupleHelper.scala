package edu.knowitall.vulcan.inference.utils

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/19/13
 * Time: 9:01 PM
 * To change this template use File | Settings | File Templates.
 */


import edu.knowitall.vulcan.common.Arg
import edu.knowitall.vulcan.common.Relation
import edu.knowitall.vulcan.common.Term
import edu.knowitall.vulcan.common.TermsArg
import edu.knowitall.vulcan.common.Tuple

object TupleHelper {

  def lemma(terms:Seq[Term]):String = terms.map(_.lemma).mkString(" ")


  def lemma(arg:Arg):String = arg match {
    case arg:TermsArg => lemma(arg.terms)
    case _ => throw new Exception("Nested tuples not supported.")
  }

  def text(terms:Seq[Term]):String = terms.map(_.text).mkString(" ")

  def text(arg:Arg):String = arg match {
    case arg:TermsArg => text(arg.terms)
    case _ => throw new Exception("Nested tuples not supported.")
  }


  def toArgText(arg:Arg): String = arg.text
  def toRelText(rel:Relation): String = rel.terms.map(_.text).mkString(" ")
  def tupleText(tuple:Tuple) = "%s %s %s".format(toArgText(tuple.arg1),
                                                 toRelText(tuple.rel),
                                                 tuple.arg2s.map(toArgText(_)).mkString(" "))

  def from(arg1:String, rel:String, arg2:String) = {
    def arg(text:String) = TermsArg(Seq(Term(text)))
    def relation(text:String) = Relation(Seq(Term(text)))
    Tuple(arg(arg1), relation(rel), Seq(arg(arg2)))
  }

}
