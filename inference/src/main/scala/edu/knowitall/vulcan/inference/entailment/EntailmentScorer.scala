package edu.knowitall.vulcan.inference.entailment

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 10/3/13
 * Time: 10:58 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import edu.knowitall.vulcan.common._
import edu.knowitall.vulcan.inference.utils.Tokenizer
import org.slf4j.LoggerFactory
import edu.knowitall.vulcan.common.Term
import edu.knowitall.vulcan.common.TermsArg

class EntailmentScorer {

  val logger = LoggerFactory.getLogger(this.getClass)

  def scoreText(t:Tuple, h:Tuple):Double = {
    val ttext = t.text
    val htext = h.text
    scoreText(ttext, htext)
    //hypothesisLikelihood(ttext, htext)
  }

  def hypothesisLikelihood(ttext:String, htext:String):Double = {
    val ttoks = Tokenizer.tokenize(ttext).toSeq
    val htoks = Tokenizer.tokenize(htext).toSeq
    val lambda = 0.7

    val logsum = htoks.map(htok => {
      val ql = if(ttoks.contains(htok)) 1.0/(ttoks.size.toDouble + 10.0) else 0.0
      val p = (1-lambda) * 0.01 + lambda * ql
      Math.log(p)
    }).sum
    Math.exp(logsum)
  }


  def scoreText(ttext: String, htext: String): Double = {
    //hypothesisLikelihood(ttext, htext)
    val ttoks = Tokenizer.tokenize(ttext).toSeq
    val htoks = Tokenizer.tokenize(htext).toSeq
    val common = ttoks.intersect(htoks).size
    val union = (htoks ++ ttoks).size
    val score = if (union > 0) common.toDouble / union else 0.0
    //logger.info("%s => %s = %.2f".format(ttext, htext, score))
    //logger.info("Common: %s".format(htoks intersect ttoks))
    score
  }

  //Does t entail h?
  def score(t:Tuple, h:Tuple) = {
    def termScore(tterms:Seq[Term], hterms:Seq[Term]) = {
      val tset = tterms.map(_.text.replaceAll("\"", " ")).toSet
      val hset = hterms.map(_.text.replaceAll("\"", " ")).toSet
      val union = {tset union hset}.size
      val overlap = {tset intersect hset}.size
      if(union > 0) {
        overlap.toDouble/union.toDouble
      }else{
        0.0
      }
    }
    def terms(arg:Arg): Seq[Term] = arg match {
      case termsArg:TermsArg => termsArg.terms
      case tuple:Tuple => terms(tuple.arg1) ++ tuple.rel.terms ++ tuple.arg2s.flatMap(terms(_))
    }
    def argMatch(targ:Arg, harg:Arg) = {
      termScore(terms(targ), terms(harg))
    }
    def relMatch(trel:Relation, hrel:Relation) = {
      termScore(trel.terms, hrel.terms)
    }
    def arg2sMatch(targs:Seq[Arg], hargs:Seq[Arg]) = {
      val tterms = targs.flatMap(terms(_))
      val hterms = hargs.flatMap(terms(_))
      termScore(tterms, hterms)
    }
    0.33 * argMatch(t.arg1, h.arg1) + 0.33 * relMatch(t.rel, h.rel) + 0.33 * arg2sMatch(t.arg2s, h.arg2s)

  }

}
