package edu.knowitall.vulcan.common

import edu.knowitall.tool.tokenize.ClearTokenizer

/**
 * Tuple defines the common representation of an arg1-rel-arg2 relation throughout the system,
 * it's built up from a type hierarchy.  
 *
 * At the bottom of the hierarcy is a Term.  A term is a piece of raw text with some associated
 * optional metadata like a lemmatized version, pos tag, and chunk.  (Whether all that metadata
 * should be optional or not, I'm not sure.)  All other types are ultimately composed of Seq[Term].
 * 
 * A Relation is the type used for the REL.  It's composed of a Seq[Term] with some Booleans to 
 * indicate if the relation is negated or is passive. (Should polarity go here as well?)
 * 
 * An Arg is a trait marking a type that can serve as an arg (either arg1 or arg2).  
 * There are two types of trait Arg:
 *
 * - TermsArg is a "simple" Arg type composed of a Seq[Term], with an Option[Term] headword.
 * - Tuple is also an Arg, which allows Tuples to be nested.
 * 
 * A Tuple is at the top of the hierarchy, and is composed of an Arg (arg1), a Relation (rel), a
 * Seq[Arg] of arg2s, and an optional context.  
 *
 * - arg1 will probably usually be a TermsArg, but could be a Tuple, for example:
 *     ((greg, ate, [the whole pie]), caused, [stomach ache])
 * 
 * - arg2s may be a sequence of TermsArg of any length, for example:
 *     n = 0 : (bees, fly, []), 
 *     n = 1 : (bees, eat, [honey]), 
 *     n > 1 : (bees, collect, [pollen, from flowers, in the woods]).
 *
 * - arg2s can also be whole Tuples, for example:
 *     (sense of smell, helps, [(a fox, find, [food, in the woods])]).  
 *   In this case there will probably only be a single arg2.
 *
 * - context comes from OpenIE-4.0.  I'm not sure exactly what it represents.
 */

/**
 * Term is just a String token with some Option add-ons: lemma, postag, chunk
 */
case class Term(text: String, 
                lemma: Option[String] = None, 
                postag: Option[String] = None, 
                chunk: Option[String] = None)
{
  def hasDetails() : Boolean = {
    !(lemma.isEmpty || postag.isEmpty || chunk.isEmpty) 
  }

  lazy val details : Option[(String,String,String)] = {
    if(hasDetails()) {
      Some((lemma.get, postag.get, chunk.get))
    } else {
      None
    }
  }
}

/**
 * Relation is a Seq of Terms with Option head Terms, and negation and passive indicators
 */
case class Relation(terms: Seq[Term], 
                    headword: Option[Seq[Term]] = None,
                    negated: Boolean = false, 
                    passive: Boolean = false) 
{
  lazy val text = terms map { _.text } mkString(" ")
}

/**
 * Arg is a trait marking types that can be used as arg1 and arg2s
 */
sealed trait Arg { 
  def text: String
}

/**
 * TermsArg is an Arg that consists of a Sequence of Terms, with Option headword Terms
 */
case class TermsArg(terms: Seq[Term], 
                    headword: Option[Seq[Term]] = None) 
  extends Arg
{ 
  lazy val text = terms map { _.text } mkString(" ")
}

/**
 * Tuple is an arg1 - rel - arg2s relation, with Option context Terms.  Tuple is also
 * an Arg, that is it can serve the place of an Arg to another Tuple.
 */
case class Tuple(arg1: Arg, 
                 rel: Relation, 
                 arg2s: Seq[Arg] = Nil, 
                 context: Option[Seq[Term]] = None)  
  extends Arg 
{ 
  lazy val text = arg1.text + " " + 
                  rel.text + " " + 
                  arg2s.map { _.text } mkString(" ")
}

/**
 * Include some utility helpers for making tuples out of plain strings
 */
object Tuple {
 
  private val tokenizer = new ClearTokenizer()

  /**
   * Utility constructor for making a Tuple out of bare Strings 
   * for arg1, rel, and arg2s
   */
  def makeTuple(arg1: String, rel: String, arg2s: String*) : Tuple = {
    Tuple(TermsArg(makeTerms(arg1)),
          Relation(makeTerms(rel)),
          arg2s map { arg2 => TermsArg(makeTerms(arg2)) })
  }

  private def makeTerms(text: String) : Seq[Term] = {
    tokenizer.tokenize(text) map { _.string } map { string => Term(string) }
  }
}
