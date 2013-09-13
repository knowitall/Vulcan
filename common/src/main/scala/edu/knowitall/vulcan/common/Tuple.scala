package edu.knowitall.vulcan.common

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

case class Term(text: String, 
                lemma: Option[String] = None, 
                postag: Option[String] = None, 
                chunk: Option[String] = None)

case class Relation(terms: Seq[Term], 
                    negated: Boolean = false, 
                    passive: Boolean = false)

trait Arg { } // trait marking types that can be used as arg1/arg2s

case class TermsArg(terms: Seq[Term], 
                    headword: Option[Seq[Term]] = None) 
  extends Arg { }

case class Tuple(arg1: Arg, 
                 rel: Relation, 
                 arg2s: Seq[Arg] = Nil, 
                 context: Option[String] = None) 
  extends Arg { }
