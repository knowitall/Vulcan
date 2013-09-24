package edu.knowitall.vulcan.inference.mln.tuffyimpl

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/23/13
 * Time: 2:53 PM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import edu.knowitall.vulcan.inference.kb.{WeightedRule, Axiom, Predicate}
import scala.collection.mutable.ArrayBuffer
import edu.knowitall.vulcan.inference.utils.TupleHelper
import edu.knowitall.vulcan.common.{Relation, Term, TermsArg, Tuple}
import scala.Some
import edu.knowitall.vulcan.inference.evidence.Evidence
import edu.knowitall.vulcan.inference.mln.MLNInstance


object MLNInstanceIO{
  val logger = LoggerFactory.getLogger(this.getClass)

  def entityArg = TermsArg(Seq(Term("entity", Some("entity"))))

  def entityRelation = Relation(Seq(Term("entity", Some("entity"))))

  def toDefinitionTuple(tuple:Tuple) = new Tuple(entityArg, entityRelation, Seq(entityArg))

  def toDefinitionPredicate(predicate:Predicate) = new Predicate(toDefinitionTuple(predicate.tuple), 1.0)

  def fromEvidence(evidence:Evidence) = {
    val query = Axiom.fromProposition(evidence.proposition)::Nil

    val predicateDefinitions = evidence.axioms.flatMap(axiom => {
      toDefinitionPredicate(axiom.consequent)::Nil ++ axiom.antecedents.map(x => toDefinitionPredicate(x))
    }).toSet.toSeq


    logger.info("Predicate Definitions: " + predicateDefinitions.map(x => x.toString()).mkString("\n"))
    val rules = evidence.rules
    val evid = evidence.axioms.iterator
    new MLNInstance(query, predicateDefinitions, rules, evid)
  }
  /**
   * conductor(entity, entity)
     composed(entity, entity)
     Metal(entity)

     //Rules
     0.5 !Metal(a1) v conductor(a1, a2)
     0.75 !composed(a1, a2) v !Metal(a2) v conductor(a1, a3)

   */
  def main(args:Array[String]){
    val evidence = new ArrayBuffer[Axiom]()

    import TupleHelper._
    var axioms = Seq[Axiom]()
    var consequent = new Predicate(from("iron", "typeOf", "metal"), 0.9)
    axioms :+= new Axiom(Seq[Predicate](), consequent, 0.9)
    consequent = new Predicate(from("iron_nail", "composedOf", "iron"), 1.0)
    axioms :+= new Axiom(Seq[Predicate](), consequent, 0.9)
    val predicateDefinitions = new Predicate(from("entity", "composedOf", "entity"), 1.0) ::
      new Predicate(from("entity", "conductorOf", "entity"), 1.0) ::
      new Predicate(from("entity", "typeOf", "entity"), 1.0) :: Nil

    var program = Seq[WeightedRule]()
    val rule1 = {
      val antecedents = Seq(new Predicate(from("a1", "typeOf", "metal"), 1.0))
      val consequent = new Predicate(from("a1", "conductorOf", "a2"), 1.0)
      new WeightedRule(antecedents, consequent, 0.5)
    }

    val rule2 = {
      var antecedents = Seq[Predicate]()
      val ismetal = new Predicate(from("a2", "typeOf", "metal"), 1.0)
      val composed = new Predicate(from("a1", "composedOf", "a2"), 1.0)
      antecedents :+= ismetal
      antecedents :+= composed
      val consequent = new Predicate(from("a1", "conductorOf", "a3"), 1.0)
      new WeightedRule(antecedents, consequent, 0.75)
    }
    program :+= rule1
    program :+= rule2
    val instance = new MLNInstance(evidence, predicateDefinitions, program, axioms.iterator)
    val exported = TuffyFormatter.export(instance)
    println(exported)
  }

}
