package edu.knowitall.vulcan.inference.mln

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 8/13/13
 * Time: 2:35 PM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import edu.knowitall.vulcan.inference.kb._
import scala.collection.mutable.ArrayBuffer
import edu.knowitall.vulcan.inference.kb.Predicate
import java.io.{PrintWriter, File}
import edu.knowitall.vulcan.inference.evidence.Evidence
import edu.knowitall.vulcan.common.{Arg, Tuple}

case class MLNInstance(query:Seq[Axiom],
                       predicateDefinitions:Seq[Predicate],
                       rules:Seq[WeightedRule],
                       evidence:Iterator[Axiom]) {



}

object MLNInstanceIO{
  val logger = LoggerFactory.getLogger(this.getClass)

  def toDefinitionTuple(tuple:Tuple) = {
    val arg1 = new Arg()
    new Tuple(arg1, rel, arg2s)
  }
  def toDefinitionPredicate(predicate:Predicate) = {
    val relation = predicate.tuple.rel
    new Predicate(new Tuple("entity", relation, "entity"), 1.0)
  }
  def fromEvidence(evidence:Evidence) = {
    val query = Axiom.fromProposition(evidence.proposition)::Nil
    val predicateDefinitions = evidence.axioms.flatMap(axiom => axiom.antecedents.map(x => toDefinitionPredicate(x))).toSet.toSeq
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

    var axioms = Seq[Axiom]()
    var consequent = new Predicate(new BinaryRelationTuple("iron", "typeOf", "metal"), 0.9)
    axioms :+= new Axiom(Seq[Predicate](), consequent, 0.9)
    consequent = new Predicate(new BinaryRelationTuple("iron_nail", "composedOf", "iron"), 1.0)
    axioms :+= new Axiom(Seq[Predicate](), consequent, 0.9)
    val predicateDefinitions = new Predicate(new BinaryRelationTuple("entity", "composedOf", "entity"), 1.0) ::
      new Predicate(new BinaryRelationTuple("entity", "conductorOf", "entity"), 1.0) ::
      new Predicate(new BinaryRelationTuple("entity", "typeOf", "entity"), 1.0) :: Nil

    var program = Seq[WeightedRule]()
    val rule1 = {
      val antecedents = Seq(new Predicate(new BinaryRelationTuple("a1", "typeOf", "metal"), 1.0))
      val consequent = new Predicate(new BinaryRelationTuple("a1", "conductorOf", "a2"), 1.0)
      new WeightedRule(antecedents, consequent, 0.5)
    }

    val rule2 = {
      var antecedents = Seq[Predicate]()
      val ismetal = new Predicate(new BinaryRelationTuple("a2", "typeOf", "metal"), 1.0)
      val composed = new Predicate(new BinaryRelationTuple("a1", "composedOf", "a2"), 1.0)
      antecedents :+= ismetal
      antecedents :+= composed
      val consequent = new Predicate(new BinaryRelationTuple("a1", "conductorOf", "a3"), 1.0)
      new WeightedRule(antecedents, consequent, 0.75)
    }
    program :+= rule1
    program :+= rule2
    val instance = new MLNInstance(evidence, predicateDefinitions, program, axioms.iterator)
    val exported = TuffyFormatter.export(instance)
    println(exported)
  }
  object TuffyFormatter{

    def export(p:Predicate, score:Boolean = false) = {
      val out = "%s(%s, %s)".format(p.tuple.relation, p.tuple.arg1, p.tuple.arg2)
      score match {
        case false => out
        case true => "%.2f %s".format(p.score(), out)
      }
    }

    val disjunction = " v "
    val NL = "\n"

    def exportDisjunctions(seq:Seq[Predicate]) = seq.map(export(_)).mkString(disjunction)

    def export(axiom:WeightedRule): String = {
      axiom.antecedents.isEmpty match {
        case true => "%.2f %s".format(axiom.score(), export(axiom.consequent))
        case false => "%.2f !%s %s %s".format(axiom.score(),export(axiom.consequent),disjunction, exportDisjunctions(axiom.antecedents))

      }

    }

    def exportPredicateDefinitions(seq:Seq[Predicate]) = seq.map(export(_)).mkString("\n")

    def export(seq:Iterator[WeightedRule]): String = {
      seq.map(export(_)).mkString(NL)
    }

    def exportQueryFile(instance:MLNInstance, file:File) = {
      val output = export(instance.query.iterator)
      val writer = new PrintWriter(file)
      writer.println(output)
      writer.close
    }

    def exportEvidenceFile(instance:MLNInstance, file:File) = {
      val output = export(instance.evidence)
      val writer = new PrintWriter(file)
      writer.println(output)
      writer.close
    }

    def exportProgramFile(instance:MLNInstance, file:File) = {
      val output = exportPredicateDefinitions(instance.predicateDefinitions) + "\n\n" + export(instance.rules.iterator)
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
      "//Axioms\n" + export(instance.evidence) + "\n" +
      "//Predicate definitions\n"  + exportPredicateDefinitions(instance.predicateDefinitions) + "\n" +
      "//Program\n" + export(instance.rules.iterator) + "\n" +
      "//Evidence\n" + export(instance.query.iterator)

      /**"//Axioms\n%s\n//Predicate Definitions\n%s\n//Program\n%s\n//Evidence\n%s".format(export(instance.axioms),
        exportPredicateDefinitions(instance.predicateDefinitions),
        export(instance.program.iterator),
        export(instance.evidence.iterator))*/
    }
  }
}
