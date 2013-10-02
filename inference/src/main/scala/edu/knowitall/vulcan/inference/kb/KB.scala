package edu.knowitall.vulcan.inference.kb

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/27/13
 * Time: 10:16 AM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import edu.knowitall.vulcan.common.{Arg, Tuple}
import org.slf4j.LoggerFactory
import edu.knowitall.vulcan.inference.utils.{Tokenizer, TupleHelper}
import edu.knowitall.vulcan.inference.matching.CNCClient

abstract class  KB {

  def relations:Seq[String]

 /** /**
   * Entries that match the query string.
   * @param query
   * @param queryRelations
   * @return
   */
  def matchingEntries(query:String, queryRelations:Seq[String]):Seq[Axiom]*/

  /**
   *  Entries that match the query string.
   * @param query
   * @param queryRelations
   * @return
   */
  def matchingEntries(query:Tuple, queryRelations:Seq[String] = Nil):Seq[Axiom]

}

object KBQueriesHelper{

  import TupleHelper._
  import Tokenizer._

  def queries(tuple:Tuple) = {
    def tokens(arg:Arg): Seq[String] = tokenize(text(tuple.arg1))
    text(tuple.arg1)::Nil ++ tuple.arg2s.map(text(_)) ++ tokens(tuple.arg1) ++ tuple.arg2s.flatMap(tokens)
  }

}

class WordnetKB extends KB {

  val logger = LoggerFactory.getLogger(this.getClass)

  val _relations = "type of"::Nil

  def relations = _relations

  /**
   * Entries that match the query string.
   * @param query
   * @param queryRelations
   * @return
   *
  def matchingEntries(query: String, queryRelations: Seq[String]) = matchingEntries(Tuple.makeTuple("iron", "type of", "metal")) */

  /**
   * Entries that match the query string.
   * @param query
   * @param queryRelations
   * @return
   */
  def matchingEntries(query: Tuple, queryRelations: Seq[String]) = {
    logger.info("Mock implementation. Returns single relation for type of.")
    val tokens = KBQueriesHelper.queries(query)
    logger.info("Tokens: " + tokens.mkString("\n"))
    Seq(Axiom.fromTuple(TupleHelper.from("iron", "type of", "metal")))
  }
}

class CNCategorizerKB(host:String, port:Int) extends KB{


  val logger = LoggerFactory.getLogger(this.getClass)
  val relationMap = ("substance-material-ingredient" -> "composed of"::Nil).toMap
  val _relations = "composed of"::Nil
  val client = new CNCClient(host, port)

  def relations = _relations

  /**
   * Entries that match the query string.
   * @return
   *
  def matchingEntries(query: String, queryRelations: Seq[String]): Seq[Axiom] = {
    matchingEntries(Tuple.makeTuple("iron nail", "composed of", "iron"))
  } */

  def cncTuple(string:String) = {
    val words = string.split(" ")
    val head: String = words.head
    val tail: String = words.last
    client.parse(string) match {
      case Some(relation:String) => {
        logger.info("Using relation: "  + relation)
        val name = relationMap.getOrElse(relation, relation)
        Some(TupleHelper.from(head, name, tail))
      }
      case _ => None
    }
  }

  /**
   * Entries that match the query string.
   * @param query
   * @param queryRelations
   * @return
   */
  def matchingEntries(query: Tuple, queryRelations: Seq[String]) = {
    logger.info("Mock implementation. Returns single relation for composed of.")
    val tokens = KBQueriesHelper.queries(query)
    val cncEntries = tokens.flatMap(token => {
      cncTuple(token) match {
        case Some(tuple:Tuple) => Some(Axiom.fromTuple(tuple))
        case None => None
      }
    })
    cncEntries.foreach(entry => logger.info("CNC entry: " + TupleHelper.lemma(entry.consequent.tuple)))
    cncEntries
    //Seq(Axiom.fromTuple(Tuple.makeTuple("iron nail", "composed of", "iron")))
  }

}
