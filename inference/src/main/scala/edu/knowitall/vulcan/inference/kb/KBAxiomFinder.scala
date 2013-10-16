package edu.knowitall.vulcan.inference.kb

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/27/13
 * Time: 10:16 AM
 * To change this template use File | Settings | File Templates.
 */


import scala.collection.JavaConversions._
import edu.knowitall.vulcan.inference.evidence.AxiomsFinder
import edu.knowitall.vulcan.inference.proposition.Proposition

class KBAxiomFinder(kbs:Seq[KB]) extends AxiomsFinder {

  def find(proposition: Proposition, propText:Option[String] = None) = kbs.flatMap(kb => kb.matchingEntries(proposition.consequent.tuple))

}
