package edu.knowitall.vulcan.inference.evidence

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 8/14/13
 * Time: 9:47 AM
 * To change this template use File | Settings | File Templates.
 */
import edu.knowitall.vulcan.inference.proposition.Proposition
import edu.knowitall.vulcan.inference.kb.Axiom

abstract class AxiomsFinder {
  def find(proposition:Proposition): Seq[Axiom]
}
