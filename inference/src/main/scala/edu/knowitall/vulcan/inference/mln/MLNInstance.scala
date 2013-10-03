package edu.knowitall.vulcan.inference.mln

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 8/13/13
 * Time: 2:35 PM
 * To change this template use File | Settings | File Templates.
 */

import edu.knowitall.vulcan.inference.kb._
import edu.knowitall.vulcan.inference.kb.Predicate
import edu.knowitall.vulcan.inference.utils.TupleHelper

case class MLNInstance(query:Seq[Axiom],
                       predicateDefinitions:Seq[Predicate],
                       rules:Seq[WeightedRule],
                       evidence:Iterator[Axiom]) {


  def wildcardQueryVariants = query.flatMap(q => {
    TupleHelper.wildcardVaiants(q.consequent.tuple)
               .map(variant => Axiom.fromTuple(variant, 1.0))
  })
}


