package edu.knowitall.vulcan.inference.mln

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 8/13/13
 * Time: 2:35 PM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import edu.knowitall.vulcan.inference.kb._
import scala.collection.mutable.ArrayBuffer
import java.io.{PrintWriter, File}
import edu.knowitall.vulcan.common.Term
import edu.knowitall.vulcan.common.TermsArg
import scala.Some
import edu.knowitall.vulcan.inference.evidence.Evidence
import edu.knowitall.vulcan.inference.kb.Predicate
import edu.knowitall.vulcan.common.Tuple
import edu.knowitall.vulcan.inference.utils.TupleHelper

case class MLNInstance(query:Seq[Axiom],
                       predicateDefinitions:Seq[Predicate],
                       rules:Seq[WeightedRule],
                       evidence:Iterator[Axiom]) {
}


