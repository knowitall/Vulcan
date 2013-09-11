package edu.knowitall.vulcan.extraction

/**
 * Loads openie extractions into a running Solr instance.
 *
 * This is cribbed heavily from the openie-backend/populator, but has been munged
 * to work with openie-4 extractions as opposed to the ReVerbExtractionGroups the 
 * backend populator uses.
 */

import scala.collection.JavaConverters.asJavaIteratorConverter
import scala.util.{Try, Success, Failure}

import edu.knowitall.common.Timing
import edu.knowitall.vulcan.openie.Instance

import java.util.concurrent.atomic.AtomicInteger

import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.common.SolrInputDocument

import org.slf4j.LoggerFactory

class SolrLoader(urlString: String, corpus: Option[String]) {

  val logger = LoggerFactory.getLogger(classOf[SolrLoader])

  val solr = new HttpSolrServer(urlString)
  val id = new AtomicInteger(0)

  def toSolrDocument(instance: Instance) = {

    val doc = new SolrInputDocument()
    val extraction = instance.extraction

    val ids = (if(corpus.nonEmpty) corpus.get + "_" else "") + id.getAndIncrement()

    doc.setField("id", ids)
    doc.setField("arg1", extraction.arg1)
    doc.setField("rel", extraction.rel)

    extraction.arg2s foreach(arg2 => doc.addField("arg2s", arg2))

    extraction.context match {
      case Some(c) => doc.setField("context", c)
      case None => ;
    }
    doc.setField("confidence", instance.confidence)

    doc.setField("sentence_text", instance.sentence)

    if(corpus nonEmpty) {
      doc.setField("corpus", corpus.get)
    }

    doc.setField("passive", extraction.passive)
    doc.setField("negation", extraction.negated)
    doc.setField("postags", instance.postagsString)

    doc
  }

  def load(instance: Instance) : Try[Int] = {
    load(Seq(instance).iterator)
  }

  def load(instances: Iterator[Instance]) : Try[Int] = {
    var n = 0
    Try({ 
      instances.foreach(instance => {
        solr.add(toSolrDocument(instance))
        n += instance.extraction.arg2s.length
      })
      solr.commit()
      n
    })
  }

  def shutdown = solr.shutdown()
}
