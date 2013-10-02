package edu.knowitall.vulcan.extraction

/**
 * Loads openie extractions into a running Solr instance.
 *
 * This is cribbed heavily from the openie-backend/populator, but has been munged
 * to work with openie-4 extractions as opposed to the ReVerbExtractionGroups the 
 * backend populator uses.
 */

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.duration.Duration

import scala.util.{Try, Success, Failure}
import scala.collection.mutable.MutableList
import scala.io.Source
import scala.io.BufferedSource

import edu.knowitall.vulcan.common.Tuple
import edu.knowitall.vulcan.common.TermsArg
import edu.knowitall.vulcan.common.Relation
import edu.knowitall.vulcan.common.Term

import edu.knowitall.vulcan.common.Extraction
import edu.knowitall.vulcan.common.serialization.TupleSerialization
import edu.knowitall.vulcan.common.serialization.TupleSerialization._
import edu.knowitall.vulcan.common.serialization.ExtractionSerialization

import java.io.File

import play.api.libs.json.Json

import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrServer
import org.apache.solr.common.SolrInputDocument

import org.slf4j.LoggerFactory

class SolrLoader(urlString: String, 
                 source: BufferedSource,
                 batchSize: Int,
                 threads: Int)
{

  val logger = LoggerFactory.getLogger(this.getClass)

  val solr = //new ConcurrentUpdateSolrServer(urlString, batchSize, threads)
             new HttpSolrServer(urlString)

  def toSolrDocument(extraction: Extraction) = {

    val doc = new SolrInputDocument()

    doc.setField("id", extraction.id)
    doc.setField("corpus", extraction.corpus)
    doc.setField("tuple", Json.stringify(Json.toJson(extraction.tuple)))
    doc.setField("sentence", extraction.sentence)
    doc.setField("confidence", extraction.confidence)
    doc.setField("sentence_details", Json.stringify(Json.toJson(extraction.sentenceDetails)))

    doc
  }

  def load(extractions: Iterable[Extraction]) : Try[Int] = {
    var n = 0
    Try({ 
      extractions.foreach(extraction => {
        solr.add(toSolrDocument(extraction))
        n += 1
      })
      solr.commit()
      n
    })
  }

  def run() = {

    val extractions : Iterator[String] = source.getLines()

    /**
     * starts a future which sits in a loop loading Extraction Tuples in batches of batchSize
     * for as long as there are any
     */
    def loaderLoop() = {
      future {
        var loaded: Int = 0
        val batch = new MutableList[Extraction]()
        
        while(extractions.hasNext) {
          extractions.synchronized {
            while(extractions.hasNext && batch.size < batchSize) {
              val extractionJson = extractions.next()
              val extraction = ExtractionSerialization.fromJson(extractionJson)
              batch += extraction 
            }
          }

          if(batch.size > 0) {
            load(batch) match {
              case Success(n: Int) => {
                loaded += n
                logger.info("loaded " + batch.size + " extractions")
              }
              case Failure(x) => logger.error("failed to load extract batch: " + x.getMessage, x)
            }
            batch.clear()
          }
        }
        loaded
      }
    }
              
    val loaders = for(i <- 0 until threads) yield loaderLoop()

    var total: Int = 0
    for(loader <- loaders) {
      val loaded = Await.result(loader, Duration.Inf) 
      total += loaded
      logger.info("loader loaded " + loaded + " extractions")
    }

    logger.info("All loaders loaded " + total + " total extractions")

    //solr.blockUntilFinished()
    solr.shutdown()
  }
}

object SolrLoaderMain {

  val logger = LoggerFactory.getLogger("SolrLoaderMain")

  case class Config(val solrUrl: String = "",
                    val source: BufferedSource = Source.stdin,
                    val batchSize: Int = 1000,
                    val threads: Int = 1)

  def parseArgs(args: Array[String]) : Option[Config] = {

    val parser = new scopt.immutable.OptionParser[Config]("SolrLoaderMain") {

      def options = Seq(
        opt("solr-url", "required: solr instance to load extractions into") { 
          (param, c) => c.copy(solrUrl = param)
        }, 

        opt("input-file", "where to read input sentences, default stdin") { 
          (param, c) => c.copy(source = Source.fromFile(new File(param))) 
        }, 

        intOpt("threads", "number of threads to upload with, default 1") {
          (param, c) => c.copy(threads = param)
        }, 

        intOpt("batch-size", "number docs to batch-write to solr at a time, default 1000") {
          (param, c) => c.copy(threads = param)
        } 
      )
    }

    parser.parse(args, Config()) match {
      case Some(c) => {
        if(c.solrUrl == "") {
          parser.showUsage
          None
        } else {
          Some(c)
        }
      }
      case None => None
    }
  }

  def main(args:Array[String]) {

    val config: Config = parseArgs(args) match {
      case Some(c) => c
      case None => return
    }

    val solrLoader = new SolrLoader(config.solrUrl, 
                                    config.source,
                                    config.batchSize,
                                    config.threads)

    solrLoader.run()
  }
}
