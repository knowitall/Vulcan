package edu.knowitall.vulcan.extraction

import scala.collection.mutable.MutableList
import scala.util.{Try, Success, Failure}
import scala.io.Source

import scopt.immutable.OptionParser

import edu.knowitall.vulcan.openie.OpenIE
import edu.knowitall.vulcan.openie.Instance
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.srl.ClearSrl

import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.chunk.ChunkedToken

import org.slf4j.LoggerFactory

/**
 * Reads sentences, one per line, from stdin, runs openie on them, and loads
 * the resulting extractions into the solr instance running on a configured url.
 */
object Extractor {

  val logger = LoggerFactory.getLogger("Extractor")

  case class Config(val url: Option[String] = None, 
                    val corpus: Option[String] = None, 
                    val file: Option[String] = None,
                    val print: Boolean = false)

  def main(args:Array[String]) {

    val config = parseArgs(args) match {
      case Some(c) => c
      case None => return
    }
    
    val solrLoader = config.url match {
      case Some(u) => Option(new SolrLoader(u, config.corpus))
      case None => None
    }

    val source = config.file match {
      case Some(path) => Source.fromFile(path)(io.Codec.ISO8859)
      case None => Source.fromInputStream(System.in, "UTF-8")
    }

    var sentences = 0
    var loaded: Int = 0

    val openie = new OpenIE(new ClearParser, new ClearSrl)
    val batch = new MutableList[Instance]()

    for(sentence <- source.getLines()) {
      
      sentences += 1
      Try(openie.extract(sentence)) match {
        case Success(instances) => {
          if(config.print) {
            instances foreach ( instance => {
              println("---------------------")
              println(instance.sentence + ":")
                println("\t(" + instance.extraction.arg1 + "; " 
                              + instance.extraction.rel + "; "
                              + instance.extraction.arg2s.mkString("[", "; ", "]") + ") " 
                              + "context: (" + instance.extraction.context.getOrElse("") + "); "
                              + "confidence: " + instance.confidence + "; "
                              + "postags: " + instance.postagsString)
            })
          }

          if(solrLoader.nonEmpty) {
            batch ++= instances
            if(batch.size > 1000) {
              solrLoader.get.load(batch.iterator) match {
                case Success(n) => {
                  loaded += n
                  logger.info("Loaded " + batch.size + " instances to solr")
                }
                case Failure(e) => 
                  logger.error("Failed to load instances: " + e.getMessage)
              }
              batch.clear()
            }
          }
        }
        case Failure(e) => 
          logger.error("Openie extraction from " + sentence + " failed: " + e.getMessage, e)
      }
    }
    if(solrLoader.nonEmpty && batch.size > 0) {
      solrLoader.get.load(batch.iterator) match {
        case Success(n) => loaded += n
        case Failure(e) => 
          logger.error("Failed to load instances: " + e.getMessage, e)
      }
      batch.clear()
    }

    println("Processed " + sentences + " sentences")
    println("Loaded " + loaded + " extractions into solr");

    if(solrLoader nonEmpty) {
      solrLoader.get.shutdown
    }
  }

  def parseArgs(args: Array[String]) : Option[Config] = {

    val parser = new OptionParser[Config]("SolrLoader") {
      def options = Seq(
        opt("corpus", "corpus to label the extractions as coming from") { 
          (s: String, c: Config) => c.copy(corpus = Some(s)) },
        flag("stdout", "print extractions to stdout") { 
          (c: Config) => c.copy(print = true) },
        opt("solr-url", "solr endpoint to load extractions into") { 
          (s: String, c: Config) => c.copy(url = Some(s)) },
        opt("file", "file to read sentences from, one per line") {
          (s: String, c: Config) => c.copy(file = Some(s)) }
      )}

    parser.parse(args, Config()) 
  }
}
