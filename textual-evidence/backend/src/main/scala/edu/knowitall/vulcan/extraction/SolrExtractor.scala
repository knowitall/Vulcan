package edu.knowitall.vulcan.extraction

import scala.io.Source
import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import scala.util.Try

import java.io.File
import java.io.PrintStream
import java.util.{List => JList}

import scopt.immutable.OptionParser

import edu.knowitall.vulcan.openie.{Instance => OpenIE4Instance}

import edu.knowitall.openie.models.Instance
import edu.knowitall.openie.models.ReVerbExtraction

import edu.knowitall.openie.models.serialize.Chill

import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.SolrQuery

import org.slf4j.LoggerFactory

/**
 * Given an input term (or terms), will query against a source solr instance
 * to retrieve relations featuring the input term.  It then extracts the source
 * sentences from those relations, passes those sentences through openie 4, and
 * loads the resulting extractions into the sink solr instance.
 */
object SolrExtractor {

  val logger = LoggerFactory.getLogger(this.getClass)

  case class Config(val sourceSolrUrl: String = "",
                    val termsFile: Option[String] = None,
                    val outputDir: Option[String] = None)


  def main(args:Array[String]) {

    val config = parseArgs(args) match {
      case Some(c) => c
      case None => return
    }
    
    val solrSource = new HttpSolrServer(config.sourceSolrUrl)

    val termSource = config.termsFile match {
      case Some(path) => Source.fromFile(path)(io.Codec.ISO8859)
      case None => Source.fromInputStream(System.in, "UTF-8")
    }

    val deserializer = Chill.createBijection()

    val batchSize = 1000
    var output : PrintStream = null

    for(term <- termSource.getLines()) {
      
      output = config.outputDir match {
        case Some(path) => {
          Try(output.close())
          new PrintStream(new File(path, term.replaceAll("[^a-zA-Z]", "_")), "UTF-8")
        }
        case None => System.out
      }

      Thread.sleep(100) 

      val start = System.currentTimeMillis

      val query = new SolrQuery("text:" + "\"" + term + "\"").setFields("text", "instances").setSortField("size", SolrQuery.ORDER.desc).setRows(batchSize)

      var continue = true
      var count = 0
      var failures = 0
      var sentences = 0

      while(continue) {

        try {

          query.setStart(count)
          val results = solrSource.query(query).getResults()

          if(results.getNumFound() == 0) {

            logger.info("Found 0 matching reverb extractions for '" + term + "'")
            continue = false

          } else {

            for(doc <- results.asScala) {
    
              val text = doc.getFieldValue("text").asInstanceOf[JList[String]].asScala.mkString(" ")
              val bytes = doc.getFieldValue("instances").asInstanceOf[Array[Byte]]
              val instances = deserializer.invert(bytes).asInstanceOf[List[Instance[ReVerbExtraction]]]
              for(sentence <- instances.map { _.extraction.sentenceText } ) {
                output.println(sentence)
                sentences += 1
              }
              //logger.info("Matched rel " + text + " to query " + term)
            }
            count += results.size()
            if (count > 10000 || results.size() < batchSize) continue = false
            failures = 0
          }

        } catch { case e: Exception => {

            failures += 1
            if(failures > 10) {
              System.exit(1)
            }

            logger.error("Failed: " + e.getMessage, e)
            Thread.sleep(60000);
          }
        }
      }

      logger.info("Extracted " + count + " documents with " + sentences + " sentences match " + term + " in " + (System.currentTimeMillis - start) + " ms");

    }
  }

  def parseArgs(args: Array[String]) : Option[Config] = {

    val parser = new OptionParser[Config]("SolrExtractor") {
      def options = Seq(
        opt("solr-source-url", "(required) solr endpoint to query terms from") { 
          (s: String, c: Config) => c.copy(sourceSolrUrl = s) },
        opt("terms", "path to a file containing terms to query source for, one per line") { 
          (s: String, c: Config) => c.copy(termsFile = Some(s)) },
        opt("output", "directory to write sentences files to, one per term") { 
          (s: String, c: Config) => c.copy(outputDir = Some(s)) }
      )}

    val config = parser.parse(args, Config())
    config match {
      case Some(c) =>
        // scopt's missing support for required named options, so check sourceSolrUrl explicitly
        if(c.sourceSolrUrl.equals("")) {
          parser.showUsage
          None
        } else if(c.outputDir.nonEmpty && !(new File(c.outputDir.get).isDirectory)) {
          parser.showUsage
          None
        } else {
          config
        }
      case None => None
    }
  }
}
