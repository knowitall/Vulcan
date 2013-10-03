package edu.knowitall.vulcan.evidence

import edu.knowitall.vulcan.common.Tuple
import edu.knowitall.vulcan.evidence.query.TupleQuery
import edu.knowitall.vulcan.evidence.query.QueryBuilder._

import scala.collection.JavaConversions.iterableAsScalaIterable

import scopt.immutable.OptionParser

import edu.knowitall.vulcan.common.serialization.ExtractionSerialization

object TextualEvidenceCLI extends App {

  case class Config(val endpoint: String = "",
                    val arg1: String = "",
                    val rel: String = "",
                    val arg2: String = "")

  def parseArgs(args: Array[String]) : Config = {

    val parser = new OptionParser[Config]("TextualEvidenceCLI") {
      def options = Seq(
        opt("endpoint-url", "(required) textual evidence service endpoint") { 
          (s: String, c: Config) => c.copy(endpoint = s) },
        opt("arg1", "arg1") { 
          (s: String, c: Config) => c.copy(arg1 = s) },
        opt("rel", "rel") { 
          (s: String, c: Config) => c.copy(rel = s) },
        opt("arg2", "arg2") { 
          (s: String, c: Config) => c.copy(arg2 = s) }
      )}

    val config = parser.parse(args, Config())
    config match {
      case Some(c) =>
        if(c.endpoint == "") {
          parser.showUsage
          sys.exit()
        } else {
          if(Seq(c.arg1, c.rel, c.arg2).mkString.isEmpty()) {
            System.err.println("At least one of --arg1, --rel, --arg2, and is required")
            parser.showUsage
            sys.exit()
          } 
          c
        }
      case None => sys.exit()
    }
  }

  val c = parseArgs(args) 
  val (endpoint: String, 
       arg1: String, 
       rel: String, 
       arg2: String) =  
    (c.endpoint, c.arg1, c.rel, c.arg2)

  val client = new TextualEvidenceClient(endpoint)
  val tuple = Tuple.makeTuple(arg1, rel, arg2)

  val resultPage = client.query(TupleQuery.forTuple(tuple))

  resultPage.results.foreach(result => {
    val extraction = ExtractionSerialization.toJson(result.extraction, true)
    println(extraction)
  })
}
