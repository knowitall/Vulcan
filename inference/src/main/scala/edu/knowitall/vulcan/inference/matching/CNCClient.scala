package edu.knowitall.vulcan.inference.matching

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/25/13
 * Time: 10:29 PM
 * To change this template use File | Settings | File Templates.
 */


import scala.Predef._
import tratz.cmdline.{CommandLineOptions, CommandLineOptionsParser, ParsedCommandLine}
import tratz.parse.{SimpleParseClient, SimpleParseServer, FullSystemWrapper}
import tratz.parse.io.SentenceReader
import tratz.parse.io.SentenceWriter
import tratz.parse.types.{Token, Arc, Parse, Sentence}
import scala.Predef.String
import java.io._

import scala.collection.JavaConversions._
import scala.collection.immutable.HashMap

import tratz.parse.util.ParseConstants
import tratz.pos.PosTagger

class CNCClient(host:String, port:Int){

  val client: SimpleParseClient = new SimpleParseClient(host, port)

  def parse(phrase:String) = {

    val sb = new StringBuilder
    try {

      val sentence: Sentence = new Sentence(PosTagger.makeMeSomeTokens(phrase.split("\\s+")))
      var request: SimpleParseServer.ParseRequest = null
      request = new SimpleParseServer.ParseRequest(sentence)
      val result: SimpleParseServer.ParseResult = client.sendRequest(request)
      if (result.getException != null) {
        System.err.println("Blah... something wrong")
        result.getException.printStackTrace
      }
      else {
        val fullResult: FullSystemWrapper.FullSystemResult = result.getResult
        val syntacticParse: Parse = fullResult.getParse
        val srlLinks: Parse = fullResult.getSrlParse
        val srlArcs: Array[Arc] = if (srlLinks == null) null else srlLinks.getHeadArcs
        val sentenceCopy: Sentence = syntacticParse.getSentence
        import scala.collection.JavaConversions._
        for (t <- sentenceCopy.getTokens) {
          //val arcHead: Arc = syntacticParse.getHeadArcs()(t.getIndex())
          //val srlArc: Arc = if (srlArcs == null) null else srlArcs(t.getIndex)
          val lexSense = t.getLexSense
          if(lexSense != null) sb.append(t.getLexSense)
          //sb.append(t.getIndex + "\t" + t.getText + "\t" + t.getPos + "\t" + arcHead.getDependency + "\t" +
          //  arcHead.getHead.getIndex + "\t" + (if (t.getLexSense == null) "_" else t.getLexSense) + "\t" + (if (srlArc == null) "_" else srlArc.getSemanticAnnotation) + "\t" + (if (srlArc == null) "_" else srlArc.getHead.getIndex))
          sb.append("\n")
        }
      }
    }

    sb.toString
  }

}

object CNCClient{
  def main(args:Array[String]) {
    val client = new CNCClient(args(0), args(1).toInt)
    print("Enter text: ")
    for( ln <- io.Source.stdin.getLines ) {
      println(client.parse(ln))
      println
      println
      print("Enter text: ")
    }
  }
}

