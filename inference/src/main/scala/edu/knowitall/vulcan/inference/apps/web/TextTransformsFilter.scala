package edu.knowitall.vulcan.inference.apps.web

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/10/13
 * Time: 3:28 PM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import scopt.mutable.OptionParser

import unfiltered.request.{GET, Path, Method}
import unfiltered.response.ResponseString
import edu.washington.cs.knowitall.morpha.MorphaStemmer
import edu.knowitall.vulcan.inference.matching.{HeadExtractor, SynonymsManager, Stemmer}

object TextTransformsFilter {

  val logger = LoggerFactory.getLogger(this.getClass)

  var stemmer = new Stemmer
  var synMgr = new SynonymsManager
  var headExtractor = new HeadExtractor

  val intentVal = unfiltered.netty.cycle.Planify {
    case req @ GET(Path("/stem")) =>{
      val response = ReqHelper.getValue(req, "x") match {
        case Some(x) => stemmer.stem(x)
        case _ => ""
      }
      ResponseString(response)
    }
    case req @ GET(Path("/sameas")) =>{
      val response = (ReqHelper.getValue(req, "x"), ReqHelper.getValue(req, "y")) match {
        case (Some(x), Some(y)) => synMgr.areSynonyms(x, y).toString
        case _ => false.toString
      }
      ResponseString(response)
    }
    case req @ GET(Path("/head")) =>{
      val response = ReqHelper.getValue(req, "x") match {
        case Some(x) => headExtractor.head(x)
        case _ => ""
      }

      ResponseString(response)
    }

  }
  def main(args:Array[String]){

    var port = 8088
    val parser = new OptionParser() {
      arg("port", "Port to run on.", {str => port = str.toInt})
    }

    if(parser.parse(args)){
      unfiltered.netty.Http(port).plan(intentVal).run()
    }else{
      println(parser.usage)
    }
  }

}
