package edu.knowitall.vulcan.inference.openie


import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.srl.ClearSrl
import edu.knowitall.openie.{OpenIE, Instance}
import edu.knowitall.vulcan.extraction.Extractor

object OpenIEWrapper{

  //val openie= new OpenIE(new ClearParser, new ClearSrl)

  def instance(wordnetHome:String, parserServer:Option[String] = None, srlServer:Option[String] = None) = new Extractor(wordnetHome, parserServer, srlServer)

  //def extract(sentence:String) = openie.extract(sentence)

  def main(args:Array[String]){
    println("Sentence: ")
    val extractor = instance(args(0))
    var count = 0
    for( ln <- io.Source.stdin.getLines ) {
      val instances = extractor.extract(ln, "stdin", () => count.toString)
      println("Extractions:")
      instances.foreach(inst => println(inst.tuple.toString()))
      println()
      println("Sentence: ")
      count = count + 1
    }
  }
}