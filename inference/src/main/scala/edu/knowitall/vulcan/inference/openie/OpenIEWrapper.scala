package edu.knowitall.vulcan.inference.openie


import edu.knowitall.vulcan.extraction.Extractor

object OpenIEWrapper{

  //val openie= new OpenIE(new ClearParser, new ClearSrl)

  def instance(wordnetHome:String, parserServer:Option[String] = None, srlServer:Option[String] = None) = new Extractor(wordnetHome, parserServer, srlServer)

  //def extract(sentence:String) = openie.extract(sentence)

  def main(args:Array[String]){
    println("Sentence: ")
    val parser = if(args.size > 1) Some(args(1)) else None
    val srl = if(args.size > 2) Some(args(2)) else None
    val extractor = instance(args(0), parser, srl)
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