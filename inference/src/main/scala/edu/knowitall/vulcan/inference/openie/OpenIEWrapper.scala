package edu.knowitall.vulcan.inference.openie


import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.srl.ClearSrl
import edu.knowitall.openie.{OpenIE, Instance}

object OpenIEWrapper{

  val openie= new OpenIE(new ClearParser, new ClearSrl)

  def extract(sentence:String) = openie.extract(sentence)

  def main(args:Array[String]){
    println("Sentence: ")
    for( ln <- io.Source.stdin.getLines ) {
      val instances: Seq[Instance] = openie.extract(ln)
      println("Extractions:")
      instances.foreach(inst => println(inst.extraction.toString()))
      println()
      println("Sentence: ")
    }
  }
}