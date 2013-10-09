package edu.knowitall.vulcan.definitions.extractor

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 10/9/13
 * Time: 9:41 AM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import scala.io.Source
import edu.knowitall.taggers.{LinkedType, TaggerCollection}
import java.io.{File, PrintWriter}
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.typer.Type
import scala.collection.mutable.ArrayBuffer


/**
 * Extraction rough draft includes name of pattern that matched
 * and a list of the named groups that are linked to it.
 * @author jgilme1
 *
 */
class Extraction (patternName:String, extractionParts:List[LinkedType]){

  override def toString():String = {
    val sb = new StringBuilder()
    sb.append(patternName+":")
    extractionParts.foreach(lt => {
      sb.append("\t")
      sb.append(lt.name.replace(this.patternName + ".", "") + ":")
      sb.append(lt.text)
    })
    sb.toString().trim()
  }
}

object Extractor{
  def main(args:Array[String]){

    val taggerPath = args(0)
    val inputPath = args(1)
    val outputPath = args(2)
    val extractor = new Extractor(taggerPath)
    extractor.extract(inputPath, outputPath)
  }
}
class Extractor(taggerPath:String){


  val patternString = Source.fromFile(taggerPath).getLines.mkString("\n")
  //Create tagger collection from input pattern string
  val t = TaggerCollection.fromString(patternString);
  val taggers = t.taggers
  val taggerDescriptors = taggers.map(_.name)
  //initialize chunker and stemmer to pass parameters to the tag method
  val chunker = new OpenNlpChunker
  val morpha = new MorphaStemmer

  def extract(inputPath:String, outputPath:String) {
    //get tagger names and store them in taggerDescriptors array

    //initialize output with header
    val pw = new PrintWriter(new File(outputPath))



    //Treat each line in input doc as a sentence
    Source.fromFile(inputPath).getLines.foreach(line => {

      pw.write(line.trim()+"\t")

      val typeNamedGroupTypeMap = typeMap(line)
      val extractions = new ArrayBuffer[Extraction]
      //turn map from parent types to children named group types into list of ordered extractions
      taggerDescriptors.foreach(level => {
        typeNamedGroupTypeMap.keySet.foreach(typ => {
          if(typ.name.equals(level)){
            extractions.add(new Extraction(typ.name,typeNamedGroupTypeMap(typ).toList))
          }
        })
      })

      //print extractions tab separated in line
      extractions.foreach(extr => pw.write("\t"+extr.toString()))
      pw.write("\n")
    })
    pw.close()
  }


  def typeMap(line: String) = {
    //initialize map for Type to linked children types
    //initialize list of extractions
    val typeNamedGroupTypeMap = new scala.collection.mutable.HashMap[Type, ArrayBuffer[LinkedType]]

    //iterate over all matching types of the input sentence
    linkedTypes(line).foreach(typ =>
      typ.link match {
        //if the type contains a T this is just to check if it is a namedgroup instead of a numbered group
        //this can probably be checked with an instanceof call.
        case Some(parentLink: Type) => {
          if (typ.name.split("\\.").length > 1) {
            if (typ.name.split("\\.")(1).contains("T")) {
              //update the map from ParentTypes to NamedGroups.
              if (typeNamedGroupTypeMap.containsKey(parentLink)) {
                typeNamedGroupTypeMap(parentLink) += typ
              } else {
                val namedGroupTypes = new ArrayBuffer[LinkedType]
                namedGroupTypes += typ
                typeNamedGroupTypeMap.put(parentLink, namedGroupTypes)
              }
            }
          }
        }
        case _ =>
      }
    )
    typeNamedGroupTypeMap
  }

  def linkedTypes(line: String): Seq[LinkedType] = {
    val chunkedSentence = chunker.chunk(line)

    val tokens = chunkedSentence.map(morpha.lemmatizeToken)

    //get all of the matching types from the sentence
    val types = t.tag(tokens)
    //iterate over taggers in order
    taggerDescriptors.foreach(level => {
      //relevantTypes are Type matches that begin with the PatternName
      val relevantTypes = types.flatMap(typ => {
        if (typ.name.startsWith(level)) Some(typ) else None
      })
      //sort types by order of their intervals
      relevantTypes.sortBy(_.tokenInterval)
    })
    types.flatMap(typ => typ match {
      case x:LinkedType => Some(x)
      case _ => None
    })
  }
}
