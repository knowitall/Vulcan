package edu.knowitall.vulcan.common.serialization

import edu.knowitall.vulcan.common.Term
import edu.knowitall.vulcan.common.Extraction

/**
 * Serializes and deserializes Extractions as Json
 */
object ExtractionSerialization {

  import play.api.libs.json._

  import TupleSerialization.termReader
  import TupleSerialization.termWriter 

  import TupleSerialization.tupleReader
  import TupleSerialization.tupleWriter

  implicit val extractionReader : Reads[Extraction] = Json.reads[Extraction]
  implicit val extractionWriter : Writes[Extraction] = Json.writes[Extraction]

  def toJson(extraction: Extraction, pretty: Boolean = false) : String = {
    val json = Json.toJson(extraction)
    if(pretty) Json.prettyPrint(json)
    else Json.stringify(json)
  }

  def fromJson(json: String) : Extraction = {
    val extractionJson = Json.parse(json) 
    Json.fromJson[Extraction](extractionJson) match {
      case JsSuccess(extraction, _) => extraction
      case JsError(errors) => 
        sys.error("Failed to parse extraction: " + JsError.toFlatJson(errors))
    }
  }
}

object ExtractionSerializationExample {

  val exampleExtraction = Extraction(TupleSerializationExample.exampleTuple,
                                     "the quick brown fox jumps over the lazy dog",
                                     Seq(Term("sentence"), Term("details")),
                                     .75,
                                     "testId",
                                     "testCorpus")

  def example = {

    val asJson = ExtractionSerialization.toJson(exampleExtraction, true)
    println("serialized Extraction as:\n" + asJson)
    val asExtraction = ExtractionSerialization.fromJson(asJson)
    val asJsonFromExtractionFromJson = ExtractionSerialization.toJson(asExtraction, true)
    println("reserialized deserialiazed Extraction as\n" + asJsonFromExtractionFromJson)
  }
}

