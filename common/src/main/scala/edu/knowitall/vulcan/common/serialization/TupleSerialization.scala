package edu.knowitall.vulcan.common.serialization

import edu.knowitall.vulcan.common.Tuple
import edu.knowitall.vulcan.common.Arg
import edu.knowitall.vulcan.common.Term
import edu.knowitall.vulcan.common.Relation
import edu.knowitall.vulcan.common.TermsArg

/**
 * Serializes and deserialized Tuples as Json
 */
object TupleSerialization {

  import play.api.libs.json._
  import play.api.data.validation.ValidationError

  /**
   * Define a more compact, fixed format for Term serialization which assumes either 
   * all Option fields are present or none are for each term, and infers them from
   * array positions.
   */
  implicit val termReader = new Reads[Term] {
    def reads(json: JsValue) : JsResult[Term] = {

      json match {

        case JsArray(Seq(JsString(text))) => JsSuccess(Term(text))

        case JsArray(Seq(JsString(text), 
                         JsString(lemma), 
                         JsString(postag),
                         JsString(chunk))) => 
          JsSuccess(Term(text, Some(lemma), Some(postag), Some(chunk)))

        case other => JsError(ValidationError("Invalid Term json: " + Json.stringify(other)))
      } 
    }
  }

  implicit val termWriter = new Writes[Term] {
    def writes(term: Term) : JsValue = {
      if(term.hasDetails()) {
        val d3 = term.details.get
        val termJsStrings = Seq(term.text, d3._1, d3._2, d3._3).map(JsString)
        JsArray(termJsStrings)
      } else {
        JsArray(Seq(JsString(term.text)))
      }
    }
  }

  implicit val relationReader = Json.reads[Relation]
  implicit val relationWriter = Json.writes[Relation]

  implicit val termsReader = Json.reads[TermsArg]
  implicit val termsWriter = Json.writes[TermsArg]

  /* 
   * Define custom json reader/writer logic for Args, which invokes 
   * the appropriate readers/writers based on type.
   */
  implicit val argReader = new Reads[Arg] {
    def reads(json: JsValue) : JsResult[Arg] = {
      // arg must be a JsObject with one member: a Tuple or a TermsArg
      json match {
        case JsObject(Seq(("termsArg", terms))) => termsReader.reads(terms)
        case JsObject(Seq(("tuple", tuple))) => tupleReader.reads(tuple)
        case other => JsError(ValidationError("Invalid Arg json: " + Json.stringify(other)))
      }
    }  
  }

  implicit val argWriter = new Writes[Arg] {
    def writes(arg: Arg) : JsValue = {
      arg match {
        case terms: TermsArg => JsObject(Seq("termsArg" -> termsWriter.writes(terms)))
        case tuple: Tuple  => JsObject(Seq("tuple" -> tupleWriter.writes(tuple)))
      }
    }
  }

  implicit val tupleReader : Reads[Tuple] = Json.reads[Tuple]
  implicit val tupleWriter : Writes[Tuple] = Json.writes[Tuple]

  /**
   * Converts the given tuple to a json String representation, optionally 
   * nicely formatted for human consumption.
   */
  def toJson(tuple: Tuple, pretty: Boolean = false) : String = {
    val json = Json.toJson(tuple)
    if(pretty) Json.prettyPrint(json)
    else Json.stringify(json)
  }

  /**
   * Parses the given json String into a Tuple.
   * Throws an Exception if the json isn't a valid Tuple.
   */
  def fromJson(json: String) : Tuple = {
    val tupleJson = Json.parse(json) \ "tuple"
    Json.fromJson[Tuple](tupleJson) match {
      case JsSuccess(tuple, _) => tuple
      case JsError(errors) => 
        sys.error("Failed to parse tuple: " + JsError.toFlatJson(errors))
    }
  }
}

object TupleSerializationExample {

  /**
   * Simple example for testing, demonstration.
   */
  val exampleTuple = 
                Tuple(
                  TermsArg(Seq(
                    Term("a"),
                    Term("good"),
                    Term("sense"),
                    Term("of"),
                    Term("smell"))),
                  Relation(Seq(
                    Term("helps", Some("help")))),
                  Seq(Tuple(
                      TermsArg(Seq(
                        Term("Mister"),
                        Term("Paws"),
                        Term("king"),
                        Term("of"),
                        Term("the"),
                        Term("foxes", Some("fox"))),
                        Some(Seq(Term("Mister"), Term("Paws")))),
                      Relation(Seq(
                        Term("find"))),
                      Seq(TermsArg(Seq(Term("food"))),
                          TermsArg(Seq(Term("in"), Term("the"), Term("woods")))))))

  def example = {

    val asJson = TupleSerialization.toJson(exampleTuple, true)
    println("serialized Tuple as:\n" + asJson)
    val asTuple = TupleSerialization.fromJson(asJson)
    val asJsonFromTupleFromJson = TupleSerialization.toJson(asTuple, true)
    println("reserialized deserialiazed Tuple as\n" + asJsonFromTupleFromJson)

  }
}
