package edu.knowitall.vulcan.common

/**
 * Serializes and deserialized Tuples as Json
 */
object TupleSerialization {

  import play.api.libs.json._

  // play's json library defines a bunch of macros for auto-serialization
  implicit val termReader = Json.reads[Term]
  implicit val termWriter = Json.writes[Term]

  implicit val relationReader = Json.reads[Relation]
  implicit val relationWriter = Json.writes[Relation]

  implicit val termsReader = Json.reads[TermsArg]
  implicit val termsWriter = Json.writes[TermsArg]

  /* 
   * Define custom json reader/writer logic for Args, which dispatches 
   * to the appropriate readers/writers based on type.
   */
  implicit val argReader = new Reads[Arg] {
    def reads(json: JsValue) : JsResult[Arg] = {
      // arg must be a JsObject with one member: a Tuple or a TermsArg
      json match {
        case JsObject(Seq(("termsArg", terms))) => termsReader.reads(terms)
        case JsObject(Seq(("tuple", tuple))) => tupleReader.reads(tuple)
        case _ => sys.error("invalid Arg json: " + Json.stringify(json))
      }
    }  
  }

  implicit val argWriter = new Writes[Arg] {
    def writes(arg: Arg) : JsValue = {
      arg match {
        case terms: TermsArg => JsObject(Seq("termsArg" -> termsWriter.writes(terms)))
        case tuple: Tuple  => JsObject(Seq("tuple" -> tupleWriter.writes(tuple)))
        case unknown: Any => sys.error("unknown Arg type: " + unknown)
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

  /**
   * Simple example for testing, demonstration.
   */
  def example = {

    val tuple = Tuple(
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
                        Term("foxes", Some("fox")))),
                      Relation(Seq(
                        Term("find"))),
                      Seq(TermsArg(Seq(Term("food"))),
                          TermsArg(Seq(Term("in"), Term("the"), Term("woods")))))))

    val asJson = TupleSerialization.toJson(tuple, true)
    println("serialized Tuple as:\n" + asJson)
    val asTuple = TupleSerialization.fromJson(asJson)
    val asJsonFromTupleFromJson = TupleSerialization.toJson(asTuple, true)
    println("reserialized deserialiazed Tuple as\n" + asJsonFromTupleFromJson)

  }
}
