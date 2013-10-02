package edu.knowitall.vulcan.evidence.query

/**
 * Abstract query type representations
 */
trait Query
case class FieldQuery(field: String, term: String) extends Query
case class AndQuery(terms: Seq[Query]) extends Query
case class OrQuery(terms: Seq[Query]) extends Query
case class NotQuery(term: Query) extends Query

// EmptyQuery is a convenience for easier query generation that shouldn't
// be used directly.  It's only used when building complex queries with
// QueryBuilder when some terms or clauses may be empty.
case object EmptyQuery extends Query

/**
 * Query companion object defines json serialization format.
 */
object Query {

  import play.api.libs.json._
  import play.api.data.validation.ValidationError

  implicit val fieldQueryReader = Json.reads[FieldQuery]
  implicit val fieldQueryWriter = Json.writes[FieldQuery]

  implicit val writer = new Writes[Query] {
    def writes(query: Query) : JsValue = {
      query match {
        case fq: FieldQuery => JsObject(Seq("fq" -> fieldQueryWriter.writes(fq)))
        case and: AndQuery => JsObject(Seq("and" -> 
                                JsArray(and.terms map { term => writes(term) })))
        case or: OrQuery => JsObject(Seq("or" -> 
                                JsArray(or.terms map { term => writes(term) })))
        case not: NotQuery => JsObject(Seq("not" -> writes(not.term)))
        case EmptyQuery => JsString("")
        case unknown: Any => sys.error("unknown query type for: " + unknown)
      }
    }
  }

  implicit val reader : Reads[Query] = new Reads[Query] {

    def reads(json: JsValue) : JsResult[Query] = {

      json match {

        case JsObject(Seq(("fq", fq))) => fieldQueryReader.reads(fq)

        case JsObject(Seq(("and", value))) => Json.fromJson[Seq[Query]](value) match {
          case JsSuccess(queries, _) => JsSuccess(AndQuery(queries))
          case error : JsError => error
        }

        case JsObject(Seq(("or", value))) => Json.fromJson[Seq[Query]](value) match {
          case JsSuccess(queries, _) => JsSuccess(OrQuery(queries))
          case error : JsError => error
        }

        case JsObject(Seq(("not", value))) => reads(value) match {
          case JsSuccess(query, _) => JsSuccess(NotQuery(query))
          case error : JsError => error
        }

        case other => JsError(ValidationError("Invalid Query json: " + Json.stringify(other)))
      }
    }
  }
}
