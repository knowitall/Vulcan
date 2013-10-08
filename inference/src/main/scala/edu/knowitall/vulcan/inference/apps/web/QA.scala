package edu.knowitall.vulcan.inference.apps.web

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 10/8/13
 * Time: 2:05 PM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import unfiltered.request.{GET, Path}
import scopt.mutable.OptionParser
import edu.knowitall.vulcan.inference.apps.web.HtmlHelper._
import scala.Some
import unfiltered.response.ResponseString
import com.vulcan.halo.util.XmlUtil
import com.vulcan.halo.core.{Answer, QuestionParse}
import edu.knowitall.vulcan.inference.solvers.TupleMatchTrueFalseSolver

class QA() {

  def marshall(xml:String): Option[QuestionParse] = {
    try{
      Some(XmlUtil.fromXmlString(xml, classOf[QuestionParse]))
    } catch {
      case e:Exception => {
        e.printStackTrace()
        None
      }
    }
  }

  def toXml(parse:QuestionParse) = {
    XmlUtil.toXmlString(parse, "question", classOf[QuestionParse])
  }
  def toXml(answer:Answer) = {
    XmlUtil.toXmlString(answer, "qa-pair", classOf[Answer])
  }
}
object QA {

  val logger = LoggerFactory.getLogger(this.getClass)

  val formName = "tfsolver"
  def form(parse: String) = {
    <div align="left">
      <form action={formName}>
        <table>
          <tr>
            <td><textarea name="parse" cols="80">{parse}</textarea></td>
            <td><input name="login" type="submit" value="Process"/></td>
          </tr>
        </table>
      </form>
    </div>
  }

  val qa = new QA()
  val solver = new TupleMatchTrueFalseSolver

  def wrapXML(string:String)  = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n" + string
  val intentVal = unfiltered.netty.cycle.Planify {
    case req @ GET(Path("/tfsolver")) =>{
      import ReqHelper._
      getValue(req, "parse") match {
        case Some(xml:String) => {
          qa.marshall(xml) match {
            case Some(parse:QuestionParse) => {
              val answer = solver.solve(parse)
              //ResponseString(qa.toXml(parse))
              ResponseString(qa.toXml(answer))
            }
            case None => ResponseString("Failed to parse input xml: " + xml)
          }
        }
        case None => ResponseString(wrapHtml(form("").toString()))
      }
    }
    case _ => ResponseString("Unknown request.")
  }


  def main(args:Array[String]){

    var port = 8088
    var numTuples = 1
    //var analsURL = ""
    val parser = new OptionParser() {
      //arg("analsURL", "QA analyzer service url", {str => analsURL = str})
      opt("p", "port", "Port to run on.", {str => port = str.toInt})
      opt("n", "numTuples", "Number of top textual evidence tuples to use.", {str => numTuples = str.toInt})
      //opt("l", "lemma", "Use lemmas instead of text", {str => lemma = str.toBoolean})
    }

    if(parser.parse(args)){
      unfiltered.netty.Http(port).plan(intentVal).run()
    }else{
      println(parser.usage)
    }
  }

}


