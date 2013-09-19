package edu.knowitall.vulcan.inference.apps.web

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/5/13
 * Time: 3:12 PM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import unfiltered.request.{GET, Path, Method, HttpRequest}
import org.apache.commons.lang.StringEscapeUtils
import edu.knowitall.openie.{Instance, OpenIE}
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.srl.ClearSrl
import scala.xml.Elem
import unfiltered.response.ResponseString
import scopt.mutable.OptionParser

object HtmlHelper {


  def queryFormXML(query: String) = {
    <form action="openie">
      Query:<textarea name="query" cols="80">{query}</textarea>
      <input name="login" type="submit" value="submit"/>
    </form>
  }

}

object ReqHelper {


  def getQuery(req:HttpRequest[Any]) = req.parameterValues("query")(0)

  def has(req: HttpRequest[Any], key: String) = req.parameterNames.contains(key)

  def getValue(req:HttpRequest[Any], key:String) = has(req, key) match{
    case true => Some(req.parameterValues(key)(0))
    case false => None
  }


}

object InferenceFilter {

  def wrapHtml(content:String) = "<html>" + content + "</html>"


  def escapeHTMLChars(string:String) = StringEscapeUtils.escapeHtml(string)


  var openie:OpenIE = null


  val tuffyArgs = Seq("-i ")
  def setupTuffy() = {}


  def setupOpenIe() = {
    if(openie == null) openie = new OpenIE(new ClearParser, new ClearSrl)
  }

  def response(query:String, instances:Seq[Instance]):String = {

    def toRow(x:Instance) =  <td>{x.extraction.toString()}</td>
    val rows = instances.map(toRow(_))
    val resultsDiv: Elem = <div>{rows}</div>
    val queryDiv: Elem = HtmlHelper.queryFormXML(query)
    val html = <html>{queryDiv} {resultsDiv}</html>
    html.toString
  }

  def wrapHTML(string:String)  = "<html>" + string + "</html>"
  def wrapXML(string:String)  = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n" + string
  val intentVal = unfiltered.netty.cycle.Planify {

    case req @ GET(Path("/openie")) =>{
      if(ReqHelper.has(req, "query")){
        val query = ReqHelper.getQuery(req)
        val result = openie.extract(query)
        ResponseString(wrapHTML(response(query, result)))
      }else{
        ResponseString(wrapHTML(HtmlHelper.queryFormXML("").toString()))
      }
    }
  }

  def main(args:Array[String]){

    var port = 8088
    val parser = new OptionParser() {
      arg("port", "Port to run on.", {str => port = str.toInt})
    }

    if(parser.parse(args)){
      setupOpenIe()
      //val port = if (args.size > 0) try {args(0).toInt} catch {case e:NumberFormatException => 8088; case _ => 8088} else 8088
      unfiltered.netty.Http(port).plan(intentVal).run()
    }else{
      println(parser.usage)
    }
  }

}

