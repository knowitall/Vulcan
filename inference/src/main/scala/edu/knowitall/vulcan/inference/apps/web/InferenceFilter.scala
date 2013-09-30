package edu.knowitall.vulcan.inference.apps.web

/**
 * Created with IntelliJ IDEA.
 * User: niranjan
 * Date: 9/5/13
 * Time: 3:12 PM
 * To change this template use File | Settings | File Templates.
 */

import org.slf4j.LoggerFactory
import unfiltered.request.{GET, Path, HttpRequest}
import org.apache.commons.lang.StringEscapeUtils
import edu.knowitall.openie.OpenIE
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.srl.ClearSrl
import scala.xml.Elem
import edu.knowitall.vulcan.inference.kb._
import edu.knowitall.vulcan.inference.utils.TupleHelper._
import edu.knowitall.vulcan.inference.proposition.Proposition
import edu.knowitall.vulcan.inference.evidence.TextualAxiomsFinder
import edu.knowitall.vulcan.inference.apps.PropositionVerifier
import edu.knowitall.vulcan.inference.mln.tuffyimpl.TuffyWrapper
import java.io.File
import scopt.mutable.OptionParser
import scala.Some
import unfiltered.response.ResponseString

object HtmlHelper {

  val formName = "scoreprop"
  def form(query: String) = {
    <form action={formName}>
      Query:<textarea name="query" cols="80">{query}</textarea>
      <input name="login" type="submit" value="submit"/>
    </form>
  }

}

object ReqHelper {


  def getQuery(req:HttpRequest[Any]) = req.parameterValues("query")(0)

  def hasKey(req: HttpRequest[Any], key: String) = req.parameterNames.contains(key)

  def getValue(req:HttpRequest[Any], key:String) = hasKey(req, key) match{
    case true => Some(req.parameterValues(key)(0))
    case false => None
  }


}

object InferenceFilter {

  val logger = LoggerFactory.getLogger(this.getClass)

  import HtmlHelper._
  import ReqHelper._

  def wrapHtml(content:String) = "<html>" + content + "</html>"


  def escapeHTMLChars(string:String) = StringEscapeUtils.escapeHtml(string)


  var openie:OpenIE = null


  val tuffyArgs = Seq("-i ")
  def setupTuffy() = {}


  def setupOpenIe() = {
    if(openie == null) openie = new OpenIE(new ClearParser, new ClearSrl)
  }

  def response(query:String, result:String):String = {
    val resultsDiv: Elem = <div>{result}</div>
    val queryDiv: Elem = form(query)
    val html = <html>{queryDiv} {resultsDiv}</html>
    html.toString
  }

  var verifier:PropositionVerifier = null

  def setup(endpoint:String, tuffyConfFile:String, rulesFile:String, tempDir:String, host:String, port:Int) = {
    val taf = new TextualAxiomsFinder(endpoint)
    val kbf = new KBAxiomFinder(new WordnetKB::new CNCategorizerKB(host, port)::Nil)
    val finders = Seq(taf, kbf)
    val tuffy = new TuffyWrapper(tuffyConfFile)
    val file = new File(rulesFile)
    val rules = LogicRules.fromFile(file)
    logger.info("# of rules loaded = %d".format(rules.size) )
    verifier = new PropositionVerifier(finders, tuffy, rules, tempDir, host, port)

  }

  val tupleRegex = """\((.*?), (.*?), (.*)\)""".r

  def wrapHTML(string:String)  = "<html>" + string + "</html>"
  def wrapXML(string:String)  = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n" + string
  val scorePropPath = "/" + HtmlHelper.formName

  val intentVal = unfiltered.netty.cycle.Planify {
    case req @ GET(Path(scorePropPath)) =>{
      if(hasKey(req, "query")){
        val query = getQuery(req)
        val tupleRegex(arg1, rel, arg2) = query
        val pred = new Predicate(from(arg1, rel, arg2), 1.0)
        val result = verifier.verify(new Proposition(Seq[Predicate](), pred))
        ResponseString(wrapHTML(response(query, result)))
      }else{
        ResponseString(wrapHTML(form("").toString()))
      }
    }
    case _ => ResponseString("Unknown request.")
  }

  def main(args:Array[String]){

    var port = 8088
    var endpoint = ""
    var tuffyConfFile = ""
    var rulesFile = ""
    var tempDir = "./"
    var host = ""
    var cncport = 0
    val parser = new OptionParser() {
      arg("endpoint", "TE client endpoint url. (e.g. http://rv-n16.cs.washington.edu:9191/api/query)", {str => endpoint = str})
      arg("tuffyConfFile", "Tuffy conf file.", {str => tuffyConfFile  = str})
      arg("rulesFile", "Logic rules file.", {str => rulesFile  = str})
      arg("tempDir", "Temp directory for tuffy.", {str => tempDir = str})
      arg("host", "CNC Host.", {str => host = str})
      arg("cncport", "CNC Port", {str => cncport = str.toInt})
      opt("p", "port", "Port to run on.", {str => port = str.toInt})
    }

    if(parser.parse(args)){
      setup(endpoint, tuffyConfFile, rulesFile, tempDir, host, cncport)
      unfiltered.netty.Http(port).plan(intentVal).run()
    }else{
      println(parser.usage)
    }
  }

}

