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
import scala.xml.{Node, Elem}
import edu.knowitall.vulcan.inference.kb._
import edu.knowitall.vulcan.inference.utils.TupleHelper._
import edu.knowitall.vulcan.inference.proposition.Proposition
import edu.knowitall.vulcan.inference.evidence.TextualAxiomsFinder
import edu.knowitall.vulcan.inference.apps.PropositionVerifier
import edu.knowitall.vulcan.inference.mln.tuffyimpl.{InferenceResults, TuffyWrapper}
import java.io.File
import scopt.mutable.OptionParser
import scala.Some
import unfiltered.response.ResponseString
import scala.collection.AbstractSeq

object HtmlHelper {

  val formName = "scoreprop"
  def form(query: String) = {
    <div align="left">
    <form action={formName}>
      <table>
      <tr>
      <td><textarea name="query" cols="80">{query}</textarea></td>
      <td><input name="login" type="submit" value="Find Evidence"/></td>
      </tr>
      </table>
    </form>
    </div>
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

  def response(query:String, resultsDiv:Elem):String = {
    val queryDiv: Elem = form(query)
    val html = <html>{queryDiv} {resultsDiv}</html>
    html.toString
  }

  var verifier:PropositionVerifier = null

  def setup(endpoint:String, tuffyConfFile:String, rulesFile:String, tempDir:String, host:String, port:Int, numTuples:Int) = {
    val taf = new TextualAxiomsFinder(endpoint, numTuples)
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

  def toResultsXml(resultsOption: Option[InferenceResults]) = resultsOption match {
    case Some(results:InferenceResults) => {
      def row(string:String, double:Double) = <tr><td>{string}</td><td>{"%.2f".format(double)}</td></tr>
      val header = <tr><th>Evidence</th><th>Probability</th></tr>
      val rows = results.marginals.toSeq.sortBy(-_._2)
            .map(pair => row(pair._1, pair._2))
      <b>Results:</b><br/><br/><table>{header}{rows}</table>;
    }
    case None => {
      logger.error("No results from inference.")
      <b>"No results from inference."</b>
    }
  }

  val intentVal = unfiltered.netty.cycle.Planify {
    case req @ GET(Path(scorePropPath)) =>{
      if(hasKey(req, "query")){
        val query = getQuery(req)
        val tupleRegex(arg1, rel, arg2) = query
        val pred = new Predicate(from(arg1, rel, arg2, addLemmas=true), 1.0)
        logger.info("Proposition predicate: " + pred.tuple.toString)
        val proposition = new Proposition(Seq[Predicate](), pred)

        val evidence = verifier.findEvidence(proposition)
        verifier.exportEvidence(evidence)
        val inferenceResults = verifier.runTuffy()
        val xml = <div>{toResultsXml(inferenceResults)}</div>
        ResponseString(wrapHTML(response(query, xml)))
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
    var numTuples = 10
    var lemma = false
    val parser = new OptionParser() {
      arg("endpoint", "TE client endpoint url. (e.g. http://rv-n16.cs.washington.edu:9191/api/query)", {str => endpoint = str})
      arg("tuffyConfFile", "Tuffy conf file.", {str => tuffyConfFile  = str})
      arg("rulesFile", "Logic rules file.", {str => rulesFile  = str})
      arg("tempDir", "Temp directory for tuffy.", {str => tempDir = str})
      arg("host", "CNC Host.", {str => host = str})
      arg("cncport", "CNC Port", {str => cncport = str.toInt})
      opt("p", "port", "Port to run on.", {str => port = str.toInt})
      opt("n", "numTuples", "Number of top textual evidence tuples to use.", {str => numTuples = str.toInt})
      //opt("l", "lemma", "Use lemmas instead of text", {str => lemma = str.toBoolean})
    }

    if(parser.parse(args)){
      setup(endpoint, tuffyConfFile, rulesFile, tempDir, host, cncport, numTuples)
      unfiltered.netty.Http(port).plan(intentVal).run()
    }else{
      println(parser.usage)
    }
  }

}

