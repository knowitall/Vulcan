package edu.knowitall.vulcan.inference.mln.tuffyimpl

/**
 * Methods imported from Tuffy java library for ease of use.
 *
 */

import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import tuffy.mln.{Predicate, MarkovLogicNetwork, Clause}
import java.io.StringWriter
import tuffy.util._

import java.sql.ResultSet
import tuffy.infer.DataMover
import tuffy.util.Config.MCSAT_OUTPUT_TUPLE_ORDER
import java.{lang, util}
import scala.collection.mutable.ArrayBuffer


object TuffyUtils {

  def toTuffyLiteral(string: String) = string.replaceAll("""[^a-zA-Z0-9 "]""", "")

  def quotedConstant(string:String) = """"%s"""".format(string)

  val logger = LoggerFactory.getLogger(this.getClass)

  def toString(instance:Clause#ClauseInstance):String = {
    "%.6f\t%s\t%s".format(instance.weight, instance.isFixedWeight, instance.conList.map(term => term.constantString()))
  }

  def atomToString(p:Predicate, rs:ResultSet , cmap:util.HashMap[lang.Long, String]) = {
    var line = p.getName() + "("
    var cs = new util.ArrayList[String]()
    try{
      (0 until p.arity()).foreach(i => {
        val  a = p.getArgs().get(i)
        val t = p.getTypeAt(i)
        val v:String = if(!t.isNonSymbolicType()){
         cmap.get(rs.getLong(a))
        }else{
         rs.getString(a)
        }
        if(v.matches("^[0-9].*$") && !StringMan.escapeJavaString(v).contains(" ")){
          cs.add("" + StringMan.escapeJavaString(v) + "");
        }else{
          cs.add("\"" + StringMan.escapeJavaString(v) + "\"");
        }
      })
    }catch{
      case e:Exception => ExceptionMan.handle(e)
    }
    line += StringMan.commaList(cs) + ")"
    line
  }

  def getQueryProbabilities(dmover:DataMover, mln:MarkovLogicNetwork) = {
    val db = dmover.db
    val relAtoms = mln.relAtoms
    val cmap: util.HashMap[lang.Long, String] = db.loadIdSymbolMapFromTable
    import scala.collection.JavaConversions._
    val preds = mln.getAllPredOrderByName.filter(!_.isImmutable)
    preds.flatMap(p => {
      val atomTypeCond: String = " "//(if (Config.mcsat_output_hidden_atoms) " " else " AND (club=1 OR club=3) ")
      var orderBy: String = " ORDER BY "
      Config.mcsat_output_order match {
        case MCSAT_OUTPUT_TUPLE_ORDER.PRED_ARGS => orderBy += StringMan.commaList(p.getArgs)
        case MCSAT_OUTPUT_TUPLE_ORDER.PROBABILITY => orderBy += " ra.prob DESC "
      }
      val sql = "SELECT pt.*, ra.*, ra.prob as raprob FROM " + p.getRelName + " pt, " + relAtoms + " ra " + " WHERE pt.id = ra.tupleID AND ra.predID = " + p.getID + " AND ra.prob >= " + Config.marginal_output_min_prob + " " + atomTypeCond + orderBy
      val rs: ResultSet = db.query(sql)
      var out = Seq[(String, Double)]()
      while (rs.next) {
        val prob: Double = rs.getDouble("raprob")
        var prior: Double = rs.getDouble("prior")
        if (rs.wasNull) {
          prior = -1
        }
        val satom: String = atomToString(p, rs, cmap)
        out :+= (satom, prob)
      }
      out
    })
  }

  /**
   * Dump marginal inference results to a file
   * @param relAtoms
   */

  def dumpProbsToFile(dmover:DataMover, mln:MarkovLogicNetwork, relAtoms: String) = {
    val db = dmover.db

    val bufferedWriter: StringWriter = new StringWriter()
    val cmap: util.HashMap[lang.Long, String] = db.loadIdSymbolMapFromTable
    val digits: Int = 4
    var sql: String = null

      import scala.collection.JavaConversions._
      val preds = mln.getAllPredOrderByName.filter(!_.isImmutable)
      for (p <- preds){

        val atomTypeCond: String = (if (Config.mcsat_output_hidden_atoms) " " else " AND (club=1 OR club=3) ")
        var orderBy: String = " ORDER BY "
        Config.mcsat_output_order match {
          case MCSAT_OUTPUT_TUPLE_ORDER.PRED_ARGS => orderBy += StringMan.commaList(p.getArgs)
          case MCSAT_OUTPUT_TUPLE_ORDER.PROBABILITY => orderBy += " ra.prob DESC "
        }
        sql = "SELECT pt.*, ra.*, ra.prob as raprob FROM " + p.getRelName + " pt, " + relAtoms + " ra " + " WHERE pt.id = ra.tupleID AND ra.predID = " + p.getID + " AND ra.prob >= " + Config.marginal_output_min_prob + " " + atomTypeCond + orderBy
        //println("Sql: " + sql)
        val rs: ResultSet = db.query(sql)
        while (rs.next) {
          val prob: Double = rs.getDouble("raprob")
          var prior: Double = rs.getDouble("prior")
          if (rs.wasNull) {
            prior = -1
          }
          val satom: String = atomToString(p, rs, cmap)
          var line: String = null
          if (Config.output_prolog_format) {
            line = "tuffyPrediction(" + UIMan.decimalRound(digits, prob) + ", " + satom + ")."
          }
          else {
            line = UIMan.decimalRound(digits, prob) + "\t" + satom
          }
          if (Config.output_prior_with_marginals && prior >= 0) {
            line += " // prior = " + UIMan.decimalRound(digits, prior)
            line += " ; delta = " + UIMan.decimalRound(digits, prob - prior)
          }
          bufferedWriter.append(line + "\n")
        }
        rs.close
      }
      bufferedWriter.close

    bufferedWriter.getBuffer.toString
  }

}
