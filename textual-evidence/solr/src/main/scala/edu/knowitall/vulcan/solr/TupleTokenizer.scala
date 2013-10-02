package edu.knowitall.vulcan.solr

import java.io.Reader
import java.util.regex.Pattern

import org.apache.lucene.analysis.Tokenizer
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute

import edu.knowitall.vulcan.common.Term
import edu.knowitall.vulcan.common.Tuple
import edu.knowitall.vulcan.common.Arg
import edu.knowitall.vulcan.common.TermsArg
import edu.knowitall.vulcan.common.serialization.TupleSerialization
import edu.knowitall.vulcan.common.serialization.TupleSerializationExample

import edu.knowitall.vulcan.common.Path

import org.slf4j.LoggerFactory

class TupleTokenizer(reader: Reader) extends Tokenizer(reader: Reader) {

  val log = LoggerFactory.getLogger(this.getClass) 

  val buffer = Array[Char](1024)
  val sb = new scala.collection.mutable.StringBuilder()
  val token : CharTermAttribute = addAttribute(classOf[CharTermAttribute])
  var readOnNextIncrement = true
  var tokens : Seq[String] = Nil 

  override def incrementToken() : Boolean = {

    clearAttributes()

    if(readOnNextIncrement) {

      var i: Int = input.read(buffer, 0, buffer.length)
      while(i != -1) {
        val s: String = new String(buffer, 0, i)
        sb.append(s) // sb.append(buffer, 0, i) give garbage?
        i = input.read(buffer)
      }
      
      log.info("incrementing token for json string " + sb.toString())
      val tuple: Tuple = TupleSerialization.fromJson(sb.toString())

      tokens = tokenize(tuple)
      readOnNextIncrement = false
    }

    if(tokens.isEmpty) {
      false
    } else {
      log.info("returning token " + tokens.head)
      token.append(tokens.head)
      tokens = tokens.tail
      true
    }
  }

  override def reset() : Unit = {
    super.reset()
    sb.clear()
    token.setLength(0)
    readOnNextIncrement = true
  }

  def tokenize(tuple: Tuple, path: Path = Path.Empty) : Seq[String] = {

    tokenizeArg(tuple.arg1, Path.Arg1 / path) ++
    tokenizeTerms(tuple.rel.terms, Path.Rel / path) ++
    tuple.arg2s.flatMap(arg2 => tokenizeArg(arg2, Path.Arg2 / path))
  }

  private def tokenizeArg(arg: Arg, path: Path) : Seq[String] = {
    arg match {
      case t: Tuple => tokenize(t, path)
      case terms: TermsArg => tokenizeTerms(terms.terms, path)
    } 
  }
  
  private val punct = Pattern.compile("\\p{Punct}*")

  private def tokenizeTerms(terms: Seq[Term], path: Path) : Seq[String] = {
    val tokenSeq = terms filter { term =>
      ! punct.matcher(term.text).matches()
    } flatMap { term =>
      val withLemmas : Seq[String] = term.lemma filter { 
        ! _.equalsIgnoreCase(term.text)
      } match {
        case Some(lemma) => Seq(term.text, lemma)
        case None => Seq(term.text)
      }
      withLemmas ++ ( withLemmas map { term => 
        Path.forTerm(term, path).rooted.pathString
      })
   }
   tokenSeq map { _.toString }
  }
}
