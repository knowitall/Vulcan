package edu.knowitall.vulcan.extraction

import edu.knowitall.srlie.SrlExtraction
import edu.knowitall.srlie.SrlExtraction.{Relation => SrlRelation}
import edu.knowitall.srlie.SrlExtraction.{Argument => SrlArgument}
import edu.knowitall.srlie.nested.SrlNestedExtraction
import edu.knowitall.srlie.nested.SrlNestedExtraction.SrlNestedArgument
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.postag.OpenNlpPostagger
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.postag.ClearPostagger
import edu.knowitall.chunkedextractor.Relnoun
import edu.knowitall.srlie.SrlExtractor
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.chunkedextractor.BinaryExtractionInstance
import edu.knowitall.srlie.SrlExtractionInstance
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.parse.graph.DependencyNode
import edu.knowitall.tool.srl.Roles
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.srl.Srl
import edu.knowitall.tool.parse.DependencyParser
import edu.knowitall.tool.srl.ClearSrl
import edu.knowitall.srlie.SrlExtraction
import edu.knowitall.srlie.confidence.SrlConfidenceFunction
import edu.knowitall.chunkedextractor.confidence.RelnounConfidenceFunction

import edu.knowitall.vulcan.headwords.HeadExtractor
import edu.knowitall.tool.postag.PostaggedToken

import edu.knowitall.vulcan.common.Tuple
import edu.knowitall.vulcan.common.Arg
import edu.knowitall.vulcan.common.Extraction
import edu.knowitall.vulcan.common.Term
import edu.knowitall.vulcan.common.Relation
import edu.knowitall.vulcan.common.TermsArg
import edu.knowitall.vulcan.common.serialization.TupleSerialization
import edu.knowitall.vulcan.common.serialization.ExtractionSerialization

import scala.collection.mutable.MutableList
import scala.io.Source
import scala.io.BufferedSource
import scala.util.{Try, Success, Failure}

import java.io.PrintStream
import java.io.File
import java.util.Date
import java.util.concurrent.atomic.AtomicInteger
import java.text.SimpleDateFormat

import org.slf4j.LoggerFactory

/**
 * Runs OpenIE-4-like extractor over input sentences, generating Extractions, and
 * dumping them as Json
 */
class Extractor(corpus: String, batchId: String) {

  val logger = LoggerFactory.getLogger(this.getClass)

  private val idSeq = new AtomicInteger(0)

  def nextId() : String = s"${corpus}_${batchId}_${idSeq.getAndIncrement()}"

  val parser = new ClearParser()
  val srl = new ClearSrl()

  // TODO have to read wordnet home from a config
  val headExtractor = new HeadExtractor("/home/gregj/scratch/wordnet3.0/")

  // confidence functions
  val srlieConf = SrlConfidenceFunction.loadDefaultClassifier()
  val relnounConf = RelnounConfidenceFunction.loadDefaultClassifier()

  // sentence pre-processors
  val tokenizer = new ClearTokenizer()
  val postagger = new OpenNlpPostagger(tokenizer)
  val chunker = new OpenNlpChunker(postagger)

  // subextractors
  val relnoun = new Relnoun
  val srlie = new SrlExtractor(srl)

  var rootN: Int = 0
  var nestedN: Int = 0

  def toTerm(token: Lemmatized[ChunkedToken]) : Term = {
    Term(token.string, Some(token.lemma), Some(token.postag), Some(token.chunk))
  }

  def getArgHead[T <: PostaggedToken](tokens: Seq[Lemmatized[T]]) : Option[Seq[Term]] = {
    val unwrapped = tokens map { _.token }
    headExtractor.argumentHead(unwrapped) match {
      case Some(tokens) => Some(tokens map { (tok) => Term(tok.string) })
      case None => None
    }
  }

  def getRelHead[T <: PostaggedToken](tokens: Seq[Lemmatized[T]]) : Option[Seq[Term]] = {
    val unwrapped = tokens map { _.token }
    headExtractor.relHead(unwrapped) match {
      case Some(tokens) => Some(tokens map { (tok) => Term(tok.string) })
      case None => None
    }
  }

  /**
   * Given a sentence, calls subextractors and returns their extractions
   * converted to common Extraction Tuples.
   */
  def extract(sentence: String) : Seq[Extraction] = {

    // pre-process the sentence
    val chunked = chunker(sentence) map MorphaStemmer.lemmatizePostaggedToken
    val sentenceDetails = chunked.map(toTerm)
    val parsed = parser(sentence)

    val relnounExtrs = relnoun(chunked)

    def offsets(part: SrlExtraction.MultiPart) = {
      var intervals = part.intervals
      var tokens = part.tokens.sorted
      var offsets = List.empty[Interval]
      while (!intervals.isEmpty) {
        val sectionTokens = tokens.take(intervals.head.size)
        tokens = tokens.drop(intervals.head.size)
        intervals = intervals.drop(1)

        offsets ::= Interval.open(sectionTokens.head.offsets.start, sectionTokens.last.offsets.end)
      }

      offsets.reverse
    }

    def tokensForPart(part: Part) = {
      part.offsets map { interval =>
        chunked filter {
          tok => interval.contains(tok.offset)
        }
      } flatten
    }
      
    def relationForPart(part: Part, negated: Boolean, passive: Boolean) : Relation = {
      val relToks = tokensForPart(part)
      val relHead = getRelHead(relToks)
      val relTerms = relToks.map(toTerm)
      Relation(relTerms, relHead, negated, passive)
    }

    def argForPart(part: Part, withHead: Boolean = true) : TermsArg = {
      val argToks = tokensForPart(part)
      val argHead = if(withHead) {
        getArgHead(argToks)
      } else {
        None
      }
      val argTerms = argToks.map(toTerm)
      TermsArg(argTerms, argHead)
    }
    
    def argForSrlArgument(arg: SrlArgument, withHead: Boolean = true) : TermsArg = {
      val argPart = new Part(arg.text, 
                             Seq(Interval.open(arg.tokens.head.offsets.start,
                                               arg.tokens.last.offsets.end)))
      argForPart(argPart) 
    }

    def relForSrlRelation(rel: SrlRelation, negated: Boolean, passive: Boolean) : Relation = {
      val relPart = new Part(rel.text, offsets(rel))
      relationForPart(relPart, negated, passive)
    }
      
    def convertSrl(inst: SrlExtractionInstance): Extraction = {


      //
      // Build a Relation from the extraction
      //
      val rel = relForSrlRelation(inst.extr.rel, inst.extr.negated, inst.extr.passive)

      //
      // Build arg1 from the extraction
      //
      val arg1Part = new Part(inst.extr.arg1.text, 
                              Seq(Interval.open(inst.extr.arg1.tokens.head.offsets.start,
                                                inst.extr.arg1.tokens.last.offsets.end)))
      val arg1 = argForPart(arg1Part) 

      //
      // Build arg2s from the extraction
      //
      val arg2sParts = inst.extr.arg2s.map(
        arg2 => new Part(arg2.text, 
                         Seq(Interval.open(arg2.tokens.head.offsets.start,
                                           arg2.tokens.last.offsets.end))))

      val arg2s = arg2sParts.size match {
        case 0 => Seq[TermsArg]()
        case 1 => Seq(argForPart(arg2sParts.head))
        case n => argForPart(arg2sParts.head) +:
                    arg2sParts.tail.map(part => argForPart(part, false))
      }

      // 
      // build up context from the extraction
      //
      val context = inst.extr.context match {
        case None => None
        case Some(context) => {
          val contextPart = new Part(context.text,
                                     Seq(Interval.open(context.tokens.head.offsets.start,
                                                       context.tokens.last.offsets.end)))
          val contextToks = tokensForPart(contextPart)
          val contextTerms = contextToks.map(toTerm)
          Some(contextTerms)
        }
      }

      val confidence = srlieConf(inst)

      Extraction(Tuple(arg1, rel, arg2s, context),
                 sentence, 
                 sentenceDetails,
                 confidence,
                 nextId(),
                 corpus) 
    }

    def convertSrlNestedArg(arg: SrlNestedArgument) : Arg = {
      arg match {
        case Left(arg1: SrlArgument) => argForSrlArgument(arg1)
        case Right(nested: SrlNestedExtraction) => convertSrlNested(nested)
      }
    }

    def convertSrlNested(extr: SrlNestedExtraction) : Tuple = {

      val relPart = new Part(extr.rel.text, offsets(extr.rel))
      val rel = relationForPart(relPart, false, false) // TODO get negation, passive flags

      Tuple(convertSrlNestedArg(extr.arg1),
            relForSrlRelation(extr.rel, false, false), // TODO get negation, passive
            extr.arg2s.map(convertSrlNestedArg))
    }
    
    def convertRelnoun(inst: BinaryExtractionInstance[ChunkedToken]): Extraction = {

      val relPart = new Part(inst.extr.rel.text, Seq(inst.extr.rel.offsetInterval))
      val rel = relationForPart(relPart, false, false)

      val arg1Part = new Part(inst.extr.arg1.text, Seq(inst.extr.arg1.offsetInterval))
      val arg1 = argForPart(arg1Part)

      val arg2Part = new Part(inst.extr.arg2.text, Seq(inst.extr.arg2.offsetInterval))
      val arg2 = Seq(argForPart(arg2Part))

      Extraction(Tuple(arg1, rel, arg2),
                 sentence,
                 sentenceDetails,
                 relnounConf(inst),
                 nextId(),
                 corpus) 
    }

    val srlExtrs: Seq[SrlExtractionInstance] = srlie(parsed)
    rootN += srlExtrs.size

    val confidence = srlExtrs match {
      case Nil => None
      case inst :: _ => Some(srlieConf(inst)) // assumes all confidences are the same
    }
    val srlNested: Seq[SrlNestedExtraction] = SrlNestedExtraction.from(srlExtrs.map { _.extr })
    nestedN += srlNested.size

    val srls = srlNested.map(srl => 
                 Extraction(convertSrlNested(srl),
                            sentence, 
                            sentenceDetails,
                            confidence.get,
                            nextId(),
                            corpus))
                            


    val extrs = srls ++ (relnounExtrs map convertRelnoun)  

    logger.info(s"extracted $rootN base and $nestedN total extractoins so far")

    extrs
  }
}

/**
 * Reads sentences, one per line, from stdin, runs openie on them, and loads
 * the resulting extractions into the solr instance running on a configured url.
 */
object ExtractorMain {

  val logger = LoggerFactory.getLogger("ExtractorMain")

  case class Config(val inputFile: Option[String] = None,
                    val output: PrintStream = System.out,
                    val corpus: Option[String] = None,
                    val batchId: Option[String] = None)
  {
    def getInputSource() : BufferedSource = inputFile match {
      case Some(filename) => Source.fromFile(new File(filename), "ISO-8859-1")
      case None => Source.stdin
    }

    def getCorpus() : String = corpus match {
      case Some(string) => string
      case None => inputFile match {
        case Some(filename) => filename
        case None => "stdin"
      }
    }
  }

  def parseArgs(args: Array[String]) : Option[Config] = {

    val parser = new scopt.immutable.OptionParser[Config]("ExtractorMain") {

      def options = Seq(
        opt("input-file", "where to read input sentences, defaults to stdin") { 
          (param, c) => c.copy(inputFile = Some(param))
        }, 

        opt("corpus", "corpus to tag extractions as coming from, defaults to input-file") { 
          (param, c) => c.copy(corpus = Some(param)) 
        }, 

        opt("batch-id", "generate extraction ids with the given batch id") { 
          (param, c) => c.copy(batchId = Some(param)) 
        }, 

        opt("output-file", "where to write output extractions, defaults to stdout") { 
          (param, c) => c.copy(output = new PrintStream(new File(param)))
        }
      )
    }

    parser.parse(args, Config()) 
  }

  def main(args:Array[String]) {

    val config = parseArgs(args) match {
      case Some(c) => c
      case None => return
    }

    val corpus = config.getCorpus()
    val batchId = config.batchId.getOrElse(
      new SimpleDateFormat("yyyy-MM-dd-HH-mm").format(new Date()))

    val extractor = new Extractor(corpus, batchId)
    
    var sentences = 0
    var nExtractions = 0

    for(sentence <- config.getInputSource().getLines()) {
      
      sentences += 1

      val extractions = extractor.extract(sentence)

      extractions foreach ( extraction => {
        nExtractions += 1
        config.output.println(ExtractionSerialization.toJson(extraction))
      })

    }

    println("Processed " + sentences + " sentences")
    println("Wrote " + nExtractions + " extractions")
  }
}

// wrapper for a Seq[Interval] --- holdover from OpenIE
case class Part(text: String, offsets: Seq[Interval])
