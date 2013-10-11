package edu.knowitall.vulcan.extraction

import edu.knowitall.srlie.SrlExtraction
import edu.knowitall.srlie.SrlExtraction.{Relation => SrlRelation}
import edu.knowitall.srlie.SrlExtraction.{Argument => SrlArgument}
import edu.knowitall.srlie.nested.SrlNestedExtraction
import edu.knowitall.srlie.nested.SrlNestedExtraction.SrlNestedArgument
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.postag.OpenNlpPostagger
import edu.knowitall.tool.parse.{RemoteDependencyParser, ClearParser, DependencyParser}
import edu.knowitall.tool.postag.ClearPostagger
import edu.knowitall.chunkedextractor.Relnoun
import edu.knowitall.srlie.SrlExtractor
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.chunkedextractor.BinaryExtractionInstance
import edu.knowitall.srlie.SrlExtractionInstance
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.parse.graph.DependencyNode
import edu.knowitall.tool.srl.{RemoteSrl, Roles, Srl, ClearSrl}
import edu.knowitall.collection.immutable.Interval
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

import scala.collection.JavaConverters._
import scala.collection.mutable.MutableList
import scala.io.Source
import scala.io.BufferedSource
import scala.util.{Try, Success, Failure}

import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter

import java.io.PrintWriter
import java.io.Writer
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.util.Date
import java.util.concurrent.atomic.AtomicInteger
import java.text.SimpleDateFormat

import org.slf4j.LoggerFactory

/**
 * Runs OpenIE-4-like extractor over input sentences, generating Extractions, and
 * dumping them as Json
 */
class Extractor(wordnetHome: String, parserServer:Option[String]=None, srlServer:Option[String]=None) {

  val logger = LoggerFactory.getLogger(this.getClass)

  private val idSeq = new AtomicInteger(0)

  //Create a new parser if a server isn't specified.
  val parser = parserServer match {
    case Some(url:String) => new RemoteDependencyParser(url)
    case None => new ClearParser()
  }
  //Create a new Srl instance if a server isn't specified.
  val srl = srlServer match {
    case Some(url:String) => new RemoteSrl(url)
    case None => new ClearSrl()
  }

  val headExtractor = new HeadExtractor(wordnetHome)

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
    headExtractor.relationHead(unwrapped) match {
      case Some(tokens) => Some(tokens map { (tok) => Term(tok.string) })
      case None => None
    }
  }

  def extractFile(file: File,
                  corpus: String, 
                  batchId: String, 
                  outputFile: File) : Unit = 
  {
    val input = Source.fromFile(file, "UTF-8")
    val output = new PrintWriter(
      new OutputStreamWriter(new FileOutputStream(outputFile, true), "UTF-8"))
     
    def nextId() : String = { 
      s"${corpus}_${batchId}_${idSeq.getAndIncrement()}"
    }

    for(sentence <- input.getLines()) {
      
      try {
        val extractions = extract(SentencePreprocessor(sentence), corpus, nextId)
        extractions.foreach( extraction => 
          output.println(ExtractionSerialization.toJson(extraction))
        )
      } catch {
        case t: Throwable => logger.error("Extraction failed for " + sentence, t);
      }
    }

    input.close()
    output.close()
  }

  /**
   * Given a sentence, calls subextractors and returns their extractions
   * converted to common Extraction Tuples.
   */
  def extract(sentence: String, corpus: String, nextId: () => String) : Seq[Extraction] = {

    // wrapper for a Seq[Interval] --- holdover from OpenIE
    case class Part(text: String, offsets: Seq[Interval])

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
      
    def convertSrlNestedArg(arg: SrlNestedArgument) : Arg = {
      arg match {
        case Left(arg1: SrlArgument) => argForSrlArgument(arg1)
        case Right(nested: SrlNestedExtraction) => convertSrlNested(nested)
      }
    }

    def convertSrlNested(extr: SrlNestedExtraction) : Tuple = {

      Tuple(convertSrlNestedArg(extr.arg1),
            relForSrlRelation(extr.rel, extr.extr.negated, extr.extr.passive),
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

    val confidence = srlExtrs match {
      case Nil => None
      case inst :: _ => Some(srlieConf(inst)) // assumes all confidences are the same
    }
    val srlNested = SrlNestedExtraction.from(srlExtrs.map { _.extr })

    val srls = srlNested.map(srl => 
                 Extraction(convertSrlNested(srl),
                            sentence, 
                            sentenceDetails,
                            confidence.get,
                            nextId(),
                            corpus))
                            


    val extrs = srls ++ (relnounExtrs map convertRelnoun)  

    extrs
  }
}

/** 
 * We want to do some preprocessing of sentences.
 *
 * Right now this just strips out parenthetical content.
 */
object SentencePreprocessor {
  def apply(sentence: String) : String = {
    sentence.replaceAll("\\([^)]*\\)", "")
  }
}

/**
 * Reads sentences, one per line, from stdin, runs openie on them, and loads
 * the resulting extractions into the solr instance running on a configured url.
 */
object ExtractorMain {

  val logger = LoggerFactory.getLogger("ExtractorMain")

  case class Config(val inputFile: Option[File] = None,
                    val outputFile: Option[File] = None,
                    val corpus: Option[String] = None,
                    val batchId: Option[String] = None,
                    val wnHome: Option[String] = None,
                    val parserUrl:Option[String] = None,
                    val srlUrl:Option[String] = None)
  {
    def getInputFiles() : Iterable[File] = inputFile match {
      case Some(file) => {
        val files = FileUtils.listFiles(file,
                                        TrueFileFilter.INSTANCE, // all files
                                        TrueFileFilter.INSTANCE) // in all subdirs
        files.asScala
      }
       case None => sys.error("unreachable")
    }
  }

  def parseArgs(args: Array[String]) : Option[Config] = {

    val parser = new scopt.immutable.OptionParser[Config]("ExtractorMain") {

      def options = Seq(
        opt("input-file", "[required] file or directory to read input sentences from, one per line") { 
          (param, c) => c.copy(inputFile = Some(new File(param)))
        }, 

        opt("corpus", "corpus to tag extractions as coming from, defaults to output filename") { 
          (param, c) => c.copy(corpus = Some(param)) 
        }, 

        opt("batch-id", "generate extraction ids with the given batch id") { 
          (param, c) => c.copy(batchId = Some(param)) 
        }, 

        opt("output-file", "[required] file or directory to write output extractions to") { 
          (param, c) => c.copy(outputFile = Some(new File(param)))
        },

        opt("wordnet-home", "[required] location of wordnet on the local filesystem") { 
          (param, c) => c.copy(wnHome = Some(param))
        },

        opt("parser-url", "Clear dependency parser server url.") {
          (param, c) => c.copy(parserUrl = Some(param))
        },

        opt("srl-url", "Clear srl parser server url.") {
          (param, c) => c.copy(srlUrl = Some(param))
        }
      )
    }

    parser.parse(args, Config()) match {
      case Some(config) => {
        if(config.wnHome.isEmpty || 
           config.inputFile.isEmpty || 
           config.outputFile.isEmpty)
        { 
          parser.showUsage
          None
        } else {
          Some(config)
        }
      }
      case None => None
    }
  }

  def main(args:Array[String]) {

    val config = parseArgs(args) match {
      case Some(c) => c
      case None => return
    }

    val batchId = config.batchId.getOrElse(
      new SimpleDateFormat("yyyy-MM-dd-HH-mm").format(new Date()))

    val extractor = new Extractor(config.wnHome.get, config.parserUrl, config.srlUrl)

    for(inputFile <- config.getInputFiles()) {

      val corpus = config.corpus.getOrElse(
        inputFile.getName().replaceAll("\\.*\\.sentences$", ""))

      val outputFile = 
        if(config.outputFile.get.isDirectory()) {
          new File(config.outputFile.get, inputFile.getName() + ".extractions")
        } else {
          config.outputFile.get
        }
      outputFile.getParentFile().mkdirs()
          
      extractor.extractFile(inputFile, 
                            corpus,
                            batchId, 
                            outputFile);
    }
  }
}
