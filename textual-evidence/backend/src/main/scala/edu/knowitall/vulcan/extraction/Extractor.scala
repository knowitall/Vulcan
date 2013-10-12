package edu.knowitall.vulcan.extraction

import edu.knowitall.srlie.SrlExtractor
import edu.knowitall.srlie.SrlExtraction
import edu.knowitall.srlie.SrlExtraction.{Relation => SrlRelation}
import edu.knowitall.srlie.SrlExtraction.{Argument => SrlArgument}
import edu.knowitall.srlie.SrlExtractionInstance
import edu.knowitall.srlie.nested.SrlNestedExtraction
import edu.knowitall.srlie.nested.SrlNestedExtraction.SrlNestedArgument
import edu.knowitall.srlie.confidence.SrlConfidenceFunction

import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.postag.OpenNlpPostagger
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.parse.RemoteDependencyParser

import edu.knowitall.chunkedextractor.Relnoun

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.srl.ClearSrl
import edu.knowitall.tool.srl.RemoteSrl
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.stem.MorphaStemmer

import edu.knowitall.collection.immutable.Interval

import edu.knowitall.chunkedextractor.BinaryExtractionInstance
import edu.knowitall.chunkedextractor.confidence.RelnounConfidenceFunction

import edu.knowitall.tool.headword.UWHeadExtractor

import edu.knowitall.vulcan.common.Tuple
import edu.knowitall.vulcan.common.Arg
import edu.knowitall.vulcan.common.Extraction
import edu.knowitall.vulcan.common.Term
import edu.knowitall.vulcan.common.Relation
import edu.knowitall.vulcan.common.TermsArg
import edu.knowitall.vulcan.common.serialization.TupleSerialization
import edu.knowitall.vulcan.common.serialization.ExtractionSerialization

import scopt.immutable.OptionParser

import scala.collection.JavaConverters._
import scala.collection.mutable.MutableList
import scala.io.Source
import scala.io.BufferedSource
import scala.util.{Try, Success, Failure}

import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import org.apache.commons.io.filefilter.SuffixFileFilter

import java.io.PrintWriter
import java.io.Writer
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.util.concurrent.atomic.AtomicInteger

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

  val headExtractor = new UWHeadExtractor(wordnetHome)

  // confidence functions
  val srlieConf = SrlConfidenceFunction.loadDefaultClassifier()
  val relnounConf = RelnounConfidenceFunction.loadDefaultClassifier()

  // sentence pre-processors
  val tokenizer = new ClearTokenizer()
  val postagger = new OpenNlpPostagger(tokenizer)
  val chunker = new OpenNlpChunker(postagger)

  // subextractors
  val relnoun = new Relnoun()
  val srlie = new SrlExtractor(srl)

  def toTerm(token: Lemmatized[ChunkedToken]) : Term = {
    Term(token.string, Some(token.lemma), Some(token.postag), Some(token.chunk))
  }

  def getArgHead[T <: PostaggedToken](tokens: Seq[Lemmatized[T]]) : Option[Seq[Term]] = {
    val unwrapped = tokens map { _.token }
    headExtractor.argumentHead(unwrapped) match {
      case tokens if !tokens.isEmpty => Some(tokens map { (tok) => Term(tok.string) })
      case Nil => None
    }
  }

  def getRelHead[T <: PostaggedToken](tokens: Seq[Lemmatized[T]]) : Option[Seq[Term]] = {
    val unwrapped = tokens map { _.token }
    headExtractor.relationHead(unwrapped) match {
      case tokens if !tokens.isEmpty => Some(tokens map { (tok) => Term(tok.string) })
      case Nil => None
    }
  }

  /**
   * Strip out all parenthetical content (like this right here) from sentences
   */
  private def preprocess(sentence: String) : String = {
    sentence.replaceAll("\\([^)]*\\)", "")
  }

  def extractFile(file: Path,
                  corpus: String, 
                  batchId: String, 
                  outputFile: Path) : Unit = 
  {
    val outPart = outputFile.resolveSibling(outputFile.getFileName().toString() + ".part")

    val input = Source.fromFile(file.toFile, "UTF-8")
    val output = new PrintWriter(
      new OutputStreamWriter(new FileOutputStream(outPart.toFile(), false), "UTF-8"))
     
    def nextId() : String = { 
      s"${corpus}_${batchId}_${idSeq.getAndIncrement()}"
    }

    for(sentence <- input.getLines()) {
      
      try {
        val extractions = extract(sentence, corpus, nextId)
        extractions.foreach( extraction => 
          output.println(ExtractionSerialization.toJson(extraction))
        )
      } catch {
        case t: Throwable => logger.error("Extraction failed for " + sentence, t);
      }
    }

    input.close()
    output.close()

    Files.move(outPart, outputFile)
  }

  /**
   * Given a sentence, calls subextractors and returns their extractions
   * converted to common Extraction Tuples.
   */
  def extract(sentence: String, corpus: String, nextId: () => String) : Seq[Extraction] = {

    // wrapper for a Seq[Interval] --- holdover from OpenIE
    case class Part(text: String, offsets: Seq[Interval])

    // pre-process the sentence
    val preprocessed = preprocess(sentence)
    val chunked = chunker(preprocessed) map MorphaStemmer.lemmatizePostaggedToken
    val sentenceDetails = chunked.map(toTerm)
    val parsed = parser(preprocessed)

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
 * FileExtractorMain extracts from sentence files (.sentence.txt) which must
 * contain one sentence per line, and writes out extraction json files (.extraction.json), one
 * extraction json object per line.
 *
 * It finds all sentence files in a given input directory (recursing into subdirectories), 
 * runs an Extractor over all of them, and writes corresponding files into a given output directory.
 *
 * The corpus assigned to the generated extractions is either the input filename (stripped of the
 * .sentences.txt suffix) or, if the file was found in a subdirectory of the inputDir, the top level
 * subirectory under the inputDir where the file was found.
 *
 * After each input file has been processed, if the doneDir is given, the input file is moved to the
 * doneDir.  If the doneDir is not given, the input file is deleted.  If processing a file fails, 
 * the file is moved to a given error directory.
 *
 * The Extractor requires WordNet to be installed on the local filesystem.
 *
 * This may be run as a daemon, in which case it will sit in a loop, polling the input directory
 * and continuously processing files.  If not run as a daemon, it will run once over all files in 
 * the input dir at the time of invocation.
 */
object FileExtractorMain {

  val logger = LoggerFactory.getLogger(this.getClass)

  val INPUT_SUFFIX = ".sentences.txt"
  val OUTPUT_SUFFIX = ".extractions.json"

  case class Config(val inputDir: Path = null,
                    val outputDir: Path = null,
                    val errorDir: Path = null,
                    val doneDir: Option[Path] = None,
                    val wnHome: String = null,
                    val daemon: Boolean = false,
                    val parserUrl: Option[String] = None,
                    val srlUrl: Option[String] = None)

  def parseArgs(args: Array[String]) : Option[Config] = {

    val parser = new OptionParser[Config]("ExtractorDaemon") {

      def options = Seq(
        opt("input-dir", "[required] directory to read input sentences files from") { 
          (param, c) => c.copy(inputDir = new File(param).getCanonicalFile.toPath)
        }, 

        opt("output-dir", "[required] directory to write output extractions to") { 
          (param, c) => c.copy(outputDir = new File(param).getCanonicalFile.toPath)
        },

        opt("error-dir", "[required] directory to move files which failed processing to") { 
          (param, c) => c.copy(errorDir = new File(param).getCanonicalFile.toPath)
        },

        opt("done-dir", "[optional] directory to move processed input files to, otherwise deleted") {
          (param, c) => c.copy(doneDir = Some(new File(param).getCanonicalFile.toPath))
        },

        opt("wordnet-home", "[required] location of wordnet on the local filesystem") { 
          (param, c) => c.copy(wnHome = param)
        },

        booleanOpt("daemon", "[optional] run as a daemon, continuously polling input-dir") { 
          (param, c) => c.copy(daemon = param)
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
        if(config.wnHome == null || 
           config.inputDir == null || !config.inputDir.toFile.isDirectory() ||
           config.outputDir == null || !config.outputDir.toFile.isDirectory() ||
           config.errorDir == null || !config.errorDir.toFile.isDirectory())
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

  def main(args: Array[String]) : Unit = {

    val config = parseArgs(args) match {
      case Some(c) => c
      case None => return
    }

    val fileExtractor = new FileExtractor(config.wnHome,
                                          config.inputDir,
                                          config.outputDir,
                                          config.errorDir,
                                          config.doneDir,
                                          Some(INPUT_SUFFIX),
                                          Some(OUTPUT_SUFFIX))
    if(config.daemon) {
      fileExtractor.runDaemon()
    } else {
      fileExtractor.processFiles()
    }
  }
}

class FileExtractor(wnHome: String,
                    inputDir: Path,
                    outputDir: Path,
                    errorDir: Path,
                    doneDir: Option[Path],
                    inputSuffix: Option[String],
                    outputSuffix: Option[String])
  extends FileProcessor(inputDir, outputDir, errorDir, doneDir, inputSuffix, outputSuffix)
{
  val extractor = new Extractor(wnHome)

  def processFile(inputFile: Path, outputFile: Path) = {

    // get the corpus from the name of the file, or the subdirectory it's in
    val relativeInputPath = inputDir.relativize(inputFile)
    val corpus = if(relativeInputPath.getNameCount() == 1) {
      val suffixLen = if(inputSuffix.isEmpty) 0 else inputSuffix.get.length
      relativeInputPath.toString().dropRight(suffixLen)
    } else {
      inputDir.relativize(inputFile).getName(0).toString
    }

    extractor.extractFile(inputFile, corpus, relativeInputPath.toString(), outputFile)
  }
}
