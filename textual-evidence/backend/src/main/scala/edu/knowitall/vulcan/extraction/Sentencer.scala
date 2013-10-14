package edu.knowitall.vulcan.extraction

import edu.knowitall.tool.sentence.OpenNlpSentencer

import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter

import java.nio.file.Path
import java.nio.file.Files

import scala.collection.JavaConverters._

import scala.io.Source

import scopt.immutable.OptionParser

class SentenceSplitter {

  val sentencer = new OpenNlpSentencer()

  def sentenceFilter(sentence: String) = {
    val terminatingCharacters = Set('.', '?', '!')
    sentence.length > 5 && terminatingCharacters(sentence.last) && sentence.length < 400
  }

  def sentenceMap(sentence: String) = {
    sentence.trim.replaceAll("\t", " ")
  }

  def sentenceFile(inputFile: Path, outputFile: Path) = {

    val input = Source.fromFile(inputFile.toFile, "UTF-8")
    val outPart = outputFile.resolveSibling(outputFile.getFileName().toString() + ".part")
    val output = new PrintWriter(
      new OutputStreamWriter(new FileOutputStream(outPart.toFile(), false), "UTF-8"))

    val lines = input.getLines.buffered
    while (lines.hasNext) {
      var segment: Vector[String] = Vector.empty
      while (lines.hasNext && !lines.head.trim.isEmpty) {
        segment = segment :+ lines.next
      }

      // skip over whitespace line
      if (lines.hasNext) lines.next

      val sentences = sentencer(segment.mkString(" "))
      sentences.iterator.map(_.text).map(sentenceMap).filter(sentenceFilter) foreach output.println
    }

    input.close()
    output.close()

    Files.move(outPart, outputFile)
  }
}

class FileSentenceSplitter(inputDir: Path,
                           outputDir: Path,
                           errorDir: Path,
                           doneDir: Option[Path],
                           inputSuffix: Option[String],
                           outputSuffix: Option[String])
  extends FileProcessor(inputDir, outputDir, errorDir, doneDir, inputSuffix, outputSuffix)
{
  val sentencer = new SentenceSplitter()

  def processFile(inputFile: Path, outputFile: Path) = {
    sentencer.sentenceFile(inputFile, outputFile)
  }
}

object FileSentenceSplitterMain {

  val OUTPUT_SUFFIX = ".sentences.txt"

  case class Config(val inputDir: Path = null,
                    val outputDir: Path = null,
                    val errorDir: Path = null,
                    val doneDir: Option[Path] = None,
                    val daemon: Boolean = false)

  def parseArgs(args: Array[String]) : Option[Config] = {

    val parser = new OptionParser[Config]("ExtractorDaemon") {

      def options = Seq(
        opt("input-dir", "[required] directory to read input files from") { 
          (param, c) => c.copy(inputDir = new File(param).getCanonicalFile.toPath)
        }, 

        opt("output-dir", "[required] directory to write output sentences to") { 
          (param, c) => c.copy(outputDir = new File(param).getCanonicalFile.toPath)
        },

        opt("error-dir", "[required] directory to move files which failed processing to") { 
          (param, c) => c.copy(errorDir = new File(param).getCanonicalFile.toPath)
        },

        opt("done-dir", "[optional] directory to move processed input files to, otherwise deleted") {
          (param, c) => c.copy(doneDir = Some(new File(param).getCanonicalFile.toPath))
        },

        booleanOpt("daemon", "[optional] run as a daemon, continuously polling input-dir") { 
          (param, c) => c.copy(daemon = param)
        }
      )
    }

    parser.parse(args, Config()) match {
      case Some(config) => {
        if(config.inputDir == null || !config.inputDir.toFile.isDirectory() ||
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

    val sentenceSplitter = new FileSentenceSplitter(config.inputDir,
                                                    config.outputDir,
                                                    config.errorDir,
                                                    config.doneDir,
                                                    None, // don't filter input
                                                    Some(OUTPUT_SUFFIX))
    if(config.daemon) {
      sentenceSplitter.runDaemon()
    } else {
      sentenceSplitter.processFiles()
    }
  }
}
