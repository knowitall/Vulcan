package edu.knowitall.vulcan.extraction

import java.io.File
import java.io.PrintWriter

import scala.io.Source
import scala.collection.JavaConverters._

import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter

import scopt.immutable.OptionParser

import edu.knowitall.common.Resource
import edu.knowitall.tool.sentence.OpenNlpSentencer

object SentenceSplitter extends App {

  case class Config(
    inputFile: File = null,
    outputFile: File = null)

  val parser = new OptionParser[Config]("sentence-splitter") {
    def options = Seq(
      argOpt("input-file", "[required] input file or directory") { (string, config) =>
        val file = new File(string)
        require(file.exists, "input file does not exist: " + file)
        config.copy(inputFile = file)
      },
      argOpt("output-file", "[required] output file or directory") { (string, config) =>
        val file = new File(string)
        config.copy(outputFile = file)
      })
  }

  parser.parse(args, Config()) match {
    case Some(config) => 
      if(config.inputFile == null || config.outputFile == null) parser.showUsage
      else run(config)
    case None =>
  }

  def sentenceFilter(sentence: String) = {
    val terminatingCharacters = Set('.', '?', '!')
    sentence.length > 5 && terminatingCharacters(sentence.last) && sentence.length < 400
  }

  def sentenceMap(sentence: String) = {
    sentence.trim.replaceAll("\t", " ")
  }

  def run(config: Config) {

    val sentencer = new OpenNlpSentencer()

    val inputFiles = FileUtils.listFiles(config.inputFile, 
                                         TrueFileFilter.INSTANCE, // match all files
                                         TrueFileFilter.INSTANCE) // recursively in all subdirs

    for (inputFile <- inputFiles.asScala) {

      println("Processing: " + inputFile)

      val subdirectory = inputFile.getParentFile.getPath.drop(config.inputFile.getPath.size)
      val outputDir = new File(config.outputFile, subdirectory)
      outputDir.mkdirs()

      val outputFileName = inputFile.getName() + ".sentences"
      val outputFile = new File(outputDir, outputFileName)

      if (!outputFile.exists()) {
        Resource.using(Source.fromFile(inputFile, "UTF-8")) { source =>
          val lines = source.getLines.buffered
          Resource.using(new PrintWriter(outputFile, "UTF-8")) { writer =>
            while (lines.hasNext) {
              var segment: Vector[String] = Vector.empty
              while (lines.hasNext && !lines.head.trim.isEmpty) {
                segment = segment :+ lines.next
              }

              // skip over whitespace line
              if (lines.hasNext) lines.next

              val sentences = sentencer(segment.mkString(" "))
              sentences.iterator.map(_.text).map(sentenceMap).filter(sentenceFilter) foreach writer.println
            }

            println("Written to: " + outputFile)
          }
        }
      }
      else {
        println("Skipping because output file exists: " + inputFile)
      }

      println()
    }
  }
}
