package edu.knowitall.vulcan.extraction

import java.io.File
import java.nio.file.Files
import java.nio.file.Path

import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import org.apache.commons.io.filefilter.SuffixFileFilter

import scala.collection.JavaConverters._

import org.slf4j.LoggerFactory

abstract class FileProcessor(inputDir: Path,
                             outputDir: Path,
                             errorDir: Path,
                             doneDir: Option[Path],
                             inputSuffix: Option[String] = None,
                             outputSuffix: Option[String] = None)
{
  val logger = LoggerFactory.getLogger(this.getClass)

  def processFile(inputFile: Path, outputFile: Path)

  /**
   * Sits in a loop polling for and processing files; never returns.
   */
  def runDaemon() : Unit = {

    while(true) {

      Thread.sleep(1000)
      
      processFiles()
    }
  }

  /**
   * Performs a single pass of file processing logic.
   */
  def processFiles() : Unit = {

    val files = FileUtils.listFiles(inputDir.toFile(),
                                    fileFilter,
                                    TrueFileFilter.INSTANCE).asScala

    for(file <- files) {

      logger.info("Processing file: " + file.getPath())

      val filePath = file.toPath()
      val fileName = file.getName()
      val relativeInputPath = inputDir.relativize(filePath)

      val relativeOutputDir = resolveAndMkDirs(outputDir, relativeInputPath).getParent()
      val outputFileName = (inputSuffix, outputSuffix) match {
        case (Some(in), out) => fileName.replaceAll(in + "$", out.getOrElse(""))
        case (None, out)     => fileName + out.getOrElse("")
      }
      val outputPath = relativeOutputDir.resolve(outputFileName)

      try {

        processFile(filePath, outputPath)
        
        // after file is processed, move it to done/ or delete it
        doneDir match {
          case Some(doneRoot) => {
            val donePath = resolveAndMkDirs(doneRoot, relativeInputPath)
            Files.move(filePath, donePath)
          }
          case None => Files.delete(filePath)
        }

      } catch {

        case t: Throwable => {
          logger.error("Failed to process: " + filePath, t) 
          val errorPath = resolveAndMkDirs(errorDir, relativeInputPath)
          Files.move(filePath, errorPath)
        }
      }

      // now if directory that contained the file is now empty, remove it
      var pathPart = filePath.getParent()
      while(pathPart != null && 
            !pathPart.equals(inputDir) &&
            pathPart.toFile().list().length == 0)
      {
        Files.delete(pathPart)
        pathPart = pathPart.getParent()
      }
    }
  }

  /**
   * How to match files to be processed.
   */
  private val fileFilter = inputSuffix match {
    case Some(suffix) => new SuffixFileFilter(suffix)
    case None => TrueFileFilter.INSTANCE
  }

  /**
   * Given a relative file path, creates and returns a Path under 
   * the given root directory mirroring that of given file path with any
   * necessary intermediary subdirectories created, e.g.
   *   resolveAndMkDirs("/foo/bar", "qux/file.txt") => "/foo/bar/qux/file.txt"
   *   resolveAndMkDirs("/foo/bar", "file.txt") => "/foo/bar/file.txt"
   */
  private def resolveAndMkDirs(root: Path, file: Path) : Path = {
    val subDir = file.getParent()
    if(subDir == null) {
      root.resolve(file.getFileName())
    } else {
      val resolvedSubdir = root.resolve(subDir)
      Files.createDirectories(resolvedSubdir)
      resolvedSubdir.resolve(file.getFileName())
    }
  }
}

