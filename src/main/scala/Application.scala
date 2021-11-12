package csaba.palfi

import better.files.File
import io.DirectoryProcessor
import io.FileProcessor
import java.nio.file.NotDirectoryException
import model.SensorStatistics
import scala.util.Try

object Application {
  def main(args: Array[String]): Unit =
    args.headOption
      .map { dirStr =>
        Try {
          val inputDir = File(dirStr)
          if (inputDir.isDirectory) {
            val fileResults = processDirectory(inputDir)
            println(s"Num of processed files: ${fileResults.length}")
            new ResultsPrinter(fileResults).printResults()
          } else
            throw new NotDirectoryException(dirStr)
        }.recover { case ex: Throwable => ex.printStackTrace() }
      }
      .fold(println("No argument was given!"))(_ => ())

  private def processDirectory(
      inputDir: File,
  ): Seq[Map[String, SensorStatistics]] = {
    implicit val fileProcessor: FileProcessor[Map[String, SensorStatistics]] =
      new FileProcessor((acc, str) => {
        val key   = str.split(",").headOption.getOrElse("")
        val value = acc.getOrElse(key, SensorStatistics(name = key))
        acc + (key -> (value ++= str))
      })

    val directoryProcessor: DirectoryProcessor[Map[String, SensorStatistics]] =
      new DirectoryProcessor()

    directoryProcessor.processDirectory(inputDir, Map.empty)
  }
}
