package csaba.palfi
package io

import better.files.File

class DirectoryProcessor[R](
    implicit
    fileProcessor: FileProcessor[R],
) {
  def processDirectory(inputDir: File, acc: R): Seq[R] =
    listCsvsBySize(inputDir).map(fileProcessor.processFile(_, acc))

  private def listCsvsBySize(inputDir: File) =
    inputDir
      .list(_.extension.contains(".csv"), 1)
      .toSeq
      .sorted(File.Order.bySize)
}
