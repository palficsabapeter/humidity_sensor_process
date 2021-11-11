package csaba.palfi
package io

import better.files.File

class FileProcessor[R](lineProcessor: (R, String) => R) {
  def processFile(file: File, empty: R): R =
    iterateAndProcess(dropHeader(file), empty)

  private def dropHeader(file: File) =
    file.lineIterator.drop(1)

  private def iterateAndProcess(iter: Iterator[String], empty: R) =
    iter.foldLeft(empty)(lineProcessor)
}
