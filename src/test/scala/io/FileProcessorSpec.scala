package csaba.palfi
package io

import better.files.File
import testutil.TestBase

class FileProcessorSpec extends TestBase {
  "FileProcessor" should {
    "#processFile" should {
      "return with 5" in {
        File.usingTemporaryFile() { f =>
          val temp = f.writeText("""header
               |1
               |1
               |1
               |1
               |1
               |""".stripMargin)

          val fp: FileProcessor[Int] =
            new FileProcessor[Int]((acc: Int, str: String) => acc + str.toInt)

          fp.processFile(temp, 0) shouldBe 5
        }
      }
    }
  }
}
