package csaba.palfi
package io

import better.files.File
import org.mockito.ArgumentMatchersSugar
import org.mockito.MockitoSugar
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class DirectoryProcessorSpec
    extends AnyWordSpecLike
    with Matchers
    with MockitoSugar
    with ArgumentMatchersSugar {
  trait TestScope {
    implicit val fpMock: FileProcessor[Int] = mock[FileProcessor[Int]]
    val dp: DirectoryProcessor[Int]         = new DirectoryProcessor[Int]()
  }

  "DirectoryProcessor" should {
    "#processDirectory" should {
      "return Seq of 2 Ints for 2 .csv and one other file in the directory" in new TestScope {
        File.usingTemporaryDirectory() { tempDir =>
          File.usingTemporaryFile(suffix = ".csv", parent = Some(tempDir)) {
            _ =>
              File.usingTemporaryFile(suffix = ".csv", parent = Some(tempDir)) {
                _ =>
                  when(fpMock.processFile(any[File], any[Int])).thenReturn(1)

                  dp.processDirectory(tempDir, 0) shouldBe Seq(1, 1)

                  verify(fpMock, times(2)).processFile(any[File], any[Int])
              }
          }
        }
      }

      "return empty Seq if there were no .csv files in the directory" in new TestScope {
        File.usingTemporaryDirectory() { tempDir =>
          File.usingTemporaryFile(prefix = "csv", parent = Some(tempDir)) { _ =>
            File.usingTemporaryFile(parent = Some(tempDir)) { _ =>
              dp.processDirectory(tempDir, 0) shouldBe Seq.empty

              verify(fpMock, times(0)).processFile(any[File], any[Int])
            }
          }
        }
      }
    }
  }
}
