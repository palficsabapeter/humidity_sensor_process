package csaba.palfi
package model

import testutil.TestBase

class SensorStatisticsSpec extends TestBase {
  "SensorStatistics" should {
    "#getAvgOpt" should {
      "return average of Humidity" in {
        SensorStatistics(
          acc = Humidity(Some(10)),
          count = 2,
        ).getAvgOpt shouldBe Some(5)

        SensorStatistics(
          acc = Humidity(Some(11)),
          count = 2,
        ).getAvgOpt shouldBe Some(5)
      }

      "return None if there was no measurement" in {
        SensorStatistics().getAvgOpt shouldBe None
      }
    }

    "#+" should {
      "combine two SensorStatistics" in {
        SensorStatistics(
          min = Humidity(Some(0)),
          max = Humidity(Some(1)),
          acc = Humidity(Some(1)),
          count = 2,
          nanCount = 1,
        ) +
          SensorStatistics(
            min = Humidity(Some(2)),
            max = Humidity(Some(3)),
            acc = Humidity(Some(5)),
            count = 2,
          ) shouldBe SensorStatistics(
            min = Humidity(Some(0)),
            max = Humidity(Some(3)),
            acc = Humidity(Some(6)),
            count = 4,
            nanCount = 1,
          )

        SensorStatistics(
          nanCount = 1,
        ) +
          SensorStatistics(
            min = Humidity(Some(2)),
            max = Humidity(Some(3)),
            acc = Humidity(Some(5)),
            count = 2,
          ) shouldBe SensorStatistics(
            min = Humidity(Some(2)),
            max = Humidity(Some(3)),
            acc = Humidity(Some(5)),
            count = 2,
            nanCount = 1,
          )
      }
    }

    "#++=" should {
      "add new measurement to SensorStatistic from String input" in {
        ((SensorStatistics(
          min = Humidity(Some(0)),
          max = Humidity(Some(1)),
          acc = Humidity(Some(1)),
          count = 2,
          nanCount = 1,
        ) ++= "NaN") ++= "3") shouldBe SensorStatistics(
          min = Humidity(Some(0)),
          max = Humidity(Some(3)),
          acc = Humidity(Some(4)),
          count = 3,
          nanCount = 2,
        )

        (((SensorStatistics(
          nanCount = 3,
        ) ++= "5") ++= "0") ++= "NaN") shouldBe SensorStatistics(
          min = Humidity(Some(0)),
          max = Humidity(Some(5)),
          acc = Humidity(Some(5)),
          count = 2,
          nanCount = 4,
        )
      }
    }
  }
}
