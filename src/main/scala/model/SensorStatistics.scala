package csaba.palfi
package model

case class SensorStatistics(
    name: String = "",
    min: Humidity = Humidity.NaN,
    max: Humidity = Humidity.NaN,
    acc: Humidity = Humidity.NaN,
    count: Int = 0,
    nanCount: Int = 0,
) {
  import SensorStatistics._

  def getAvgOpt: Option[Int] =
    acc.value.map(acc => (acc.toDouble / count).toInt)

  def +(stat: SensorStatistics): SensorStatistics =
    SensorStatistics.combine(this, stat)

  def ++=(str: String): SensorStatistics =
    this ++= SensorMeasurement.apply(str)

  private def ++=(measurement: SensorMeasurement): SensorStatistics =
    if (name.isEmpty || name == measurement.name)
      SensorStatistics.add(this, measurement)
    else
      this
}

object SensorStatistics {
  private def combine(
      stat1: SensorStatistics,
      stat2: SensorStatistics,
  ): SensorStatistics = {
    val minHum   = Humidity.chooseMin(stat1.min, stat2.min)
    val maxHum   = Humidity.chooseMax(stat1.max, stat2.max)
    val accHum   = stat1.acc + stat2.acc
    val count    = stat1.count + stat2.count
    val nanCount = stat1.nanCount + stat2.nanCount

    SensorStatistics(stat1.name, minHum, maxHum, accHum, count, nanCount)
  }

  private def add(
      acc: SensorStatistics,
      measurement: SensorMeasurement,
  ): SensorStatistics =
    if (!measurement.value.isNaN) {
      val newMin = Humidity.chooseMin(acc.min, measurement.value)
      val newMax = Humidity.chooseMax(acc.max, measurement.value)
      val newAcc = acc.acc + measurement.value
      acc.copy(min = newMin, max = newMax, acc = newAcc, count = acc.count + 1)
    } else
      acc.copy(nanCount = acc.nanCount + 1)

  case class SensorMeasurement(name: String, value: Humidity)

  object SensorMeasurement {
    def apply(str: String): SensorMeasurement = {
      val cols = str.split(",")
      SensorMeasurement(
        cols.headOption.getOrElse(""),
        Humidity.apply(cols.lastOption.getOrElse("NaN")),
      )
    }
  }
}
