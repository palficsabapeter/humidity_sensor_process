package csaba.palfi
package model

case class Humidity(value: Option[Int]) {
  def isNaN: Boolean = value.isEmpty

  def +(h: Humidity): Humidity =
    Humidity.add(this, h)
}

object Humidity {
  def apply(str: String): Humidity =
    str match {
      case "NaN" => Humidity(None)
      case num   => Humidity(Some(num.toInt))
    }

  def NaN: Humidity = Humidity(None)

  def chooseMin(h1: Humidity, h2: Humidity): Humidity =
    (h1.value, h2.value) match {
      case (None, None)         => h1
      case (Some(_), None)      => h1
      case (None, Some(_))      => h2
      case (Some(v1), Some(v2)) => if (v1 < v2) h1 else h2
    }

  def chooseMax(h1: Humidity, h2: Humidity): Humidity =
    (h1.value, h2.value) match {
      case (None, None)         => h1
      case (Some(_), None)      => h1
      case (None, Some(_))      => h2
      case (Some(v1), Some(v2)) => if (v1 < v2) h2 else h1
    }

  private def add(h1: Humidity, h2: Humidity): Humidity =
    (h1.value, h2.value) match {
      case (None, None)         => Humidity.NaN
      case (Some(_), None)      => h1
      case (None, Some(_))      => h2
      case (Some(v1), Some(v2)) => Humidity(Some(v1 + v2))
    }
}
