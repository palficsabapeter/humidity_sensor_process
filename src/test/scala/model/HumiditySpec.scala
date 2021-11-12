package csaba.palfi
package model

import testutil.TestBase

class HumiditySpec extends TestBase {
  "Humidity" should {
    "#isNaN" should {
      "return true if humidity value is None" in {
        Humidity(None).isNaN shouldBe true
      }

      "return false if humidity value is Some" in {
        Humidity(Some(1)).isNaN shouldBe false
      }
    }

    "#+" should {
      "return the value sum of two Humidity objects with existing values" in {
        Humidity(Some(1)) + Humidity(Some(2)) shouldBe Humidity(Some(3))
      }

      "return NaN value if neither Humidity objects has existing values" in {
        Humidity(None) + Humidity(None) shouldBe Humidity.NaN
      }

      "return the existing value if only one Humidity object has existing value" in {
        Humidity(None) + Humidity(Some(1)) shouldBe Humidity(Some(1))
        Humidity(Some(2)) + Humidity(None) shouldBe Humidity(Some(2))
      }
    }

    "#apply" should {
      "return with None value if the input is 'NaN'" in {
        Humidity.apply("NaN") shouldBe Humidity(None)
      }

      "return with Some value if the input is a number" in {
        Humidity.apply("1") shouldBe Humidity(Some(1))
      }

      "throw a NumberFormatException if input is not a number" in {
        intercept[NumberFormatException](Humidity.apply("not a number"))
      }
    }

    "#NaN" should {
      "return with None value" in {
        Humidity.NaN shouldBe Humidity(None)
      }
    }

    "#chooseMin" should {
      "choose smaller value" in {
        Humidity.chooseMin(
          Humidity(Some(1)),
          Humidity(Some(2)),
        ) shouldBe Humidity(Some(1))

        Humidity.chooseMin(
          Humidity(Some(3)),
          Humidity(Some(2)),
        ) shouldBe Humidity(Some(2))
      }

      "choose existing value if other is None" in {
        Humidity.chooseMin(Humidity(Some(1)), Humidity(None)) shouldBe Humidity(
          Some(1),
        )

        Humidity.chooseMin(Humidity(None), Humidity(Some(2))) shouldBe Humidity(
          Some(2),
        )
      }

      "return with None if neither Humidty has existing value" in {
        Humidity.chooseMin(Humidity(None), Humidity(None)) shouldBe Humidity(
          None,
        )
      }
    }

    "#chooseMax" should {
      "choose greater value" in {
        Humidity.chooseMax(
          Humidity(Some(1)),
          Humidity(Some(2)),
        ) shouldBe Humidity(Some(2))

        Humidity.chooseMax(
          Humidity(Some(3)),
          Humidity(Some(2)),
        ) shouldBe Humidity(Some(3))
      }

      "choose existing value if other is None" in {
        Humidity.chooseMax(Humidity(Some(1)), Humidity(None)) shouldBe Humidity(
          Some(1),
        )

        Humidity.chooseMax(Humidity(None), Humidity(Some(2))) shouldBe Humidity(
          Some(2),
        )
      }

      "return with None if neither Humidity has existing value" in {
        Humidity.chooseMax(Humidity(None), Humidity(None)) shouldBe Humidity(
          None,
        )
      }
    }
  }
}
