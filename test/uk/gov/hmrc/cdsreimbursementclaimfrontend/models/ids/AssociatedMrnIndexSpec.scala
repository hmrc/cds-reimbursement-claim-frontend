/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndexSpec._
import play.api.i18n._

class AssociatedMrnIndexSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {

  implicit val messagesMap: Map[String, String] = Map(
    "ordinal.label.0"   -> "zero",
    "ordinal.label.1"   -> "first",
    "ordinal.label.2"   -> "second",
    "ordinal.label.3"   -> "third",
    "ordinal.label.4"   -> "fourth",
    "ordinal.label.5"   -> "fifth",
    "ordinal.label.6"   -> "sixth",
    "ordinal.label.7"   -> "seventh",
    "ordinal.label.8"   -> "eighth",
    "ordinal.label.9"   -> "ninth",
    "ordinal.suffix.0"  -> "th",
    "ordinal.suffix.1"  -> "st",
    "ordinal.suffix.1x" -> "th",
    "ordinal.suffix.2"  -> "nd",
    "ordinal.suffix.3"  -> "rd",
    "ordinal.suffix.4"  -> "th",
    "ordinal.suffix.5"  -> "th",
    "ordinal.suffix.6"  -> "th",
    "ordinal.suffix.7"  -> "th",
    "ordinal.suffix.8"  -> "th",
    "ordinal.suffix.9"  -> "th"
  )
  implicit val messagesApi: MessagesApi         = new DefaultMessagesApi(messages = Map("default" -> messagesMap))
  implicit val messages: Messages               = MessagesImpl(Lang.defaultLang, messagesApi)

  "Converting MRN and original index back and forth" should {
    "match" in {
      forAll { (original: Int) =>
        AssociatedMrnIndex.fromListIndex(original).toListIndex should be(original)
      }
    }
  }

  "Adding number to MRN index" should {
    "increase its value" in {
      forAll { (mrnIndex: AssociatedMrnIndex) =>
        mrnIndex + 2 should be(AssociatedMrnIndex(mrnIndex.urlIndex + 2))
      }
    }
  }

  "Subtracting number from MRN index" should {
    "decrease its value" in {
      forAll { (mrnIndex: AssociatedMrnIndex) =>
        (mrnIndex - 2) should be(AssociatedMrnIndex(mrnIndex.urlIndex - 2))
      }
    }
  }

  "Displaying ordinal numeral for the number 1" should {
    "contain `first`" in {
      AssociatedMrnIndex(1).ordinalNumeral should be("first")
    }
  }

  "Displaying ordinal numeral for the number ending with 1" should {
    "contain `st` postfix" in {
      forAll { (n: Int) =>
        whenever(n > 1) {
          val index = n.truncateToEndWithPrecision(1)
          AssociatedMrnIndex(index).ordinalNumeral.lastTwo should be("st")
        }
      }
    }
  }

  "Displaying ordinal numeral for the number ending with 2" should {
    "contain `nd` postfix" in {
      forAll { (n: Int) =>
        val index = n.truncateToEndWithPrecision(2)
        AssociatedMrnIndex(index).ordinalNumeral.lastTwo should be("nd")
      }
    }
  }

  "Displaying ordinal numeral for the number ending with 3" should {
    "contain `rd` postfix" in {
      forAll { (n: Int) =>
        val index = n.truncateToEndWithPrecision(3)
        AssociatedMrnIndex(index).ordinalNumeral.lastTwo should be("rd")
      }
    }
  }

  "Displaying ordinal numeral for the number which" should {
    "contain `th`" in {
      forAll(Gen.choose(4, 19)) { (n: Int) =>
        AssociatedMrnIndex(n).ordinalNumeral.lastTwo should be("th")
      }
    }
  }

  "The ordinal numeral from 0 to 9" should {
    "contain only words" in {
      forAll(Gen.choose(-9, 9)) { (index: Int) =>
        AssociatedMrnIndex(index).ordinalNumeral.matches("[minus a-z]+") should be(true)
      }
    }
  }
}

object AssociatedMrnIndexSpec {

  implicit class IntegerOps(private val n: Int) {
    def truncateToEndWithPrecision(x: Int): Int =
      (n / 10) * 10 + (if (n >= 0) x else -x)
  }

  implicit class StringOps(private val s: String) {
    def lastTwo: String = s.substring(s.length - 2)
  }
}
