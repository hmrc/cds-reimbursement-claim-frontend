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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.StringUtils._
import org.scalacheck.Gen

class StringUtilsSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {

  "StringUtils" should {
    "wrap a string as Some if non empty" in {
      forAll(Gen.nonEmptyListOf(Gen.alphaChar).map(String.valueOf)) { string: String =>
        whenever(string.trim().nonEmpty) {
          string.asSomeIfNonEmpty shouldBe Some(string)
        }
      }
    }

    "wrap a string as None if empty" in {
      "".asSomeIfNonEmpty    shouldBe None
      " ".asSomeIfNonEmpty   shouldBe None
      "  ".asSomeIfNonEmpty  shouldBe None
      "   ".asSomeIfNonEmpty shouldBe None
    }
  }

}
