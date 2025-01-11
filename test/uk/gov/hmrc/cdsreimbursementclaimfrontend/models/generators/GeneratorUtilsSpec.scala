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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalacheck.Arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class GeneratorUtilsSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  case class TestCase1(foo: TestCase2, bar: Int, zoo: String, opt: Option[Int] = Some(0))
  case class TestCase2(faz: String, zaz: Int, opt2: Option[TestCase3] = None) {
    def withFaz(faz2: String) = this.copy(faz = faz2)
    def withZaz(zaz2: Int)    = this.copy(zaz = zaz2)
  }
  case class TestCase3(tas: Boolean)

  implicit val testCaseArb: Arbitrary[TestCase1] = GeneratorUtils.gen[TestCase1]

  " GeneratorUtils" should {
    "derive generator from the case class" in {
      forAll(testCaseArb.arbitrary) { testCase =>
        testCase
      }
    }
  }
}
