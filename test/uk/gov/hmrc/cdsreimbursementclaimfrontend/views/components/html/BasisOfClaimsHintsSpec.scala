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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.hints.DropdownHints

class BasisOfClaimsHintsSpec extends AnyWordSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  "The basis of claims examples" should {
    "drop N items" in {
      forAll(Gen.choose(0, 13)) { n =>
        DropdownHints.range(n, 13).keys should be((n to 13).map(_.toString))
      }
    }
  }
}
