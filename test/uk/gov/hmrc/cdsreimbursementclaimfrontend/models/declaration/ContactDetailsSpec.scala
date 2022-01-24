/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration

import cats.implicits.catsSyntaxSemigroup
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.arbitraryContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.genContactDetails

class ContactDetailsSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  "The contact details" should {

    "display an address" when {

      "mandatory fields provided" in forAll { contactDetails: ContactDetails =>
        contactDetails.showAddress shouldBe (
          contactDetails.addressLine1 |+|
            contactDetails.addressLine2.map(", " + _) |+|
            contactDetails.addressLine3.map(", " + _) |+|
            contactDetails.addressLine4.map(", " + _) |+|
            contactDetails.postalCode.map(", " + _)
        )
      }
    }

    "not to display an address" when {
      "address line 1 is missing" in forAll(genContactDetails, MinSuccessful(1)) { contactDetails =>
        contactDetails.copy(addressLine1 = None).showAddress shouldBe None
      }
    }

    "not to display an address" when {
      "post code is missing" in forAll(genContactDetails, MinSuccessful(1)) { contactDetails =>
        contactDetails.copy(postalCode = None).showAddress shouldBe None
      }
    }
  }
}
