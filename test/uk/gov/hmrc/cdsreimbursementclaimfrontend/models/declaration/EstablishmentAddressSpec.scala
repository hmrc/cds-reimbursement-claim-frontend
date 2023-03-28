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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.genContactAddress

class EstablishmentAddressSpec extends ControllerSpec with ScalaCheckPropertyChecks {

  "Establishment Address" should {
    "contain all of the details from a contact address" in {
      forAll(genContactAddress) { address =>
        whenever(address.line2.isDefined && address.line3.isDefined) {
          EstablishmentAddress.fromContactAddress(address) should have(
            Symbol("addressLine1")(address.line1),
            Symbol("addressLine2")(Some(s"${address.line2.get}, ${address.line3.get}")),
            Symbol("addressLine3")(Some(address.line4)),
            Symbol("postalCode")(Some(address.postcode)),
            Symbol("countryCode")(address.country.code)
          )
        }
      }
    }

    "have a blank line2 from a contact address with no line2 or line3" in {
      forAll(genContactAddress) { address =>
        val adjustedAddress = address.copy(line2 = None, line3 = None)
        EstablishmentAddress.fromContactAddress(adjustedAddress) should have(
          Symbol("addressLine1")(address.line1),
          Symbol("addressLine2")(None),
          Symbol("addressLine3")(Some(address.line4)),
          Symbol("postalCode")(Some(address.postcode)),
          Symbol("countryCode")(address.country.code)
        )
      }
    }
  }
}
