package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration

import cats.implicits.catsSyntaxSemigroup
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.{arbitraryContactDetails, genContactDetails}

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
