/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.data.NonEmptyList
import cats.implicits.catsSyntaxOptionId
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim.newDraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

class DraftClaimSpec extends AnyWordSpec with ScalaCheckPropertyChecks with EitherValues with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 3)

  "The draft claim" should {
    "not contain mandatory contact details" when {
      "MRN contact address is not provided" in {
        forAll(genContactAddressOpt) { maybeContactAddress: Option[ContactAddress] =>
          newDraftC285Claim
            .copy(mrnContactAddressAnswer = maybeContactAddress)
            .isMandatoryContactDataAvailable should be(false)
        }
      }
    }

    "not contain mandatory contact details" when {
      "MRN contact details are not provided" in {
        forAll(genMrnContactDetailsOpt) { maybeContactDetails: Option[MrnContactDetails] =>
          newDraftC285Claim
            .copy(mrnContactDetailsAnswer = maybeContactDetails)
            .isMandatoryContactDataAvailable should be(false)
        }
      }
    }

    "contain mandatory contact details" when {
      "required data is provided" in {
        forAll { (contactDetails: MrnContactDetails, contactAddress: ContactAddress) =>
          newDraftC285Claim
            .copy(
              mrnContactAddressAnswer = contactAddress.some,
              mrnContactDetailsAnswer = contactDetails.some
            )
            .isMandatoryContactDataAvailable should be(true)
        }
      }
    }
  }

  "The total of MRNs" should {
    "be zero" when {
      "no MRNs added" in {
        newDraftC285Claim.MRNs.total should be(0)
      }
    }

    "be one" when {
      "only lead MRN is added and no other MRNs present" in {
        forAll { mrn: MovementReferenceNumber =>
          newDraftC285Claim
            .copy(movementReferenceNumber = mrn.some)
            .MRNs
            .total should be(1)
        }
      }
    }

    "equal to size of added MRNs combined with lead MRN" in {
      forAll(genMRN, Gen.nonEmptyListOf(genMRN)) { (mrn: MRN, mrns: List[MRN]) =>
        newDraftC285Claim
          .copy(
            movementReferenceNumber = MovementReferenceNumber(mrn).some,
            associatedMRNsAnswer = NonEmptyList.fromList(mrns)
          )
          .MRNs
          .total should be(1 + mrns.size)
      }
    }
  }

  "The list of MRNs" should {
    "be empty" in {
      newDraftC285Claim.MRNs() should be(Nil)
    }

    "contain only lead MRN" when {
      "no other MRNs added" in {
        forAll { mrn: MovementReferenceNumber =>
          newDraftC285Claim
            .copy(movementReferenceNumber = mrn.some)
            .MRNs() should be(List(mrn.value.value))
        }
      }
    }

    "contain all added MRNs" in {
      forAll(genMRN, Gen.nonEmptyListOf(genMRN)) { (mrn: MRN, mrns: List[MRN]) =>
        newDraftC285Claim
          .copy(
            movementReferenceNumber = MovementReferenceNumber(mrn).some,
            associatedMRNsAnswer = NonEmptyList.fromList(mrns)
          )
          .MRNs() should be(mrn +: mrns)
      }
    }
  }

  "The list of MRNs and declarations" should {
    "contain only associated MRNs and declarations" in {
      forAll { (mrn: Option[MovementReferenceNumber], mrns: List[MRN], declarations: List[DisplayDeclaration]) =>
        newDraftC285Claim
          .copy(
            movementReferenceNumber = mrn,
            associatedMRNsAnswer = NonEmptyList.fromList(mrns),
            associatedMRNsDeclarationAnswer = NonEmptyList.fromList(declarations)
          )
          .MRNs
          .combineWithDeclarations should be(mrns zip declarations)
      }
    }

    "be empty" in {
      forAll { maybeDeclaration: Option[DisplayDeclaration] =>
        newDraftC285Claim
          .copy(displayDeclaration = maybeDeclaration)
          .MRNs
          .combineWithDeclarations should be(Nil)
      }
    }

    "contain lead MRN and declaration combined with associated MRNs and declarations" in {
      forAll {
        (
          mrn: MovementReferenceNumber,
          declaration: DisplayDeclaration,
          mrns: List[MRN],
          declarations: List[DisplayDeclaration]
        ) =>
          newDraftC285Claim
            .copy(
              movementReferenceNumber = mrn.some,
              displayDeclaration = declaration.some,
              associatedMRNsAnswer = NonEmptyList.fromList(mrns),
              associatedMRNsDeclarationAnswer = NonEmptyList.fromList(declarations)
            )
            .MRNs
            .combineWithDeclarations should be((mrn.value.value, declaration) +: (mrns zip declarations))
      }
    }
  }
}
