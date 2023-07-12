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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.data.NonEmptyList
import cats.implicits.catsSyntaxOptionId
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

class DraftClaimSpec extends AnyWordSpec with ScalaCheckPropertyChecks with EitherValues with Matchers {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 3)

  "The draft claim" should {
    "not contain mandatory contact details" when {
      "MRN contact address is not provided" in {
        forAll(genContactAddressOpt) { maybeContactAddress: Option[ContactAddress] =>
          DraftClaim.blank
            .copy(mrnContactAddressAnswer = maybeContactAddress)
            .isMandatoryContactDataAvailable should be(false)
        }
      }
    }

    "not contain mandatory contact details" when {
      "MRN contact details are not provided" in {
        forAll(genMrnContactDetailsOpt) { maybeContactDetails: Option[MrnContactDetails] =>
          DraftClaim.blank
            .copy(mrnContactDetailsAnswer = maybeContactDetails)
            .isMandatoryContactDataAvailable should be(false)
        }
      }
    }

    "contain mandatory contact details" when {
      "required data is provided" in {
        forAll { (contactDetails: MrnContactDetails, contactAddress: ContactAddress) =>
          DraftClaim.blank
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
        DraftClaim.blank.MRNs.total should be(0)
      }
    }

    "be one" when {
      "only first MRN is added and no other MRNs present" in {
        forAll { mrn: MRN =>
          DraftClaim.blank
            .copy(movementReferenceNumber = mrn.some)
            .MRNs
            .total should be(1)
        }
      }
    }

    "equal to size of added MRNs combined with first MRN" in {
      forAll(genMRN, Gen.nonEmptyListOf(genMRN)) { (mrn: MRN, mrns: List[MRN]) =>
        DraftClaim.blank
          .copy(
            movementReferenceNumber = mrn.some,
            associatedMRNsAnswer = NonEmptyList.fromList(mrns)
          )
          .MRNs
          .total should be(1 + mrns.size)
      }
    }
  }

  "The list of MRNs" should {
    "be empty" in {
      DraftClaim.blank.MRNs() should be(Nil)
    }

    "contain only first MRN" when {
      "no other MRNs added" in {
        forAll { mrn: MRN =>
          DraftClaim.blank
            .copy(movementReferenceNumber = mrn.some)
            .MRNs() should be(List(mrn))
        }
      }
    }

    "contain all added MRNs" in {
      forAll(genMRN, Gen.nonEmptyListOf(genMRN)) { (mrn: MRN, mrns: List[MRN]) =>
        DraftClaim.blank
          .copy(
            movementReferenceNumber = mrn.some,
            associatedMRNsAnswer = NonEmptyList.fromList(mrns)
          )
          .MRNs() should be(mrn +: mrns)
      }
    }
  }

  "The list of MRNs and declarations" should {
    "contain only associated MRNs and declarations" in {
      forAll { (mrn: Option[MRN], mrns: List[MRN], declarations: List[DisplayDeclaration]) =>
        DraftClaim.blank
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
        DraftClaim.blank
          .copy(displayDeclaration = maybeDeclaration)
          .MRNs
          .combineWithDeclarations should be(Nil)
      }
    }

    "contain first MRN and declaration combined with associated MRNs and declarations" in {
      forAll {
        (
          mrn: MRN,
          declaration: DisplayDeclaration,
          mrns: List[MRN],
          declarations: List[DisplayDeclaration]
        ) =>
          DraftClaim.blank
            .copy(
              movementReferenceNumber = mrn.some,
              displayDeclaration = declaration.some,
              associatedMRNsAnswer = NonEmptyList.fromList(mrns),
              associatedMRNsDeclarationAnswer = NonEmptyList.fromList(declarations)
            )
            .MRNs
            .combineWithDeclarations should be((mrn, declaration) +: (mrns zip declarations))
      }
    }
  }

  "CDS Details Extraction" should {

    "Extract establishment address for DeclarantTypeAnswer.Importer" in {
      val eori                = sample[Eori]
      val signedInUserDetails = sample[SignedInUserDetails].copy(eori = eori)
      val acc14               = genAcc14WithoutContactDetails.withConsigneeEori(eori)
      val acc14consignee      = acc14.displayResponseDetail.consigneeDetails
      val initialClaim        = sample[DraftClaim]
      val draftClaim          = initialClaim.copy(
        displayDeclaration = Some(acc14)
      )

      val address = draftClaim.extractEstablishmentAddress(signedInUserDetails)
      address shouldBe acc14consignee.map(_.establishmentAddress)
    }

    "Extract establishment address for DeclarantTypeAnswer.AssociatedWithImporterCompany" in {
      val eori                = sample[Eori]
      val signedInUserDetails = sample[SignedInUserDetails].copy(eori = eori)
      val acc14               = genAcc14WithoutContactDetails.withDeclarantEori(eori)
      val acc14consignee      = acc14.displayResponseDetail.consigneeDetails
      val initialClaim        = sample[DraftClaim]
      val draftClaim          = initialClaim.copy(
        displayDeclaration = Some(acc14)
      )

      val address = draftClaim.extractEstablishmentAddress(signedInUserDetails)
      address shouldBe acc14consignee.map(_.establishmentAddress)
    }

    "Extract establishment address for DeclarantTypeAnswer.AssociatedWithRepresentativeCompany" in {
      val eori                = sample[Eori]
      val signedInUserDetails = sample[SignedInUserDetails].copy(eori = eori)
      val acc14               = genAcc14WithoutContactDetails
      val acc14Declarant      = acc14.displayResponseDetail.declarantDetails
      val initialClaim        = sample[DraftClaim]
      val draftClaim          = initialClaim.copy(
        displayDeclaration = Some(acc14)
      )

      val address = draftClaim.extractEstablishmentAddress(signedInUserDetails)
      address shouldBe Some(acc14Declarant.establishmentAddress)
    }
  }
}
