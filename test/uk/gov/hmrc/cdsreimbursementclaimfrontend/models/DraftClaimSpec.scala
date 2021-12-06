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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DeclarantTypeAnswer._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.genAcc14WithAddresses
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

class DraftClaimSpec extends AnyWordSpec with ScalaCheckPropertyChecks with EitherValues with Matchers {

  implicit override val generatorDrivenConfig =
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
      "only lead MRN is added and no other MRNs present" in {
        forAll { mrn: MRN =>
          DraftClaim.blank
            .copy(movementReferenceNumber = mrn.some)
            .MRNs
            .total should be(1)
        }
      }
    }

    "equal to size of added MRNs combined with lead MRN" in {
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

    "contain only lead MRN" when {
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

    "contain lead MRN and declaration combined with associated MRNs and declarations" in {
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
    "Extract contact details for DeclarantTypeAnswer.Importer" in {
      val verifiedEmail  = sample[Email]
      val acc14          = genAcc14WithAddresses
      val acc14consignee = acc14.displayResponseDetail.consigneeDetails
      val initialClaim   = sample[DraftClaim]
      val draftClaim     = initialClaim.copy(
        displayDeclaration = Some(acc14),
        declarantTypeAnswer = Some(Importer)
      )

      val namePhoneEmail =
        draftClaim.extractDetailsRegisteredWithCDS(verifiedEmail)
      namePhoneEmail.name                     shouldBe acc14consignee.map(_.legalName)
      namePhoneEmail.phoneNumber.map(_.value) shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.telephone)
      namePhoneEmail.email.getOrElse(fail())  shouldBe verifiedEmail
    }

    "Extract establishment address for DeclarantTypeAnswer.Importer" in {
      val acc14          = genAcc14WithAddresses
      val acc14consignee = acc14.displayResponseDetail.consigneeDetails
      val initialClaim   = sample[DraftClaim]
      val draftClaim     = initialClaim.copy(
        displayDeclaration = Some(acc14),
        declarantTypeAnswer = Some(Importer)
      )

      val address = draftClaim.extractEstablishmentAddress
      address shouldBe acc14consignee.map(_.establishmentAddress)
    }

    "Extract contact details for DeclarantTypeAnswer.AssociatedWithImporterCompany" in {
      val verifiedEmail  = sample[Email]
      val acc14          = genAcc14WithAddresses
      val acc14consignee = acc14.displayResponseDetail.consigneeDetails
      val initialClaim   = sample[DraftClaim]
      val draftClaim     = initialClaim.copy(
        displayDeclaration = Some(acc14),
        declarantTypeAnswer = Some(AssociatedWithImporterCompany)
      )

      val namePhoneEmail =
        draftClaim.extractDetailsRegisteredWithCDS(verifiedEmail)
      namePhoneEmail.name                     shouldBe acc14consignee.map(_.legalName)
      namePhoneEmail.phoneNumber.map(_.value) shouldBe acc14consignee.flatMap(_.contactDetails).flatMap(_.telephone)
      namePhoneEmail.email.getOrElse(fail())  shouldBe verifiedEmail
    }

    "Extract establishment address for DeclarantTypeAnswer.AssociatedWithImporterCompany" in {
      val acc14          = genAcc14WithAddresses
      val acc14consignee = acc14.displayResponseDetail.consigneeDetails
      val initialClaim   = sample[DraftClaim]
      val draftClaim     = initialClaim.copy(
        displayDeclaration = Some(acc14),
        declarantTypeAnswer = Some(AssociatedWithImporterCompany)
      )

      val address = draftClaim.extractEstablishmentAddress
      address shouldBe acc14consignee.map(_.establishmentAddress)
    }

    "Extract contact details for DeclarantTypeAnswer.AssociatedWithRepresentativeCompany" in {
      val verifiedEmail  = sample[Email]
      val acc14          = genAcc14WithAddresses
      val acc14Declarant = acc14.displayResponseDetail.declarantDetails
      val initialClaim   = sample[DraftClaim]
      val draftClaim     = initialClaim.copy(
        displayDeclaration = Some(acc14),
        declarantTypeAnswer = Some(AssociatedWithRepresentativeCompany)
      )

      val namePhoneEmail =
        draftClaim.extractDetailsRegisteredWithCDS(verifiedEmail)
      namePhoneEmail.name                     shouldBe Some(acc14Declarant.legalName)
      namePhoneEmail.phoneNumber.map(_.value) shouldBe acc14Declarant.contactDetails.flatMap(_.telephone)
      namePhoneEmail.email.getOrElse(fail())  shouldBe verifiedEmail
    }

    "Extract establishment address for DeclarantTypeAnswer.AssociatedWithRepresentativeCompany" in {
      val acc14          = genAcc14WithAddresses
      val acc14Declarant = acc14.displayResponseDetail.declarantDetails
      val initialClaim   = sample[DraftClaim]
      val draftClaim     = initialClaim.copy(
        displayDeclaration = Some(acc14),
        declarantTypeAnswer = Some(AssociatedWithRepresentativeCompany)
      )

      val address = draftClaim.extractEstablishmentAddress
      address shouldBe Some(acc14Declarant.establishmentAddress)
    }
  }
}
