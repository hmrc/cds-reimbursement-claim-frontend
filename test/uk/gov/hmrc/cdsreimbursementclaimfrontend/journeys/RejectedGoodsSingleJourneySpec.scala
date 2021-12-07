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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen
import RejectedGoodsSingleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer
import cats.data.{ReaderT, Validated}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import org.scalacheck.Gen

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class RejectedGoodsSingleJourneySpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with RejectedGoodsSingleJourneyTestData {

  "RejectedGoodsSingleJourney" should {
    "have an empty instance" in {
      emptyJourney.answers.userEoriNumber                   shouldBe exampleEori
      emptyJourney.answers.bankAccountDetails               shouldBe None
      emptyJourney.answers.bankAccountType                  shouldBe None
      emptyJourney.answers.basisOfClaim                     shouldBe None
      emptyJourney.answers.basisOfClaimSpecialCircumstances shouldBe None
      emptyJourney.answers.contactAddress                   shouldBe None
      emptyJourney.answers.contactDetails                   shouldBe None
      emptyJourney.answers.contactAddress                   shouldBe None
      emptyJourney.answers.declarantEoriNumber              shouldBe None
      emptyJourney.answers.detailsOfRejectedGoods           shouldBe None
      emptyJourney.answers.displayDeclaration               shouldBe None
      emptyJourney.answers.consigneeEoriNumber              shouldBe None
      emptyJourney.answers.inspectionAddress                shouldBe None
      emptyJourney.answers.inspectionDate                   shouldBe None
      emptyJourney.answers.methodOfDisposal                 shouldBe None
      emptyJourney.answers.reimbursementClaims              shouldBe None
      emptyJourney.answers.reimbursementMethod              shouldBe None
      emptyJourney.answers.supportingEvidences              shouldBe None
      emptyJourney.getNdrcDetails                           shouldBe None
      emptyJourney.getSelectedDuties                        shouldBe None
      emptyJourney.isAllSelectedDutiesAreCMAEligible        shouldBe false
      emptyJourney.isCompleteReimbursementClaims            shouldBe false
      emptyJourney.isCompleteSupportingEvidences            shouldBe false
      emptyJourney.isComplete                               shouldBe false
      emptyJourney.toOutput.isLeft                          shouldBe true
    }

    "check completeness and produce the correct output" in {
      forAll(completeJourneyGen) { journey =>
        RejectedGoodsSingleJourney.validator.apply(journey) shouldBe Validated.Valid(())
        journey.isComplete                                  shouldBe true
        val output = journey.toOutput.getOrElse(fail("Journey output not defined."))
        output.movementReferenceNumber  shouldBe journey.answers.movementReferenceNumber.get
        output.declarantType            shouldBe journey.getDeclarantType
        output.basisOfClaim             shouldBe journey.answers.basisOfClaim.get
        output.methodOfDisposal         shouldBe journey.answers.methodOfDisposal.get
        output.detailsOfRejectedGoods   shouldBe journey.answers.detailsOfRejectedGoods.get
        output.inspectionDate           shouldBe journey.answers.inspectionDate.get
        output.inspectionAddress        shouldBe journey.answers.inspectionAddress.get
        output.reimbursementMethod      shouldBe journey.answers.reimbursementMethod
          .getOrElse(ReimbursementMethodAnswer.BankAccountTransfer)
        output.totalReimbursementAmount shouldBe journey.getTotalReimbursementAmount
        output.supportingEvidences      shouldBe journey.answers.supportingEvidences.get.mapValues(_.get)
        output.consigneeEoriNumber      shouldBe journey.getConsigneeEoriFromACC14.getOrElse(journey.answers.userEoriNumber)
        output.declarantEoriNumber      shouldBe journey.getDeclarantEoriFromACC14.getOrElse(journey.answers.userEoriNumber)
        output.contactDetails           shouldBe exampleContactDetails
        output.contactAddress           shouldBe exampleContactAddress
        output.bankAccountDetails       shouldBe journey.answers.bankAccountDetails
      }
    }

    "accept submission of a new MRN" in {
      forAll(IdGen.genMRN) { mrn =>
        val journey = emptyJourney.submitMovementReferenceNumber(mrn)
        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.isComplete                                    shouldBe false
        journey.isCompleteReimbursementClaims                 shouldBe false
        journey.isCompleteSupportingEvidences                 shouldBe false
      }
    }

    "accept change of the MRN" in {
      forAll(completeJourneyGen) { journey =>
        val modifiedJourney = journey.submitMovementReferenceNumber(exampleMrn)
        modifiedJourney.answers.displayDeclaration    shouldBe empty
        modifiedJourney.isComplete                    shouldBe false
        modifiedJourney.isCompleteReimbursementClaims shouldBe false
        modifiedJourney.isCompleteSupportingEvidences shouldBe false
      }
    }

    "accept submission of the same MRN" in {
      forAll(completeJourneyGen) { journey =>
        val modifiedJourney = journey
          .submitMovementReferenceNumber(journey.answers.movementReferenceNumber.get)
        modifiedJourney                               shouldBe journey
        modifiedJourney.isComplete                    shouldBe true
        modifiedJourney.isCompleteReimbursementClaims shouldBe true
        modifiedJourney.isCompleteSupportingEvidences shouldBe true
      }
    }

    "accept submission of a new ACC14 data" in {
      forAll(displayDeclarationGen) { acc14 =>
        val journey = emptyJourney
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(acc14)

        journey.answers.movementReferenceNumber.contains(exampleMrn) shouldBe true
        journey.answers.displayDeclaration.contains(acc14)           shouldBe true
        journey.isComplete                                           shouldBe false
        journey.isCompleteReimbursementClaims                        shouldBe false
        journey.isCompleteSupportingEvidences                        shouldBe false
      }
    }

    "accept change of the ACC14 data" in {
      forAll(completeJourneyGen) { journey =>
        val modifiedJourney =
          journey
            .submitDisplayDeclaration(exampleDisplayDeclaration)
        modifiedJourney.answers.displayDeclaration    shouldBe Some(exampleDisplayDeclaration)
        modifiedJourney.answers.reimbursementClaims   shouldBe None
        modifiedJourney.isComplete                    shouldBe false
        modifiedJourney.isCompleteReimbursementClaims shouldBe false
        modifiedJourney.isCompleteSupportingEvidences shouldBe true
      }
    }

    "needs declarant and consignee submission if user's eori not matching those of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(anotherExampleEori))
      val journey            =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclaration)

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe true
    }

    "does not need declarant and consignee submission if user's eori is matching that of declarant" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori, consigneeEORI = None)
      val journey            =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclaration)

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
    }

    "does not need declarant and consignee submission if user's eori is matching that of consignee" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleEori))
      val journey            =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclaration)

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
    }

    "fail building journey if user's eori is not matching those of ACC14 and separate EORIs were not provided by the user" in {
      val journeyGen = buildJourneyGen(
        acc14DeclarantMatchesUserEori = false,
        acc14ConsigneeMatchesUserEori = false,
        submitDeclarantDetails = false,
        submitConsigneeDetails = false
      )
      forAll(journeyGen) { result =>
        val journey = result.getOrElse(fail("Journey building has failed."))
        journey.isComplete shouldBe false
      }
    }

    "fail if submitted consignee EORI is not needed" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori)
      val journeyEither      =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclaration)
          .submitConsigneeEoriNumber(anotherExampleEori)

      journeyEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val journeyEither      =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclaration)
          .submitConsigneeEoriNumber(yetAnotherExampleEori)

      journeyEither shouldBe Left("submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14")
    }

    "fail if submitted declarant EORI is not needed" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = exampleEori)
      val journeyEither      =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclaration)
          .submitDeclarantEoriNumber(anotherExampleEori)

      journeyEither shouldBe Left("submitDeclarantEoriNumber.unexpected")
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildDisplayDeclaration(declarantEORI = anotherExampleEori)
      val journeyEither      =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclaration)
          .submitDeclarantEoriNumber(yetAnotherExampleEori)

      journeyEither shouldBe Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
    }

    "submit contact details" in {
      forAll(ContactDetailsGen.genMrnContactDetails) { contactDetails =>
        val journey =
          RejectedGoodsSingleJourney
            .empty(exampleEori)
            .submitContactDetails(contactDetails)

        journey.answers.contactDetails shouldBe Some(contactDetails)
      }
    }

    "change contact details" in {
      forAll(completeJourneyGen, ContactDetailsGen.genMrnContactDetails) { (journey, contactDetails) =>
        val modifiedJourney = journey.submitContactDetails(contactDetails)

        modifiedJourney.isComplete                     shouldBe true
        modifiedJourney.toOutput.map(_.contactDetails) shouldBe Right(contactDetails)
      }
    }

    "submit contact address" in {
      forAll(ContactAddressGen.genContactAddress) { contactAddress =>
        val journey = RejectedGoodsSingleJourney.empty(exampleEori).submitContactAddress(contactAddress)

        journey.answers.contactAddress shouldBe Some(contactAddress)
      }
    }

    "change contact address" in {
      forAll(completeJourneyGen, ContactAddressGen.genContactAddress) { (journey, contactAddress) =>
        val modifiedJourney = journey.submitContactAddress(contactAddress)

        modifiedJourney.isComplete                     shouldBe true
        modifiedJourney.toOutput.map(_.contactAddress) shouldBe Right(contactAddress)
      }
    }

    "submit basis of claim" in {
      forAll(Gen.oneOf(BasisOfRejectedGoodsClaim.all)) { basisOfClaim =>
        val journey = RejectedGoodsSingleJourney.empty(exampleEori).submitBasisOfClaim(basisOfClaim)
        journey.answers.basisOfClaim shouldBe Some(basisOfClaim)
      }
    }

    "change basis of claim" in {
      forAll(completeJourneyGen, Gen.oneOf(BasisOfRejectedGoodsClaim.allButSpecialCircumstances)) {
        (journey, basisOfClaim) =>
          val modifiedJourney = journey.submitBasisOfClaim(basisOfClaim)

          modifiedJourney.isComplete                   shouldBe true
          modifiedJourney.toOutput.map(_.basisOfClaim) shouldBe Right(basisOfClaim)
      }
    }

    "change basis of claim if special circumstances" in {
      forAll(completeJourneyGenWithoutSpecialCircumstances) { journey =>
        val modifiedJourney = journey.submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)

        modifiedJourney.isComplete                   shouldBe false
        modifiedJourney.toOutput.map(_.basisOfClaim) shouldBe Left(
          "basisOfClaimSpecialCircumstances must be defined when basisOfClaim value is SpecialCircumstances" :: Nil
        )
      }
    }

    "submit basis of claim special circumstances details" in {
        val journeyEither = RejectedGoodsSingleJourney.empty(exampleEori)
          .submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)
          .submitBasisOfClaimSpecialCircumstancesDetails(exampleSpecialCircumstancesDetails)

        journeyEither.isRight shouldBe true

    }

    "change basis of claim special circumstances details" in {
      forAll(completeJourneyGen) { journey =>
        val modifiedJourneyEither = journey.submitBasisOfClaimSpecialCircumstancesDetails(exampleSpecialCircumstancesDetails)

        modifiedJourneyEither.isRight shouldBe journey.needsSpecialCircumstancesBasisOfClaim
      }
    }

    "submit method of disposal" in {
      forAll(Gen.oneOf(MethodOfDisposal.all)) { methodOfDisposal =>
        val journey = RejectedGoodsSingleJourney.empty(exampleEori).submitMethodOfDisposal(methodOfDisposal)
        journey.answers.methodOfDisposal shouldBe Some(methodOfDisposal)
      }
    }

    "change method of disposal" in {
      forAll(completeJourneyGen, Gen.oneOf(MethodOfDisposal.all)) { (journey, methodOfDisposal) =>
        val modifiedJourney = journey.submitMethodOfDisposal(methodOfDisposal)

        modifiedJourney.isComplete                       shouldBe true
        modifiedJourney.toOutput.map(_.methodOfDisposal) shouldBe Right(methodOfDisposal)
      }
    }

    "submit details of rejected goods" in {
      forAll(Gen.asciiPrintableStr) { rejectedGoodsDetails =>
        val journey = RejectedGoodsSingleJourney.empty(exampleEori).submitDetailsOfRejectedGoods(rejectedGoodsDetails)
        journey.answers.detailsOfRejectedGoods shouldBe Some(rejectedGoodsDetails)
      }
    }

    "change details of rejected goods" in {
      forAll(completeJourneyGen, Gen.asciiPrintableStr) { (journey, rejectedGoodsDetails) =>
        val modifiedJourney = journey.submitDetailsOfRejectedGoods(rejectedGoodsDetails)

        modifiedJourney.isComplete                             shouldBe true
        modifiedJourney.toOutput.map(_.detailsOfRejectedGoods) shouldBe Right(rejectedGoodsDetails)
      }
    }

    "select valid tax codes for reimbursement when none yet selected" in {
      // TODO test when all selected tax codes are in the ACC14
    }

    "replace valid tax codes for reimbursement" in {
      // TODO replace previous selection with new valid selection
    }

    "select invalid tax codes for reimbursement" in {
      // TODO test when some of the selected tax codes are NOT in the ACC14
    }

    "change tax code for reimbursement with the same set" in {
      // TODO
    }

    "change tax code for reimbursement with the new valid set" in {
      // TODO test if already provided amounts has been preserved
    }

    "change tax code for reimbursement with the new invalid set" in {
      // TODO
    }

    "submit valid amount for selected tax code" in {
      // TODO
    }

    "submit valid amount for wrong tax code" in {
      // TODO
    }

    "submit invalid amount for selected tax code" in {
      // TODO
    }

    "submit invalid amount for wrong tax code" in {
      // TODO
    }

    "change to valid amount for selected tax code" in {
      // TODO
    }

    "change to invalid amount for selected tax code" in {
      // TODO
    }

    "change to valid amount for wrong tax code" in {
      // TODO
    }

    "submit inspection date" in {
      forAll(DateGen.genDate) { inspectionDate =>
        val journey = RejectedGoodsSingleJourney.empty(exampleEori).submitInspectionDate(inspectionDate)

        journey.answers.inspectionDate shouldBe Some(inspectionDate)

      }
    }

    "change inspection date" in {
      forAll(completeJourneyGen, DateGen.genDate) { (journey, inspectionDate) =>
        val modifiedJourney = journey.submitInspectionDate(inspectionDate)

        modifiedJourney.isComplete shouldBe true
        modifiedJourney.toOutput.map(_.inspectionDate) shouldBe Right(inspectionDate)

      }
    }

    "submit inspection address" in {
      forAll(InspectionAddressGen.genInspectionAddress) { inspectionAddress =>
        val journey = RejectedGoodsSingleJourney.empty(exampleEori).submitInspectionAddress(inspectionAddress)

        journey.answers.inspectionAddress shouldBe Some(inspectionAddress)

      }
    }

    "change inspection address" in {
      forAll(completeJourneyGen, InspectionAddressGen.genInspectionAddress) { (journey, inspectionAddress) =>
        val modifiedJourney = journey.submitInspectionAddress(inspectionAddress)

        modifiedJourney.isComplete shouldBe true
        modifiedJourney.toOutput.map(_.inspectionAddress) shouldBe Right(inspectionAddress)
      }
    }

    "submit CurrentMonthAdjustment as reimbursement method when all duties are CMA eligible" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val journeyEither                    =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclarationAllCMAEligible)
          .selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00))
          .flatMap(_.submitAmountForReimbursement(TaxCode.A00, BigDecimal("1.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethodAnswer.CurrentMonthAdjustment))

      journeyEither.isRight shouldBe true
    }

    "fail submitting CurrentMonthAdjustment as reimbursement method when NOT all duties are CMA eligible" in {
      val displayDeclarationNotCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), false)))
      val journeyEither                    =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclarationNotCMAEligible)
          .selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00))
          .flatMap(_.submitAmountForReimbursement(TaxCode.A00, BigDecimal("1.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethodAnswer.CurrentMonthAdjustment))

      journeyEither shouldBe Left("submitReimbursementMethodAnswer.notCMAEligible")
    }

    "submit BankAccountTransfer as reimbursement method when all duties are CMA eligible" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val journeyEither                    =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclarationAllCMAEligible)
          .selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00))
          .flatMap(_.submitAmountForReimbursement(TaxCode.A00, BigDecimal("1.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethodAnswer.BankAccountTransfer))

      journeyEither.isRight shouldBe true
    }

    "fail submitting BankAccountTransfer as reimbursement method when NOT all duties are CMA eligible" in {
      val displayDeclarationNotCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), false)))
      val journeyEither                    =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclarationNotCMAEligible)
          .selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00))
          .flatMap(_.submitAmountForReimbursement(TaxCode.A00, BigDecimal("1.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethodAnswer.BankAccountTransfer))

      journeyEither shouldBe Left("submitReimbursementMethodAnswer.notCMAEligible")
    }

    "submit bankAccountDetails and bankAccountType if reimbursement method is BankAccountTransfer" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val journeyEither                    =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclarationAllCMAEligible)
          .selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00))
          .flatMap(_.submitAmountForReimbursement(TaxCode.A00, BigDecimal("1.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethodAnswer.BankAccountTransfer))
          .flatMap(_.submitBankAccountDetails(exampleBankAccountDetails))
          .flatMap(_.submitBankAccountType(BankAccountType.BusinessBankAccount))

      journeyEither.isRight shouldBe true
    }

    "fail submitting bankAccountDetails if not needed" in {
      val displayDeclarationAllCMAEligible =
        buildDisplayDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val journeyEither                    =
        RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclarationAllCMAEligible)
          .selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode.A00))
          .flatMap(_.submitAmountForReimbursement(TaxCode.A00, BigDecimal("1.00")))
          .flatMap(_.submitReimbursementMethod(ReimbursementMethodAnswer.CurrentMonthAdjustment))
          .flatMap(_.submitBankAccountDetails(exampleBankAccountDetails))
          .flatMap(_.submitBankAccountType(BankAccountType.BusinessBankAccount))

      journeyEither shouldBe Left("submitBankAccountDetails.unexpected")
    }

    "change bankAccountDetails in a complete journey with all duties CMA eligible" in {
      forAll(completeJourneyCMAEligibleGen) { journey =>
        val journeyEither =
          journey.submitBankAccountDetails(exampleBankAccountDetails)

        journeyEither.isRight shouldBe journey.needsBanksAccountDetailsAndTypeSubmission
      }
    }

    "change bankAccountDetails in a complete journey not eligible for CMA" in {
      forAll(completeJourneyNotCMAEligibleGen) { journey =>
        val journeyEither =
          journey.submitBankAccountDetails(exampleBankAccountDetails)

        journeyEither.isRight shouldBe true
      }
    }

  }

}
