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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import org.scalacheck.Gen
import org.scalacheck.ShrinkLowPriority
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ClaimantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.RetrievedUserTypeGen.authenticatedUserGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

import scala.collection.immutable.SortedMap

import SecuritiesJourneyGenerators._
import JourneyValidationErrors._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo

class SecuritiesJourneySpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers with ShrinkLowPriority {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  "SecuritiesJourney" should {
    "have an empty instance" in {
      emptyJourney.answers.userEoriNumber             shouldBe exampleEori
      emptyJourney.answers.bankAccountDetails         shouldBe None
      emptyJourney.answers.bankAccountType            shouldBe None
      emptyJourney.answers.contactAddress             shouldBe None
      emptyJourney.answers.contactDetails             shouldBe None
      emptyJourney.answers.contactAddress             shouldBe None
      emptyJourney.answers.declarantEoriNumber        shouldBe None
      emptyJourney.answers.displayDeclaration         shouldBe None
      emptyJourney.answers.consigneeEoriNumber        shouldBe None
      emptyJourney.answers.selectedDocumentType       shouldBe None
      emptyJourney.answers.supportingEvidences        shouldBe Seq.empty
      emptyJourney.answers.checkYourAnswersChangeMode shouldBe false
      emptyJourney.hasCompleteSupportingEvidences     shouldBe true
      emptyJourney.hasCompleteSecuritiesReclaims      shouldBe false
      emptyJourney.hasCompleteAnswers                 shouldBe false
      emptyJourney.toOutput.isLeft                    shouldBe true
      emptyJourney.isFinalized                        shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeJourneyGen) { journey =>
        journey.answers.securitiesReclaims.isDefined shouldBe true
        journey.hasCompleteSecuritiesReclaims        shouldBe true

        val output: SecuritiesJourney.Output =
          journey.toOutput.fold(e => fail(s"Cannot build output because of $e"), identity)

        output.movementReferenceNumber                      shouldBe journey.answers.movementReferenceNumber.get
        output.reasonForSecurity                            shouldBe journey.answers.reasonForSecurity.get
        output.claimantType                                 shouldBe journey.getClaimantType
        output.securitiesReclaims                           shouldBe journey.getSecuritiesReclaims
        output.supportingEvidences                          shouldBe journey.answers.supportingEvidences.map(EvidenceDocument.from)
        output.bankAccountDetails                           shouldBe journey.answers.bankAccountDetails
        output.claimantInformation.eori                     shouldBe journey.answers.userEoriNumber
        output.temporaryAdmissionMethodOfDisposal.isDefined shouldBe journey.needsMethodOfDisposalSubmission
        output.exportMovementReferenceNumber.isDefined      shouldBe journey.needsMethodOfDisposalSubmission && journey.answers.temporaryAdmissionMethodOfDisposal
          .contains(TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment)
      }
    }

    "finalize journey with caseNumber" in {
      forAll(completeJourneyGen) { journey =>
        journey.hasCompleteSecuritiesReclaims  shouldBe true
        journey.hasCompleteSupportingEvidences shouldBe true
        journey.hasCompleteAnswers             shouldBe true
        journey.isFinalized                    shouldBe false
        val result          = journey.finalizeJourneyWith("foo-123-abc")
        val modifiedJourney = result.getOrFail
        modifiedJourney.isFinalized                    shouldBe true
        modifiedJourney.hasCompleteSecuritiesReclaims  shouldBe true
        modifiedJourney.hasCompleteSupportingEvidences shouldBe true
        modifiedJourney.hasCompleteAnswers             shouldBe true
        modifiedJourney.finalizeJourneyWith("bar")     shouldBe Left(JOURNEY_ALREADY_FINALIZED)
      }
    }

    "accept submission of a new MRN" in {
      forAll { (mrn: MRN) =>
        val journey = emptyJourney
          .submitMovementReferenceNumber(mrn)
        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.hasCompleteAnswers                            shouldBe false
        journey.hasCompleteSecuritiesReclaims                 shouldBe false
        journey.hasCompleteSupportingEvidences                shouldBe true
        journey.isFinalized                                   shouldBe false
      }
    }

    "accept change of the MRN" in {
      forAll(completeJourneyGen, genMRN) { (journey, mrn) =>
        val modifiedJourney = journey
          .submitMovementReferenceNumber(mrn)
        modifiedJourney.hasCompleteAnswers             shouldBe false
        modifiedJourney.hasCompleteSecuritiesReclaims  shouldBe false
        modifiedJourney.hasCompleteSupportingEvidences shouldBe true
        modifiedJourney.getLeadMovementReferenceNumber shouldBe Some(mrn)
      }
    }

    "accept submission of a new RfS and declaration if matching the MRN" in {
      forAll(mrnWithRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val journey = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .getOrFail
        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.answers.reasonForSecurity.contains(rfs)       shouldBe true
        journey.answers.displayDeclaration.contains(decl)     shouldBe true
        journey.hasCompleteAnswers                            shouldBe false
        journey.hasCompleteSupportingEvidences                shouldBe true
        journey.isFinalized                                   shouldBe false
        journey.needsExportMRNSubmission                      shouldBe (ReasonForSecurity.temporaryAdmissions(
          rfs
        ) && journey.answers.temporaryAdmissionMethodOfDisposal.contains(
          TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment
        ))
      }
    }

    "accept change of an existing RfS and declaration" in {
      forAll(completeJourneyGen) { journey =>
        val rfs             = journey.answers.reasonForSecurity.get
        val decl            = journey.answers.displayDeclaration.get
        val modifiedJourney = journey
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .getOrFail

        (modifiedJourney eq journey) shouldBe true
      }
    }

    "reject submission of a new RfS and declaration when MRN has not been provided yet" in {
      forAll(rfsWithDisplayDeclarationGen) { case (rfs, decl) =>
        val journeyResult = emptyJourney
          .submitReasonForSecurityAndDeclaration(rfs, decl)

        journeyResult shouldBe Left("missingFirstMovementReferenceNumber")
      }
    }

    "reject submission of a new RfS and declaration when different MRN in declaration" in {
      forAll(genMRN, rfsWithDisplayDeclarationGen) { case (mrn, (rfs, decl)) =>
        val journeyResult = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)

        journeyResult shouldBe Left("submitReasonForSecurityAndDeclaration.wrongDisplayDeclarationMrn")
      }
    }

    "reject submission of a new RfS and declaration when different RfS in declaration" in {
      forAll(genMRN, Gen.oneOf(ReasonForSecurity.values), securitiesDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        whenever(!decl.getReasonForSecurity.contains(rfs)) {
          val declWithMRN   = decl.withDeclarationId(mrn.value)
          val journeyResult = emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, declWithMRN)

          journeyResult shouldBe Left("submitReasonForSecurityAndDeclaration.wrongDisplayDeclarationRfS")
        }
      }
    }

    "accept change of the RfS and declaration" in {
      forAll(completeJourneyGen, rfsWithDisplayDeclarationGen) { case (journey, (rfs, decl)) =>
        val declWithMRN     = decl.optionallyWithMRN(journey.getLeadMovementReferenceNumber)
        val modifiedJourney = journey
          .submitReasonForSecurityAndDeclaration(rfs, declWithMRN)
          .getOrFail
        modifiedJourney.answers.reasonForSecurity.contains(rfs)          shouldBe true
        modifiedJourney.answers.displayDeclaration.contains(declWithMRN) shouldBe true
        modifiedJourney.hasCompleteAnswers                               shouldBe false
        modifiedJourney.hasCompleteSecuritiesReclaims                    shouldBe false
        modifiedJourney.hasCompleteSupportingEvidences                   shouldBe true
        modifiedJourney.isFinalized                                      shouldBe false
      }
    }

    "accept submission of a method of disposal for a suitable reason for security" in {
      forAll(
        genMRN,
        Gen.oneOf(ReasonForSecurity.temporaryAdmissions),
        securitiesDisplayDeclarationGen
      ) { case (mrn, rfs, decl) =>
        val journey = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl.withReasonForSecurity(rfs).withDeclarationId(mrn.value))
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(
            _.submitTemporaryAdmissionMethodOfDisposal(TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment)
          )
          .getOrFail

        journey.answers.temporaryAdmissionMethodOfDisposal shouldBe Some(
          TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment
        )
      }
    }

    "reject submission of a valid method of disposal for a non-suitable reason for security" in {
      forAll(
        genMRN,
        Gen.oneOf(ReasonForSecurity.values -- ReasonForSecurity.temporaryAdmissions),
        Gen.oneOf(TemporaryAdmissionMethodOfDisposal.values),
        securitiesDisplayDeclarationGen
      ) { case (mrn, rfs, methodOfDisposal, decl) =>
        val journeyResult = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl.withReasonForSecurity(rfs).withDeclarationId(mrn.value))
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.submitTemporaryAdmissionMethodOfDisposal(methodOfDisposal))

        journeyResult shouldBe Left("submitTemporaryAdmissionMethodOfDisposal.unexpected")
      }
    }

    "accept submission of a valid export MRN for a suitable reason for security and method of disposal" in {
      forAll(
        genMRN,
        Gen.oneOf(ReasonForSecurity.temporaryAdmissions),
        securitiesDisplayDeclarationGen,
        exportMrnTrueGen
      ) { case (mrn, rfs, decl, exportMrn) =>
        val journey = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl.withReasonForSecurity(rfs).withDeclarationId(mrn.value))
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(
            _.submitTemporaryAdmissionMethodOfDisposal(TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment)
          )
          .flatMap(_.submitExportMovementReferenceNumber(exportMrn))
          .getOrFail

        journey.answers.temporaryAdmissionMethodOfDisposal shouldBe Some(
          TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment
        )
        journey.answers.exportMovementReferenceNumber      shouldBe Some(exportMrn)
      }
    }

    "reject submission of a valid export MRN for a non-suitable reason for security" in {
      forAll(
        genMRN,
        Gen.oneOf(ReasonForSecurity.values -- ReasonForSecurity.temporaryAdmissions),
        securitiesDisplayDeclarationGen,
        exportMrnTrueGen
      ) { case (mrn, rfs, decl, exportMrn) =>
        val journeyResult = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl.withReasonForSecurity(rfs).withDeclarationId(mrn.value))
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.submitExportMovementReferenceNumber(exportMrn))

        journeyResult shouldBe Left("submitExportMovementReferenceNumber.unexpected")
      }
    }

    "reject submission of a valid export MRN for a non-suitable method of disposal" in {
      forAll(
        genMRN,
        Gen.oneOf(ReasonForSecurity.temporaryAdmissions),
        Gen.oneOf(
          TemporaryAdmissionMethodOfDisposal.values - TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment
        ),
        securitiesDisplayDeclarationGen,
        exportMrnTrueGen
      ) { case (mrn, rfs, methodOfDisposal, decl, exportMrn) =>
        val journeyResult = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl.withReasonForSecurity(rfs).withDeclarationId(mrn.value))
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.submitTemporaryAdmissionMethodOfDisposal(methodOfDisposal))
          .flatMap(_.submitExportMovementReferenceNumber(exportMrn))

        journeyResult shouldBe Left("submitExportMovementReferenceNumber.unexpected")
      }
    }

    "accept submission of a valid selection of depositIds" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds = decl.getSecurityDepositIds.map(_.halfNonEmpty).get
        val journey    = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(depositIds))
          .getOrFail
        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.answers.reasonForSecurity.contains(rfs)       shouldBe true
        journey.answers.displayDeclaration.contains(decl)     shouldBe true
        journey.getSelectedDepositIds                           should contain theSameElementsAs depositIds
        journey.hasCompleteAnswers                            shouldBe false
        journey.hasCompleteSupportingEvidences                shouldBe true
        journey.hasCompleteSecuritiesReclaims                 shouldBe false
        journey.isFinalized                                   shouldBe false
      }
    }

    "reject submission of an empty selection of depositIds" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val journeyResult = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(Seq.empty))

        journeyResult shouldBe Left("selectSecurityDepositIds.emptySelection")
      }
    }

    "reject submission of a partially invalid selection of depositIds" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds    = decl.getSecurityDepositIds.map(_.halfNonEmpty).get
        val journeyResult = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(depositIds :+ "invalid-deposit-id"))

        journeyResult shouldBe Left("selectSecurityDepositIds.invalidSecurityDepositId")
      }
    }

    "reject submission of a completely invalid selection of depositIds" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val journeyResult = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(Seq("invalid-deposit-id-1", "invalid-deposit-id-2")))

        journeyResult shouldBe Left("selectSecurityDepositIds.invalidSecurityDepositId")
      }
    }

    "accept change of the depositIds selection with another valid one" in {
      forAll(completeJourneyGen) { journey =>
        val existingDepositIds = journey.getSelectedDepositIds
        val newDepositIds      = journey.getSecurityDepositIds
          .takeExcept(existingDepositIds)

        whenever(newDepositIds.nonEmpty && !newDepositIds.sameElements(existingDepositIds)) {
          val modifiedJourney = journey
            .selectSecurityDepositIds(newDepositIds)
            .getOrFail

          modifiedJourney.hasCompleteAnswers             shouldBe false
          modifiedJourney.hasCompleteSecuritiesReclaims  shouldBe false
          modifiedJourney.hasCompleteSupportingEvidences shouldBe true
          modifiedJourney.getSelectedDepositIds            should contain theSameElementsAs newDepositIds
        }
      }
    }

    "reject change of the depositIds selection with another invalid one" in {
      forAll(completeJourneyGen) { journey =>
        whenever(journey.getSecurityDepositIds.size > 1) {
          val journeyResult = journey
            .selectSecurityDepositIds(Seq("invalid-deposit-id-1", "invalid-deposit-id-2"))

          journeyResult shouldBe Left("selectSecurityDepositIds.invalidSecurityDepositId")
        }
      }
    }

    "accept selection of a valid depositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositId = decl.getSecurityDepositIds.map(_.head).get
        val journey   = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositId(depositId))
          .getOrFail
        journey.getSecuritySelectionStatus(depositId)         shouldBe Some(YesNo.Yes)
        (journey.getSecurityDepositIds.toSet - depositId).foreach(sid =>
          journey.getSecuritySelectionStatus(sid) shouldBe None
        )
        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.answers.reasonForSecurity.contains(rfs)       shouldBe true
        journey.answers.displayDeclaration.contains(decl)     shouldBe true
        journey.getSelectedDepositIds                           should contain theSameElementsAs Seq(depositId)
        journey.hasCompleteAnswers                            shouldBe false
        journey.hasCompleteSupportingEvidences                shouldBe true
        journey.hasCompleteSecuritiesReclaims                 shouldBe false
        journey.isFinalized                                   shouldBe false
      }
    }

    "accept removal of a valid depositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds = decl.getSecurityDepositIds.get
        val depositId  = depositIds.head
        val journey    = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(depositIds))
          .map(_.submitCheckDeclarationDetailsChangeMode(true))
          .flatMap(_.removeSecurityDepositId(depositId))
          .getOrFail

        journey.getSecuritySelectionStatus(depositId)         shouldBe Some(YesNo.No)
        (journey.getSecurityDepositIds.toSet - depositId).foreach(sid =>
          journey.getSecuritySelectionStatus(sid) shouldBe Some(YesNo.Yes)
        )
        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.answers.reasonForSecurity.contains(rfs)       shouldBe true
        journey.answers.displayDeclaration.contains(decl)     shouldBe true
        journey.getSelectedDepositIds                           should contain theSameElementsAs depositIds.tail
        journey.hasCompleteAnswers                            shouldBe false
        journey.hasCompleteSupportingEvidences                shouldBe true
        journey.hasCompleteSecuritiesReclaims                 shouldBe false
        journey.isFinalized                                   shouldBe false
      }
    }

    "reject submission of an invalid depositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val journeyResult = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositId("invalid-deposit-id-1"))

        journeyResult shouldBe Left("selectSecurityDepositIds.invalidSecurityDepositId")
      }
    }

    "accept submission of the valid selection of the taxCodes for a known securityDepositId" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1).distinct

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, amount) => (tc, amount) }).toSeq

        reclaimsBySecurityDepositId should not be empty

        val journey =
          emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (journey: SecuritiesJourney) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  journey
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
            )
            .getOrFail

        val expectedSecuritiesReclaims: SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]] =
          SortedMap(
            reclaims.groupBy(_._1).mapValues(ss => SortedMap(ss.map { case (_, tc, _) => (tc, None) }: _*)).toSeq: _*
          )

        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.answers.reasonForSecurity.contains(rfs)       shouldBe true
        journey.answers.displayDeclaration.contains(decl)     shouldBe true
        journey.getSelectedDepositIds                           should contain theSameElementsAs depositIds
        journey.answers.securitiesReclaims                    shouldBe Some(expectedSecuritiesReclaims)
        journey.hasCompleteAnswers                            shouldBe false
        journey.hasCompleteSupportingEvidences                shouldBe true
        journey.hasCompleteSecuritiesReclaims                 shouldBe false
        journey.isFinalized                                   shouldBe false
      }
    }

    "accept submission of the valid selection of the taxCodes for a known securityDepositId including export" in {
      forAll(mrnIncludingExportRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1).distinct
        assert(reclaims.nonEmpty)

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, amount) => (tc, amount) }).toSeq
        assert(reclaimsBySecurityDepositId.nonEmpty)
        reclaimsBySecurityDepositId should not be empty

        val journey =
          emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (journey: SecuritiesJourney) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  journey
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
            )
            .getOrFail

        val expectedSecuritiesReclaims: SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]] =
          SortedMap(
            reclaims.groupBy(_._1).mapValues(ss => SortedMap(ss.map { case (_, tc, _) => (tc, None) }: _*)).toSeq: _*
          )

        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.answers.reasonForSecurity.contains(rfs)       shouldBe true
        journey.answers.displayDeclaration.contains(decl)     shouldBe true
        journey.getSelectedDepositIds                           should contain theSameElementsAs depositIds
        journey.answers.securitiesReclaims                    shouldBe Some(expectedSecuritiesReclaims)
        journey.hasCompleteAnswers                            shouldBe false
        journey.hasCompleteSupportingEvidences                shouldBe true
        journey.hasCompleteSecuritiesReclaims                 shouldBe false
        journey.isFinalized                                   shouldBe false
      }
    }

    "reject submission of the valid selection of the taxCodes for a bogus securityDepositId" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1)
        val journeyResult           = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(depositIds))
          .flatMapEach(
            reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, amount) => (tc, amount) }).toSeq,
            (journey: SecuritiesJourney) =>
              (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                journey
                  .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId("bogus", args._2.map(_._1))
          )

        journeyResult shouldBe Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidSecurityDepositId")
      }
    }

    "reject submission of the valid selection of the taxCodes for a not-selected securityDepositId" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.map(_.takeExcept(reclaims.map(_._1))).get

        whenever(depositIds.nonEmpty) {
          val journeyResult = emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, amount) => (tc, amount) }).toSeq,
              (journey: SecuritiesJourney) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  journey
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
            )

          journeyResult shouldBe Left(
            "selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.securityDepositIdNotSelected"
          )
        }
      }
    }

    "reject submission of an empty selection of the taxCodes for a valid securityDepositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.map(_.halfNonEmpty).get
        val journeyResult           = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(depositIds))
          .flatMap(_.selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(depositIds.head, Seq.empty))

        journeyResult shouldBe Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.emptyTaxCodeSelection")
      }
    }

    "reject submission of an invalid selection of the taxCodes for a valid securityDepositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.map(_.halfNonEmpty).get
        val invalidTaxCodeSelection = TaxCodes.allExcept(decl.getSecurityTaxCodesFor(depositIds.head).toSet).headSeq
        val journeyResult           = emptyJourney
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(depositIds))
          .flatMap(_.selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(depositIds.head, invalidTaxCodeSelection))

        journeyResult shouldBe Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidTaxCodeSelection")
      }
    }

    "accept change of the taxCodes selection with another valid one" in {
      forAll(completeJourneyGen) { journey =>
        val depositId: String                   = journey.getSelectedDepositIds.head
        val validTaxCodeSelection: Seq[TaxCode] = journey.getSecurityTaxCodesFor(depositId).secondHalfNonEmpty

        val modifiedJourney = journey
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(depositId, validTaxCodeSelection)
          .getOrFail

        modifiedJourney.hasCompleteAnswers             shouldBe false
        modifiedJourney.hasCompleteSecuritiesReclaims  shouldBe false
        modifiedJourney.hasCompleteSupportingEvidences shouldBe true
      }
    }

    "reject change of the taxCodes selection with invalid one" in {
      forAll(completeJourneyGen) { journey =>
        val depositId: String                     = journey.getSelectedDepositIds.head
        val invalidTaxCodeSelection: Seq[TaxCode] =
          TaxCodes.allExcept(journey.getSecurityTaxCodesFor(depositId).toSet).headSeq

        val journeyResult = journey
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(depositId, invalidTaxCodeSelection)

        journeyResult shouldBe Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidTaxCodeSelection")
      }
    }

    "reject change of the taxCodes selection with empty one" in {
      forAll(completeJourneyGen) { journey =>
        val depositId: String = journey.getSelectedDepositIds.head
        val journeyResult     = journey
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(depositId, Seq.empty)

        journeyResult shouldBe Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.emptyTaxCodeSelection")
      }
    }

    "reject change of the taxCodes selection with bogus securityDepositId" in {
      forAll(completeJourneyGen) { journey =>
        val depositId: String                   = journey.getSelectedDepositIds.head
        val validTaxCodeSelection: Seq[TaxCode] = journey.getSecurityTaxCodesFor(depositId)

        val journeyResult = journey
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId("bogus", validTaxCodeSelection)

        journeyResult shouldBe Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidSecurityDepositId")
      }
    }

    "accept submission of the valid reclaim amount for any valid securityDepositId and taxCode" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1).distinct

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, amount) => (tc, amount) }).toSeq

        reclaimsBySecurityDepositId should not be empty

        val journey =
          emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (journey: SecuritiesJourney) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  journey
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                    .flatMapEach(
                      args._2,
                      (journey: SecuritiesJourney) =>
                        (args2: (TaxCode, BigDecimal)) => journey.submitAmountForReclaim(args._1, args2._1, args2._2)
                    )
            )
            .getOrFail

        val expectedSecuritiesReclaims: SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]] =
          SortedMap(
            reclaims
              .groupBy(_._1)
              .mapValues(ss => SortedMap(ss.map { case (_, tc, amount) => (tc, Some(amount)) }: _*))
              .toSeq: _*
          )

        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.answers.reasonForSecurity.contains(rfs)       shouldBe true
        journey.answers.displayDeclaration.contains(decl)     shouldBe true
        journey.getSelectedDepositIds                           should contain theSameElementsAs depositIds
        journey.answers.securitiesReclaims                    shouldBe Some(expectedSecuritiesReclaims)
        journey.hasCompleteAnswers                            shouldBe false
        journey.hasCompleteSupportingEvidences                shouldBe true
        journey.hasCompleteSecuritiesReclaims                 shouldBe true
        journey.isFinalized                                   shouldBe false
        depositIds.foreach { depositId =>
          journey.isFullSecurityAmountClaimed(depositId) shouldBe false
        }
      }
    }

    "accept submission of the full reclaim amounts" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.get.halfNonEmpty

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          depositIds.map { sid =>
            (sid, decl.getSecurityDetailsFor(sid).map(_.taxDetails.map(td => (td.getTaxCode, td.getAmount))).get)
          }

        reclaimsBySecurityDepositId should not be empty

        val journey =
          emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (journey: SecuritiesJourney) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  journey
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                    .flatMapEach(
                      args._2,
                      (journey: SecuritiesJourney) =>
                        (args2: (TaxCode, BigDecimal)) => journey.submitAmountForReclaim(args._1, args2._1, args2._2)
                    )
            )
            .getOrFail

        val expectedSecuritiesReclaims: SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]] =
          SortedMap(
            (reclaimsBySecurityDepositId
              .map { case (sid, reclaims) =>
                (sid, SortedMap((reclaims.map { case (tc, a) => (tc, Some(a)) }): _*))
              }): _*
          )

        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.answers.reasonForSecurity.contains(rfs)       shouldBe true
        journey.answers.displayDeclaration.contains(decl)     shouldBe true
        journey.getSelectedDepositIds                           should contain theSameElementsAs depositIds
        journey.answers.securitiesReclaims                    shouldBe Some(expectedSecuritiesReclaims)
        journey.hasCompleteAnswers                            shouldBe false
        journey.hasCompleteSupportingEvidences                shouldBe true
        journey.hasCompleteSecuritiesReclaims                 shouldBe true
        journey.isFinalized                                   shouldBe false
        depositIds.foreach { depositId =>
          journey.isFullSecurityAmountClaimed(depositId) shouldBe true
        }
      }
    }

    "reject submission of a zero reclaim amount for any valid securityDepositId and taxCode" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1)

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, _) => (tc, BigDecimal("0.00")) }).toSeq

        reclaimsBySecurityDepositId should not be empty

        val journeyResult =
          emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (journey: SecuritiesJourney) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  journey
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                    .flatMapEach(
                      args._2,
                      (journey: SecuritiesJourney) =>
                        (args2: (TaxCode, BigDecimal)) => journey.submitAmountForReclaim(args._1, args2._1, args2._2)
                    )
            )

        journeyResult shouldBe Left("submitAmountForReclaim.invalidAmount")
      }
    }

    "reject submission of an exceeding reclaim amount for any valid securityDepositId and taxCode" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1)

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, amount) => (tc, amount) }).toSeq

        reclaimsBySecurityDepositId should not be empty

        val journeyResult =
          emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (journey: SecuritiesJourney) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  journey
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                    .flatMapEach(
                      args._2,
                      (journey: SecuritiesJourney) =>
                        (args2: (TaxCode, BigDecimal)) =>
                          journey
                            .submitAmountForReclaim(
                              args._1,
                              args2._1,
                              journey.getSecurityDepositAmountFor(args._1, args2._1).get + BigDecimal("0.01")
                            )
                    )
            )

        journeyResult shouldBe Left("submitAmountForReclaim.invalidAmount")
      }
    }

    "reject submission of a reclaim amount if bogus securityDepositId" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1)

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, _) => (tc, BigDecimal("0.00")) }).toSeq

        reclaimsBySecurityDepositId should not be empty

        val journeyResult =
          emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (journey: SecuritiesJourney) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  journey
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                    .flatMapEach(
                      args._2,
                      (journey: SecuritiesJourney) =>
                        (args2: (TaxCode, BigDecimal)) => journey.submitAmountForReclaim("bogus", args2._1, args2._2)
                    )
            )

        journeyResult shouldBe Left("submitAmountForReclaim.invalidSecurityDepositId")
      }
    }

    "reject submission of a reclaim amount if invalid taxCode" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1)

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, _) => (tc, BigDecimal("0.00")) }).toSeq

        reclaimsBySecurityDepositId should not be empty

        val journeyResult =
          emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (journey: SecuritiesJourney) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  journey
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                    .flatMapEach(
                      args._2,
                      (journey: SecuritiesJourney) =>
                        (args2: (TaxCode, BigDecimal)) =>
                          journey.submitAmountForReclaim(
                            args._1,
                            journey
                              .getSecurityTaxCodesFor(args._1)
                              .takeExcept(journey.getSelectedDutiesFor(args._1).get)
                              .head,
                            args2._2
                          )
                    )
            )

        journeyResult shouldBe Left("submitAmountForReclaim.invalidTaxCode")
      }
    }

    "accept change of the reclaim amount with another valid one" in {
      forAll(completeJourneyGen) { journey =>
        val depositId: String                     = journey.getSelectedDepositIds.head
        val taxCode: TaxCode                      = journey.getSecurityTaxCodesFor(depositId).head
        val currentAmount: BigDecimal             = journey.getReclaimAmountFor(depositId, taxCode).get
        val newAmount: BigDecimal                 = currentAmount / 2
        val currentTotalReclaimAmount: BigDecimal = journey.getTotalReclaimAmount
        val newTotalReclaimAmount: BigDecimal     = currentTotalReclaimAmount - currentAmount + newAmount

        val modifiedJourney = journey
          .submitAmountForReclaim(depositId, taxCode, newAmount)
          .getOrFail

        modifiedJourney.getTotalReclaimAmount shouldBe newTotalReclaimAmount

        modifiedJourney.hasCompleteAnswers             shouldBe true
        modifiedJourney.hasCompleteSecuritiesReclaims  shouldBe true
        modifiedJourney.hasCompleteSupportingEvidences shouldBe true
      }
    }

    "reject change of the reclaim amount with invalid one" in {
      forAll(completeJourneyGen) { journey =>
        val depositId: String          = journey.getSelectedDepositIds.head
        val taxCode: TaxCode           = journey.getSecurityTaxCodesFor(depositId).head
        val securityAmount: BigDecimal = journey.getSecurityDepositAmountFor(depositId, taxCode).get

        val journeyResult = journey
          .submitAmountForReclaim(depositId, taxCode, securityAmount + BigDecimal("0.01"))

        journeyResult shouldBe Left("submitAmountForReclaim.invalidAmount")
      }
    }

    "reject change of the reclaim amount with zero" in {
      forAll(completeJourneyGen) { journey =>
        val depositId: String = journey.getSelectedDepositIds.head
        val taxCode: TaxCode  = journey.getSecurityTaxCodesFor(depositId).head

        val journeyResult = journey
          .submitAmountForReclaim(depositId, taxCode, BigDecimal("0.00"))

        journeyResult shouldBe Left("submitAmountForReclaim.invalidAmount")
      }
    }

    "accept submission of the full amounts reclaim for any valid securityDepositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.get

        val journey =
          emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              depositIds,
              (journey: SecuritiesJourney) => journey.submitFullAmountsForReclaim(_)
            )
            .getOrFail

        val expectedSecuritiesReclaims: SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]] =
          SortedMap(
            decl.displayResponseDetail.securityDetails.getOrElse(Nil).map { sd =>
              (
                sd.securityDepositId,
                SortedMap(sd.taxDetails.map(td => (td.getTaxCode, Some(td.getAmount))): _*)
              )
            }: _*
          )

        journey.answers.movementReferenceNumber.contains(mrn) shouldBe true
        journey.answers.reasonForSecurity.contains(rfs)       shouldBe true
        journey.answers.displayDeclaration.contains(decl)     shouldBe true
        journey.getSelectedDepositIds                           should contain theSameElementsAs depositIds
        journey.answers.securitiesReclaims                    shouldBe Some(expectedSecuritiesReclaims)
        journey.hasCompleteAnswers                            shouldBe false
        journey.hasCompleteSupportingEvidences                shouldBe true
        journey.hasCompleteSecuritiesReclaims                 shouldBe true
        journey.isFinalized                                   shouldBe false
      }
    }

    "reject submission of the full amounts reclaim for any invalid securityDepositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.get
        val journeyResult           =
          emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMap(_.submitFullAmountsForReclaim("invalid-security-deposit-id"))

        journeyResult shouldBe Left("submitFullAmountForReclaim.invalidSecurityDepositId")
      }
    }

    "reject submission of the full amounts reclaim for any not selected securityDepositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.get
        whenever(depositIds.size > 1) {
          val journeyResult =
            emptyJourney
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositIds(depositIds.halfNonEmpty))
              .flatMap(_.submitFullAmountsForReclaim(depositIds.last))

          journeyResult shouldBe Left("submitFullAmountForReclaim.securityDepositIdNotSelected")
        }
      }
    }

    "needs declarant and consignee submission if user's eori not matching those of ACC14" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.AccountSales.acc14Code,
          declarantEORI = anotherExampleEori,
          consigneeEORI = Some(anotherExampleEori)
        )
      val journey            =
        SecuritiesJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.AccountSales, displayDeclaration)
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe true
      journey.getClaimantType                          shouldBe ClaimantType.User
      journey.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's eori is matching that of declarant" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.CommunitySystemsOfDutyRelief.acc14Code,
          declarantEORI = exampleEori,
          consigneeEORI = Some(anotherExampleEori)
        )
      val journey            =
        SecuritiesJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.CommunitySystemsOfDutyRelief, displayDeclaration)
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      journey.getClaimantType                          shouldBe ClaimantType.Declarant
      journey.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's eori is matching that of consignee" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.CommunitySystemsOfDutyRelief.acc14Code,
          declarantEORI = anotherExampleEori,
          consigneeEORI = Some(exampleEori)
        )
      val journey            =
        SecuritiesJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.CommunitySystemsOfDutyRelief, displayDeclaration)
          .getOrFail

      journey.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      journey.getClaimantType                          shouldBe ClaimantType.Consignee
      journey.getClaimantEori                          shouldBe exampleEori
    }

    "fail building journey if user's eori is not matching those of ACC14 and separate EORIs were not provided by the user" in {
      val journeyGen = buildJourneyGen(
        acc14DeclarantMatchesUserEori = false,
        acc14ConsigneeMatchesUserEori = false,
        submitDeclarantDetails = false,
        submitConsigneeDetails = false
      )
      forAll(journeyGen) { result =>
        result.isLeft shouldBe true
      }
    }

    "fail if submitted consignee EORI is not needed" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.CommunitySystemsOfDutyRelief.acc14Code,
          declarantEORI = exampleEori
        )
      val journeyEither      =
        SecuritiesJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.CommunitySystemsOfDutyRelief, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(anotherExampleEori))

      journeyEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.EndUseRelief.acc14Code,
          declarantEORI = anotherExampleEori
        )
      val journeyEither      =
        SecuritiesJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.EndUseRelief, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))

      journeyEither shouldBe Left("submitConsigneeEoriNumber.shouldMatchConsigneeEoriFromACC14")
    }

    "fail if submitted declarant EORI is not needed" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.InwardProcessingRelief.acc14Code,
          declarantEORI = exampleEori
        )
      val journeyEither      =
        SecuritiesJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.InwardProcessingRelief, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      journeyEither shouldBe Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.ManualOverrideDeposit.acc14Code,
          declarantEORI = anotherExampleEori
        )
      val journeyEither      =
        SecuritiesJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.ManualOverrideDeposit, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(yetAnotherExampleEori))

      journeyEither shouldBe Left("submitDeclarantEoriNumber.shouldMatchDeclarantEoriFromACC14")
    }

    "get contact details" should {
      "return the specified details if they have been entered" in {
        forAll(completeJourneyGen, authenticatedUserGen) { (journey, signedInUser) =>
          whenever(journey.answers.contactDetails.isDefined) {
            val result = journey.computeContactDetails(signedInUser)
            result shouldBe journey.answers.contactDetails
          }
        }
      }

      "return the consignee details if no specific details entered and the signed in user is the consignee and consignee details are present" in {
        forAll(
          buildCompleteJourneyGen(
            acc14ConsigneeMatchesUserEori = true,
            acc14DeclarantMatchesUserEori = false,
            submitContactDetails = false
          ),
          authenticatedUserGen
        ) { (journey, signedInUser) =>
          whenever(
            journey.answers.displayDeclaration.flatMap(_.getConsigneeDetails.flatMap(_.contactDetails)).isDefined
          ) {
            val expectedContact   = journey.answers.displayDeclaration
              .flatMap(_.getConsigneeDetails.flatMap(_.contactDetails))
              .getOrElse(fail("Failed to get contact details"))
            val calculatedContact = journey.computeContactDetails(signedInUser).get
            calculatedContact.fullName                 shouldBe expectedContact.contactName.getOrElse("")
            calculatedContact.emailAddress.value       shouldBe expectedContact.emailAddress.getOrElse(
              signedInUser.email.get.value
            )
            calculatedContact.phoneNumber.map(_.value) shouldBe expectedContact.telephone
          }
        }
      }

      "return the signed in user details if no specific details entered and the signed in user is the consignee and consignee details are not present" in {
        forAll(
          buildCompleteJourneyGen(
            acc14ConsigneeMatchesUserEori = true,
            acc14DeclarantMatchesUserEori = false,
            submitConsigneeDetails = false,
            submitContactDetails = false
          ),
          authenticatedUserGen
        ) { (journey, signedInUser) =>
          whenever(
            journey.answers.displayDeclaration.flatMap(_.getDeclarantDetails.contactDetails).isDefined &&
              journey.answers.displayDeclaration.flatMap(_.getConsigneeDetails.flatMap(_.contactDetails)).isEmpty
          ) {
            val calculatedContact = journey.computeContactDetails(signedInUser).get
            calculatedContact.fullName           shouldBe signedInUser.name
              .map(_.toFullName)
              .getOrElse(fail("No signed in user name present"))
            calculatedContact.emailAddress.value shouldBe signedInUser.email
              .map(_.value)
              .getOrElse(fail("No signed in user email present"))
            calculatedContact.phoneNumber        shouldBe None
          }
        }
      }

      "return the declarant details if no specific details entered and the signed in user is the declarant" in {
        forAll(
          buildCompleteJourneyGen(submitContactDetails = false),
          authenticatedUserGen
        ) { (journey, signedInUser) =>
          whenever(
            journey.answers.displayDeclaration.flatMap(_.getDeclarantDetails.contactDetails).isDefined
          ) {
            val expectedContact   = journey.answers.displayDeclaration
              .flatMap(_.getDeclarantDetails.contactDetails)
              .getOrElse(fail("Failed to get contact details"))
            val calculatedContact = journey.computeContactDetails(signedInUser).get
            calculatedContact.fullName                 shouldBe expectedContact.contactName.getOrElse("")
            calculatedContact.emailAddress.value       shouldBe expectedContact.emailAddress.getOrElse(
              signedInUser.email.get.value
            )
            calculatedContact.phoneNumber.map(_.value) shouldBe expectedContact.telephone
          }
        }
      }

      "return the declarant details if no specific details entered and the signed in user is neither the consignee or declarant" in {
        forAll(
          buildCompleteJourneyGen(
            acc14ConsigneeMatchesUserEori = false,
            acc14DeclarantMatchesUserEori = false,
            submitConsigneeDetails = true,
            submitContactDetails = false
          ),
          authenticatedUserGen
        ) { (journey, signedInUser) =>
          whenever(
            journey.answers.displayDeclaration.flatMap(_.getDeclarantDetails.contactDetails).isDefined
          ) {
            val expectedContact   = journey.answers.displayDeclaration
              .flatMap(_.getDeclarantDetails.contactDetails)
              .getOrElse(fail("Failed to get contact details"))
            val calculatedContact = journey.computeContactDetails(signedInUser).get
            calculatedContact.fullName                 shouldBe expectedContact.contactName.getOrElse("")
            calculatedContact.emailAddress.value       shouldBe expectedContact.emailAddress.getOrElse(
              signedInUser.email.get.value
            )
            calculatedContact.phoneNumber.map(_.value) shouldBe expectedContact.telephone
          }
        }
      }
    }

    "get contact address" should {
      "return the specified details if they have been entered" in {
        forAll(completeJourneyGen) { journey =>
          whenever(journey.answers.contactAddress.isDefined) {
            journey.computeAddressDetails shouldBe journey.answers.contactAddress
          }
        }
      }

      "return the consignee address if no specific address entered and the signed in user is the consignee and consignee address is present" in {
        forAll(
          buildCompleteJourneyGen(
            submitContactAddress = false,
            acc14ConsigneeMatchesUserEori = true,
            acc14DeclarantMatchesUserEori = false
          )
        ) { journey =>
          val expectedAddress = journey.answers.displayDeclaration.flatMap(
            _.getConsigneeDetails.map(_.establishmentAddress.toContactAddress)
          )
          journey.computeAddressDetails shouldBe expectedAddress
        }
      }

      "return the declarant address if no specific address entered and the signed in user is the consignee and consignee address is not present" in {
        forAll(
          buildCompleteJourneyGen(
            submitContactAddress = false,
            acc14ConsigneeMatchesUserEori = true,
            submitConsigneeDetails = false
          )
        ) { journey =>
          val expectedAddress =
            journey.answers.displayDeclaration.map(_.getDeclarantDetails.establishmentAddress.toContactAddress)
          journey.computeAddressDetails shouldBe expectedAddress
        }
      }

      "return the declarant address if no specific address entered and the signed in user is the declarant" in {
        forAll(
          buildCompleteJourneyGen(
            submitContactAddress = false,
            acc14ConsigneeMatchesUserEori = false,
            acc14DeclarantMatchesUserEori = true
          )
        ) { journey =>
          val expectedAddress =
            journey.answers.displayDeclaration.map(_.getDeclarantDetails.establishmentAddress.toContactAddress)
          journey.computeAddressDetails shouldBe expectedAddress
        }
      }

      "return the declarant address if no specific address entered and the signed in user is neither the declarant or the consignee and declarant address is present" in {
        forAll(
          buildCompleteJourneyGen(
            submitContactAddress = false,
            acc14DeclarantMatchesUserEori = false
          )
        ) { journey =>
          val expectedAddress =
            journey.answers.displayDeclaration.map(_.getDeclarantDetails.establishmentAddress.toContactAddress)
          journey.computeAddressDetails shouldBe expectedAddress
        }
      }
    }

    "submit contact details" in {
      forAll(ContactDetailsGen.genMrnContactDetails) { contactDetails =>
        val journey =
          SecuritiesJourney
            .empty(exampleEori)
            .submitContactDetails(Some(contactDetails))

        journey.answers.contactDetails shouldBe Some(contactDetails)
      }
    }

    "change contact details" in {
      forAll(completeJourneyGen, ContactDetailsGen.genMrnContactDetails) { (journey, contactDetails) =>
        val modifiedJourney = journey.submitContactDetails(Some(contactDetails))

        modifiedJourney.hasCompleteAnswers     shouldBe true
        modifiedJourney.answers.contactDetails shouldBe Some(contactDetails)
      }
    }

    "submit contact address" in {
      forAll(ContactAddressGen.genContactAddress) { contactAddress =>
        val journey = SecuritiesJourney.empty(exampleEori).submitContactAddress(contactAddress)

        journey.answers.contactAddress shouldBe Some(contactAddress)
      }
    }

    "change contact address" in {
      forAll(completeJourneyGen, ContactAddressGen.genContactAddress) { (journey, contactAddress) =>
        val modifiedJourney = journey.submitContactAddress(contactAddress)

        modifiedJourney.hasCompleteAnswers     shouldBe true
        modifiedJourney.answers.contactAddress shouldBe Some(contactAddress)
      }
    }

    "submit bankAccountDetails and bankAccountType if expected" in {
      val displayDeclarationNotAllGuaranteeEligible =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.EndUseRelief.acc14Code,
          reclaimsDetails = Seq("sid-001" -> Seq(TaxCode.A00 -> BigDecimal("12.34"))),
          allDutiesGuaranteeEligible = false
        )
      val journey                                   =
        SecuritiesJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(
            ReasonForSecurity.EndUseRelief,
            displayDeclarationNotAllGuaranteeEligible
          )
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(Seq("sid-001")))
          .flatMap(_.submitBankAccountDetails(exampleBankAccountDetails))
          .flatMap(_.submitBankAccountType(BankAccountType.Business))
          .getOrFail

      journey.answers.bankAccountDetails shouldBe Some(exampleBankAccountDetails)
    }

    "fail submitting bankAccountDetails if not needed" in {
      val displayDeclarationAllGuaranteeEligible =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.EndUseRelief.acc14Code,
          reclaimsDetails = Seq("sid-001" -> Seq(TaxCode.A00 -> BigDecimal("12.34"))),
          allDutiesGuaranteeEligible = true
        )
      val journeyEither                          =
        SecuritiesJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(
            ReasonForSecurity.EndUseRelief,
            displayDeclarationAllGuaranteeEligible
          )
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(Seq("sid-001")))
          .flatMap(_.submitBankAccountDetails(exampleBankAccountDetails))
          .flatMap(_.submitBankAccountType(BankAccountType.Business))

      journeyEither shouldBe Left("submitBankAccountDetails.unexpected")
    }

    "allow to change bankAccountDetails in a complete journey not guarantee eligible" in {
      forAll(buildCompleteJourneyGen(allDutiesGuaranteeEligibleOpt = Some(false))) { journey =>
        val journeyEither =
          journey.submitBankAccountDetails(exampleBankAccountDetails)

        journeyEither.isRight shouldBe journey.needsBanksAccountDetailsSubmission
      }
    }

    "reject change of the bankAccountDetails in a complete journey guarantee eligible" in {
      forAll(buildCompleteJourneyGen(allDutiesGuaranteeEligibleOpt = Some(true))) { journey =>
        val journeyEither =
          journey.submitBankAccountDetails(exampleBankAccountDetails)

        journeyEither shouldBe Left("submitBankAccountDetails.unexpected")
      }
    }

  }
}
