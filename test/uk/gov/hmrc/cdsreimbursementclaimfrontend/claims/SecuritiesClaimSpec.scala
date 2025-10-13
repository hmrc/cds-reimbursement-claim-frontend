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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.claims

import org.scalacheck.Gen
import org.scalacheck.ShrinkLowPriority
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimValidationErrors.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal.exportedMethodsOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*

import scala.collection.immutable.SortedMap

class SecuritiesClaimSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers with ShrinkLowPriority {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  "SecuritiesClaim" should {
    "have an empty instance" in {
      emptyClaim.answers.userEoriNumber                                         shouldBe exampleEori
      emptyClaim.answers.bankAccountDetails                                     shouldBe None
      emptyClaim.answers.bankAccountType                                        shouldBe None
      emptyClaim.answers.additionalDetails                                      shouldBe None
      emptyClaim.answers.contactAddress                                         shouldBe None
      emptyClaim.answers.contactDetails                                         shouldBe None
      emptyClaim.answers.contactAddress                                         shouldBe None
      emptyClaim.answers.eoriNumbersVerification                                shouldBe None
      emptyClaim.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber) shouldBe None
      emptyClaim.answers.displayDeclaration                                     shouldBe None
      emptyClaim.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber) shouldBe None
      emptyClaim.answers.selectedDocumentType                                   shouldBe None
      emptyClaim.answers.supportingEvidences                                    shouldBe Seq.empty
      emptyClaim.answers.checkYourAnswersChangeMode                             shouldBe false
      emptyClaim.hasCompleteSupportingEvidences                                 shouldBe false
      emptyClaim.hasCompleteSecuritiesReclaims                                  shouldBe false
      emptyClaim.hasCompleteAnswers                                             shouldBe false
      emptyClaim.toOutput.isLeft                                                shouldBe true
      emptyClaim.isFinalized                                                    shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeClaimGen) { claim =>
        claim.answers.correctedAmounts.isDefined shouldBe true
        claim.hasCompleteSecuritiesReclaims      shouldBe true

        val output: SecuritiesClaim.Output =
          claim.toOutput.fold(e => fail(s"Cannot build output because of $e"), identity)

        output.movementReferenceNumber                       shouldBe claim.answers.movementReferenceNumber.get
        output.reasonForSecurity                             shouldBe claim.answers.reasonForSecurity.get
        output.claimantType                                  shouldBe claim.getClaimantType
        output.securitiesReclaims                            shouldBe claim.getSecuritiesReclaims
        output.supportingEvidences                           shouldBe (
          claim.answers.supportingEvidences.map(EvidenceDocument.from)
            ++ claim.answers.billOfDischargeDocuments.map(EvidenceDocument.from).toSeq
            ++ claim.answers.proofOfOriginDocuments.map(EvidenceDocument.from).toSeq
        )
        output.bankAccountDetails                            shouldBe claim.answers.bankAccountDetails
        output.additionalDetails                             shouldBe claim.answers.additionalDetails
        output.claimantInformation.eori                      shouldBe claim.answers.userEoriNumber
        output.temporaryAdmissionMethodsOfDisposal.isDefined shouldBe claim.needsMethodOfDisposalSubmission
        output.exportMovementReferenceNumber.isDefined       shouldBe claim.needsMethodOfDisposalSubmission && TemporaryAdmissionMethodOfDisposal
          .containsExportedMethodsOfDisposal(claim.answers.temporaryAdmissionMethodsOfDisposal.get)
      }
    }

    "finalize claim with caseNumber" in {
      forAll(completeClaimGen) { claim =>
        claim.hasCompleteSecuritiesReclaims  shouldBe true
        claim.hasCompleteSupportingEvidences shouldBe true
        claim.hasCompleteAnswers             shouldBe true
        claim.isFinalized                    shouldBe false
        val result        = claim.finalizeClaimWith("foo-123-abc")
        val modifiedClaim = result.getOrFail
        modifiedClaim.isFinalized                    shouldBe true
        modifiedClaim.hasCompleteSecuritiesReclaims  shouldBe true
        modifiedClaim.hasCompleteSupportingEvidences shouldBe true
        modifiedClaim.hasCompleteAnswers             shouldBe true
        modifiedClaim.finalizeClaimWith("bar")       shouldBe Left(CLAIM_ALREADY_FINALIZED)
      }
    }

    "accept submission of a new MRN" in {
      forAll { (mrn: MRN) =>
        val claim = emptyClaim
          .submitMovementReferenceNumber(mrn)
        claim.answers.movementReferenceNumber.contains(mrn) shouldBe true
        claim.hasCompleteAnswers                            shouldBe false
        claim.hasCompleteSecuritiesReclaims                 shouldBe false
        claim.hasCompleteSupportingEvidences                shouldBe false
        claim.isFinalized                                   shouldBe false
      }
    }

    "accept change of the MRN" in {
      forAll(completeClaimGen, genMRN) { (claim, mrn) =>
        val modifiedClaim = claim
          .submitMovementReferenceNumber(mrn)
        modifiedClaim.hasCompleteAnswers             shouldBe false
        modifiedClaim.hasCompleteSecuritiesReclaims  shouldBe false
        modifiedClaim.hasCompleteSupportingEvidences shouldBe false
        modifiedClaim.getLeadMovementReferenceNumber shouldBe Some(mrn)
      }
    }

    "accept submission of a new RfS and declaration if matching the MRN" in {
      forAll(mrnWithRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val claim = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .getOrFail
        claim.answers.movementReferenceNumber.contains(mrn) shouldBe true
        claim.answers.reasonForSecurity.contains(rfs)       shouldBe true
        claim.answers.displayDeclaration.contains(decl)     shouldBe true
        claim.hasCompleteAnswers                            shouldBe false
        claim.hasCompleteSupportingEvidences                shouldBe false
        claim.isFinalized                                   shouldBe false
        claim.needsExportMRNSubmission                      shouldBe (ReasonForSecurity.ntas(
          rfs
        ) && claim.answers.temporaryAdmissionMethodsOfDisposal.exists(mods =>
          mods.contains(TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment)
        ))
      }
    }

    "accept change of an existing RfS and declaration" in {
      forAll(completeClaimGen) { claim =>
        val rfs           = claim.answers.reasonForSecurity.get
        val decl          = claim.answers.displayDeclaration.get
        val modifiedClaim = claim
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .getOrFail

        (modifiedClaim eq claim) shouldBe true
      }
    }

    "accept change of the MRN when user has XI eori" in {
      forAll(completeClaimGen.map(_.submitUserXiEori(UserXiEori(exampleXIEori.value)))) { claim =>
        val rfs           = claim.answers.reasonForSecurity.get
        val decl          = claim.answers.displayDeclaration.get
        val modifiedClaim = claim
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .getOrFail

        (modifiedClaim eq claim)                      shouldBe true
        modifiedClaim.answers.eoriNumbersVerification shouldBe Some(
          EoriNumbersVerification(userXiEori = Some(UserXiEori(exampleXIEori.value)))
        )
      }
    }

    "reject submission of a new RfS and declaration when MRN has not been provided yet" in {
      forAll(rfsWithDisplayDeclarationGen) { case (rfs, decl) =>
        val claimResult = emptyClaim
          .submitReasonForSecurityAndDeclaration(rfs, decl)

        claimResult shouldBe Left("missingFirstMovementReferenceNumber")
      }
    }

    "reject submission of a new RfS and declaration when different MRN in declaration" in {
      forAll(genMRN, rfsWithDisplayDeclarationGen) { case (mrn, (rfs, decl)) =>
        val claimResult = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)

        claimResult shouldBe Left("submitReasonForSecurityAndDeclaration.wrongDisplayDeclarationMrn")
      }
    }

    "reject submission of a new RfS and declaration when different RfS in declaration" in {
      forAll(genMRN, Gen.oneOf(ReasonForSecurity.values), securitiesDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        whenever(!decl.getReasonForSecurity.contains(rfs)) {
          val declWithMRN = decl.withDeclarationId(mrn.value)
          val claimResult = emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, declWithMRN)

          claimResult shouldBe Left("submitReasonForSecurityAndDeclaration.wrongDisplayDeclarationRfS")
        }
      }
    }

    "accept change of the RfS and declaration" in {
      forAll(completeClaimGen, rfsWithDisplayDeclarationGen) { case (claim, (rfs, decl)) =>
        val declWithMRN   = decl.optionallyWithMRN(claim.getLeadMovementReferenceNumber)
        val modifiedClaim = claim
          .submitReasonForSecurityAndDeclaration(rfs, declWithMRN)
          .getOrFail
        modifiedClaim.answers.reasonForSecurity.contains(rfs)          shouldBe true
        modifiedClaim.answers.displayDeclaration.contains(declWithMRN) shouldBe true
        modifiedClaim.hasCompleteAnswers                               shouldBe false
        modifiedClaim.hasCompleteSecuritiesReclaims                    shouldBe false
        modifiedClaim.hasCompleteSupportingEvidences                   shouldBe false
        modifiedClaim.isFinalized                                      shouldBe false
      }
    }

    "accept submission of a method of disposal for a suitable reason for security" in {
      forAll(
        genMRN,
        Gen.oneOf(ReasonForSecurity.ntas),
        securitiesDisplayDeclarationGen
      ) { case (mrn, rfs, decl) =>
        val claim = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl.withReasonForSecurity(rfs).withDeclarationId(mrn.value))
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(
            _.submitTemporaryAdmissionMethodsOfDisposal(
              List(TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment)
            )
          )
          .getOrFail

        claim.answers.temporaryAdmissionMethodsOfDisposal shouldBe Some(
          List(TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment)
        )
      }
    }

    "reject submission of a valid method of disposal for a non-suitable reason for security" in {
      forAll(
        genMRN,
        Gen.oneOf(ReasonForSecurity.values -- ReasonForSecurity.ntas),
        Gen.listOf(Gen.oneOf(TemporaryAdmissionMethodOfDisposal.values)),
        securitiesDisplayDeclarationGen
      ) { case (mrn, rfs, methodsOfDisposal, decl) =>
        val claimResult = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl.withReasonForSecurity(rfs).withDeclarationId(mrn.value))
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.submitTemporaryAdmissionMethodsOfDisposal(methodsOfDisposal))

        claimResult shouldBe Left("submitTemporaryAdmissionMethodsOfDisposal.unexpected")
      }
    }

    "accept submission of a valid export MRN for a suitable reason for security and method of disposal" in {
      forAll(
        genMRN,
        Gen.oneOf(ReasonForSecurity.ntas),
        securitiesDisplayDeclarationGen,
        exportMrnTrueGen
      ) { case (mrn, rfs, decl, exportMrn) =>
        val claim = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl.withReasonForSecurity(rfs).withDeclarationId(mrn.value))
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(
            _.submitTemporaryAdmissionMethodsOfDisposal(
              List(TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment)
            )
          )
          .flatMap(_.submitExportMovementReferenceNumber(0, exportMrn))
          .getOrFail

        claim.answers.temporaryAdmissionMethodsOfDisposal shouldBe Some(
          List(TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment)
        )
        claim.answers.exportMovementReferenceNumbers      shouldBe Some(Seq(exportMrn))
      }
    }

    "reject submission of a valid export MRN for a non-suitable reason for security" in {
      forAll(
        genMRN,
        Gen.oneOf(ReasonForSecurity.values -- ReasonForSecurity.ntas),
        securitiesDisplayDeclarationGen,
        exportMrnTrueGen
      ) { case (mrn, rfs, decl, exportMrn) =>
        val claimResult = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl.withReasonForSecurity(rfs).withDeclarationId(mrn.value))
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.submitExportMovementReferenceNumber(0, exportMrn))

        claimResult shouldBe Left("submitExportMovementReferenceNumber.unexpected")
      }
    }

    "reject submission of a valid export MRN for a non-suitable method of disposal" in {
      forAll(
        genMRN,
        Gen.oneOf(ReasonForSecurity.ntas),
        Gen.listOf(
          Gen.oneOf(
            TemporaryAdmissionMethodOfDisposal.values.diff(exportedMethodsOfDisposal)
          )
        ),
        securitiesDisplayDeclarationGen,
        exportMrnTrueGen
      ) { case (mrn, rfs, methodsOfDisposal, decl, exportMrn) =>
        val claimResult = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl.withReasonForSecurity(rfs).withDeclarationId(mrn.value))
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.submitTemporaryAdmissionMethodsOfDisposal(methodsOfDisposal))
          .flatMap(_.submitExportMovementReferenceNumber(0, exportMrn))

        claimResult shouldBe Left("submitExportMovementReferenceNumber.unexpected")
      }
    }

    "accept submission of a valid selection of depositIds" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds = decl.getSecurityDepositIds.map(_.halfNonEmpty).get
        val claim      = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(depositIds))
          .getOrFail
        claim.answers.movementReferenceNumber.contains(mrn) shouldBe true
        claim.answers.reasonForSecurity.contains(rfs)       shouldBe true
        claim.answers.displayDeclaration.contains(decl)     shouldBe true
        claim.getSelectedDepositIds                           should contain theSameElementsAs depositIds
        claim.hasCompleteAnswers                            shouldBe false
        claim.hasCompleteSupportingEvidences                shouldBe false
        claim.hasCompleteSecuritiesReclaims                 shouldBe false
        claim.isFinalized                                   shouldBe false
      }
    }

    "reject submission of an empty selection of depositIds" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val claimResult = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(Seq.empty))

        claimResult shouldBe Left("selectSecurityDepositIds.emptySelection")
      }
    }

    "reject submission of a partially invalid selection of depositIds" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds  = decl.getSecurityDepositIds.map(_.halfNonEmpty).get
        val claimResult = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(depositIds :+ "invalid-deposit-id"))

        claimResult shouldBe Left("selectSecurityDepositIds.invalidSecurityDepositId")
      }
    }

    "reject submission of a completely invalid selection of depositIds" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val claimResult = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(Seq("invalid-deposit-id-1", "invalid-deposit-id-2")))

        claimResult shouldBe Left("selectSecurityDepositIds.invalidSecurityDepositId")
      }
    }

    "accept change of the depositIds selection with another valid one" in {
      forAll(completeClaimWithoutIPROrENUGen) { claim =>
        val existingDepositIds = claim.getSelectedDepositIds
        val newDepositIds      = claim.getSecurityDepositIds
          .takeExcept(existingDepositIds)

        whenever(newDepositIds.nonEmpty && !newDepositIds.iterator.sameElements(existingDepositIds)) {
          val modifiedClaim = claim
            .selectSecurityDepositIds(newDepositIds)
            .getOrFail

          modifiedClaim.hasCompleteAnswers             shouldBe false
          modifiedClaim.hasCompleteSecuritiesReclaims  shouldBe false
          modifiedClaim.hasCompleteSupportingEvidences shouldBe true
          modifiedClaim.getSelectedDepositIds            should contain theSameElementsAs newDepositIds
        }
      }
    }

    "reject change of the depositIds selection with another invalid one" in {
      forAll(completeClaimGen) { claim =>
        whenever(claim.getSecurityDepositIds.size > 1) {
          val claimResult = claim
            .selectSecurityDepositIds(Seq("invalid-deposit-id-1", "invalid-deposit-id-2"))

          claimResult shouldBe Left("selectSecurityDepositIds.invalidSecurityDepositId")
        }
      }
    }

    "accept selection of a valid depositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositId = decl.getSecurityDepositIds.map(_.head).get
        val claim     = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositId(depositId))
          .getOrFail
        claim.getSecuritySelectionStatus(depositId)         shouldBe Some(YesNo.Yes)
        (claim.getSecurityDepositIds.toSet - depositId).foreach(sid =>
          claim.getSecuritySelectionStatus(sid) shouldBe None
        )
        claim.answers.movementReferenceNumber.contains(mrn) shouldBe true
        claim.answers.reasonForSecurity.contains(rfs)       shouldBe true
        claim.answers.displayDeclaration.contains(decl)     shouldBe true
        claim.getSelectedDepositIds                           should contain theSameElementsAs Seq(depositId)
        claim.hasCompleteAnswers                            shouldBe false
        claim.hasCompleteSupportingEvidences                shouldBe false
        claim.hasCompleteSecuritiesReclaims                 shouldBe false
        claim.isFinalized                                   shouldBe false
      }
    }

    "accept removal of a valid depositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds = decl.getSecurityDepositIds.get
        val depositId  = depositIds.head
        val claim      = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(depositIds))
          .map(_.submitCheckDeclarationDetailsChangeMode(true))
          .flatMap(_.removeSecurityDepositId(depositId))
          .getOrFail

        claim.getSecuritySelectionStatus(depositId)         shouldBe Some(YesNo.No)
        (claim.getSecurityDepositIds.toSet - depositId).foreach(sid =>
          claim.getSecuritySelectionStatus(sid) shouldBe Some(YesNo.Yes)
        )
        claim.answers.movementReferenceNumber.contains(mrn) shouldBe true
        claim.answers.reasonForSecurity.contains(rfs)       shouldBe true
        claim.answers.displayDeclaration.contains(decl)     shouldBe true
        claim.getSelectedDepositIds                           should contain theSameElementsAs depositIds.tail
        claim.hasCompleteAnswers                            shouldBe false
        claim.hasCompleteSupportingEvidences                shouldBe false
        claim.hasCompleteSecuritiesReclaims                 shouldBe false
        claim.isFinalized                                   shouldBe false
      }
    }

    "reject submission of an invalid depositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val claimResult = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositId("invalid-deposit-id-1"))

        claimResult shouldBe Left("selectSecurityDepositIds.invalidSecurityDepositId")
      }
    }

    "accept submission of the valid selection of the taxCodes for a known securityDepositId" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1).distinct

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).view.mapValues(_.map { case (_, tc, _, amount) => (tc, amount) }).toSeq

        reclaimsBySecurityDepositId should not be empty

        val claim =
          emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (claim: SecuritiesClaim) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  claim
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
            )
            .getOrFail

        val expectedSecuritiesReclaims: SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]] =
          SortedMap(
            reclaims
              .groupBy(_._1)
              .view
              .view
              .mapValues(ss => SortedMap(ss.map { case (_, tc, _, _) => (tc, None) }*))
              .toSeq*
          )

        claim.answers.movementReferenceNumber.contains(mrn) shouldBe true
        claim.answers.reasonForSecurity.contains(rfs)       shouldBe true
        claim.answers.displayDeclaration.contains(decl)     shouldBe true
        claim.getSelectedDepositIds                           should contain theSameElementsAs depositIds
        claim.answers.correctedAmounts                      shouldBe Some(expectedSecuritiesReclaims)
        claim.hasCompleteAnswers                            shouldBe false
        claim.hasCompleteSupportingEvidences                shouldBe false
        claim.hasCompleteSecuritiesReclaims                 shouldBe false
        claim.isFinalized                                   shouldBe false
      }
    }

    "accept submission of the valid selection of the taxCodes for a known securityDepositId including export" in {
      forAll(mrnIncludingExportRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1).distinct
        assert(reclaims.nonEmpty)

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).view.mapValues(_.map { case (_, tc, _, amount) => (tc, amount) }).toSeq
        assert(reclaimsBySecurityDepositId.nonEmpty)
        reclaimsBySecurityDepositId should not be empty

        val claim =
          emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (claim: SecuritiesClaim) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  claim
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
            )
            .getOrFail

        val expectedSecuritiesReclaims: SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]] =
          SortedMap(
            reclaims
              .groupBy(_._1)
              .view
              .view
              .mapValues(ss => SortedMap(ss.map { case (_, tc, _, _) => (tc, None) }*))
              .toSeq*
          )

        claim.answers.movementReferenceNumber.contains(mrn) shouldBe true
        claim.answers.reasonForSecurity.contains(rfs)       shouldBe true
        claim.answers.displayDeclaration.contains(decl)     shouldBe true
        claim.getSelectedDepositIds                           should contain theSameElementsAs depositIds
        claim.answers.correctedAmounts                      shouldBe Some(expectedSecuritiesReclaims)
        claim.hasCompleteAnswers                            shouldBe false
        claim.hasCompleteSupportingEvidences                shouldBe false
        claim.hasCompleteSecuritiesReclaims                 shouldBe false
        claim.isFinalized                                   shouldBe false
      }
    }

    "reject submission of the valid selection of the taxCodes for a bogus securityDepositId" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1)
        val claimResult             = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(depositIds))
          .flatMapEach(
            reclaims.groupBy(_._1).view.mapValues(_.map { case (_, tc, _, amount) => (tc, amount) }).toSeq,
            (claim: SecuritiesClaim) =>
              (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                claim
                  .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId("bogus", args._2.map(_._1))
          )

        claimResult shouldBe Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidSecurityDepositId")
      }
    }

    "reject submission of the valid selection of the taxCodes for a not-selected securityDepositId" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.map(_.takeExcept(reclaims.map(_._1))).get

        whenever(depositIds.nonEmpty) {
          val claimResult = emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaims.groupBy(_._1).view.mapValues(_.map { case (_, tc, _, amount) => (tc, amount) }).toSeq,
              (claim: SecuritiesClaim) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  claim
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
            )

          claimResult shouldBe Left(
            "selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.securityDepositIdNotSelected"
          )
        }
      }
    }

    "reject submission of an empty selection of the taxCodes for a valid securityDepositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.map(_.halfNonEmpty).get
        val claimResult             = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(depositIds))
          .flatMap(_.selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(depositIds.head, Seq.empty))

        claimResult shouldBe Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.emptyTaxCodeSelection")
      }
    }

    "reject submission of an invalid selection of the taxCodes for a valid securityDepositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.map(_.halfNonEmpty).get
        val invalidTaxCodeSelection = TaxCodes.allExcept(decl.getSecurityTaxCodesFor(depositIds.head).toSet).headSeq
        val claimResult             = emptyClaim
          .submitMovementReferenceNumber(mrn)
          .submitReasonForSecurityAndDeclaration(rfs, decl)
          .flatMap(_.submitClaimDuplicateCheckStatus(false))
          .flatMap(_.selectSecurityDepositIds(depositIds))
          .flatMap(_.selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(depositIds.head, invalidTaxCodeSelection))

        claimResult shouldBe Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidTaxCodeSelection")
      }
    }

    "accept change of the taxCodes selection with another valid one" in {
      forAll(completeClaimWithoutIPROrENUGen) { claim =>
        val depositId: String                   = claim.getSelectedDepositIds.head
        val validTaxCodeSelection: Seq[TaxCode] = claim.getSecurityTaxCodesFor(depositId).secondHalfNonEmpty

        val modifiedClaim = claim
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(depositId, validTaxCodeSelection)
          .getOrFail

        modifiedClaim.hasCompleteAnswers             shouldBe false
        modifiedClaim.hasCompleteSecuritiesReclaims  shouldBe false
        modifiedClaim.hasCompleteSupportingEvidences shouldBe true
      }
    }

    "reject change of the taxCodes selection with invalid one" in {
      forAll(completeClaimGen) { claim =>
        val depositId: String                     = claim.getSelectedDepositIds.head
        val invalidTaxCodeSelection: Seq[TaxCode] =
          TaxCodes.allExcept(claim.getSecurityTaxCodesFor(depositId).toSet).headSeq

        val claimResult = claim
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(depositId, invalidTaxCodeSelection)

        claimResult shouldBe Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidTaxCodeSelection")
      }
    }

    "reject change of the taxCodes selection with empty one" in {
      forAll(completeClaimGen) { claim =>
        val depositId: String = claim.getSelectedDepositIds.head
        val claimResult       = claim
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(depositId, Seq.empty)

        claimResult shouldBe Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.emptyTaxCodeSelection")
      }
    }

    "reject change of the taxCodes selection with bogus securityDepositId" in {
      forAll(completeClaimGen) { claim =>
        val depositId: String                   = claim.getSelectedDepositIds.head
        val validTaxCodeSelection: Seq[TaxCode] = claim.getSecurityTaxCodesFor(depositId)

        val claimResult = claim
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId("bogus", validTaxCodeSelection)

        claimResult shouldBe Left("selectAndReplaceTaxCodeSetForSelectedSecurityDepositId.invalidSecurityDepositId")
      }
    }

    "accept submission of the valid correct amount for any valid securityDepositId and taxCode" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1).distinct

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).view.mapValues(_.map { case (_, tc, _, amount) => (tc, amount) }).toSeq

        reclaimsBySecurityDepositId should not be empty

        val claim =
          emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (claim: SecuritiesClaim) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  claim
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                    .flatMapEach(
                      args._2,
                      (claim: SecuritiesClaim) =>
                        (args2: (TaxCode, BigDecimal)) => claim.submitCorrectAmount(args._1, args2._1, args2._2)
                    )
            )
            .getOrFail

        val expectedSecuritiesReclaims: SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]] =
          SortedMap(
            reclaims
              .groupBy(_._1)
              .view
              .mapValues(ss => SortedMap(ss.map { case (_, tc, _, amount) => (tc, Some(amount)) }*))
              .toSeq*
          )

        claim.answers.movementReferenceNumber.contains(mrn) shouldBe true
        claim.answers.reasonForSecurity.contains(rfs)       shouldBe true
        claim.answers.displayDeclaration.contains(decl)     shouldBe true
        claim.getSelectedDepositIds                           should contain theSameElementsAs depositIds
        claim.answers.correctedAmounts                      shouldBe Some(expectedSecuritiesReclaims)
        claim.hasCompleteAnswers                            shouldBe false
        claim.hasCompleteSupportingEvidences                shouldBe false
        claim.hasCompleteSecuritiesReclaims                 shouldBe true
        claim.isFinalized                                   shouldBe false
        depositIds.foreach { depositId =>
          claim.isFullSecurityAmountClaimed(depositId) shouldBe false
        }
      }
    }

    "accept submission of the valid claim amount for any valid securityDepositId and taxCode" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1).distinct

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal, BigDecimal)])] =
          reclaims
            .groupBy(_._1)
            .view
            .mapValues(_.map { case (_, tc, paidAmount, amount) => (tc, paidAmount, amount) })
            .toSeq

        reclaimsBySecurityDepositId should not be empty

        val claim =
          emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (claim: SecuritiesClaim) =>
                (args: (String, Seq[(TaxCode, BigDecimal, BigDecimal)])) =>
                  claim
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                    .flatMapEach(
                      args._2,
                      (claim: SecuritiesClaim) =>
                        (args2: (TaxCode, BigDecimal, BigDecimal)) =>
                          claim.submitClaimAmount(args._1, args2._1, args2._2 - args2._3)
                    )
            )
            .getOrFail

        val expectedSecuritiesReclaims: SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]] =
          SortedMap(
            reclaims
              .groupBy(_._1)
              .view
              .mapValues(ss => SortedMap(ss.map { case (_, tc, _, amount) => (tc, Some(amount)) }*))
              .toSeq*
          )

        claim.answers.movementReferenceNumber.contains(mrn) shouldBe true
        claim.answers.reasonForSecurity.contains(rfs)       shouldBe true
        claim.answers.displayDeclaration.contains(decl)     shouldBe true
        claim.getSelectedDepositIds                           should contain theSameElementsAs depositIds
        claim.answers.correctedAmounts                      shouldBe Some(expectedSecuritiesReclaims)
        claim.hasCompleteAnswers                            shouldBe false
        claim.hasCompleteSupportingEvidences                shouldBe false
        claim.hasCompleteSecuritiesReclaims                 shouldBe true
        claim.isFinalized                                   shouldBe false
        depositIds.foreach { depositId =>
          claim.isFullSecurityAmountClaimed(depositId) shouldBe false
        }
      }
    }

    "accept submission of the full reclaim amounts" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.get.halfNonEmpty

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          depositIds.map { sid =>
            (sid, decl.getSecurityDetailsFor(sid).map(_.taxDetails.map(td => (td.getTaxCode, BigDecimal("0.00")))).get)
          }

        reclaimsBySecurityDepositId should not be empty

        val claim =
          emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (claim: SecuritiesClaim) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  claim
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                    .flatMapEach(
                      args._2,
                      (claim: SecuritiesClaim) =>
                        (args2: (TaxCode, BigDecimal)) => claim.submitCorrectAmount(args._1, args2._1, args2._2)
                    )
            )
            .getOrFail

        val expectedSecuritiesReclaims: SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]] =
          SortedMap(
            reclaimsBySecurityDepositId
              .map { case (sid, reclaims) =>
                (sid, SortedMap(reclaims.map { case (tc, a) => (tc, Some(a)) }*))
              }*
          )

        claim.answers.movementReferenceNumber.contains(mrn) shouldBe true
        claim.answers.reasonForSecurity.contains(rfs)       shouldBe true
        claim.answers.displayDeclaration.contains(decl)     shouldBe true
        claim.getSelectedDepositIds                           should contain theSameElementsAs depositIds
        claim.answers.correctedAmounts                      shouldBe Some(expectedSecuritiesReclaims)
        claim.hasCompleteAnswers                            shouldBe false
        claim.hasCompleteSupportingEvidences                shouldBe false
        claim.hasCompleteSecuritiesReclaims                 shouldBe true
        claim.isFinalized                                   shouldBe false
        depositIds.foreach { depositId =>
          claim.isFullSecurityAmountClaimed(depositId) shouldBe true
        }
      }
    }

    "reject submission of a full deposit amount for any valid securityDepositId and taxCode" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1)

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).view.mapValues(_.map { case (_, tc, _, _) => (tc, BigDecimal("0.00")) }).toSeq

        reclaimsBySecurityDepositId should not be empty

        val claimResult =
          emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (claim: SecuritiesClaim) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  claim
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                    .flatMapEach(
                      args._2,
                      (claim: SecuritiesClaim) =>
                        (args2: (TaxCode, BigDecimal)) =>
                          claim.submitCorrectAmount(
                            args._1,
                            args2._1,
                            claim.getSecurityDepositAmountFor(args._1, args2._1).get - args2._2
                          )
                    )
            )

        claimResult shouldBe Left("submitCorrectAmount.invalidAmount")
      }
    }

    "reject submission of an exceeding reclaim amount for any valid securityDepositId and taxCode" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1)

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).view.mapValues(_.map { case (_, tc, _, amount) => (tc, amount) }).toSeq

        reclaimsBySecurityDepositId should not be empty

        val claimResult =
          emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (claim: SecuritiesClaim) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  claim
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                    .flatMapEach(
                      args._2,
                      (claim: SecuritiesClaim) =>
                        (args2: (TaxCode, BigDecimal)) =>
                          claim
                            .submitCorrectAmount(
                              args._1,
                              args2._1,
                              claim.getSecurityDepositAmountFor(args._1, args2._1).get + BigDecimal("0.01")
                            )
                    )
            )

        claimResult shouldBe Left("submitCorrectAmount.invalidAmount")
      }
    }

    "reject submission of a reclaim amount if bogus securityDepositId" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1)

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).view.mapValues(_.map { case (_, tc, _, _) => (tc, BigDecimal("0.00")) }).toSeq

        reclaimsBySecurityDepositId should not be empty

        val claimResult =
          emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (claim: SecuritiesClaim) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  claim
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                    .flatMapEach(
                      args._2,
                      (claim: SecuritiesClaim) =>
                        (args2: (TaxCode, BigDecimal)) => claim.submitCorrectAmount("bogus", args2._1, args2._2)
                    )
            )

        claimResult shouldBe Left("submitCorrectAmount.invalidSecurityDepositId")
      }
    }

    "reject submission of a reclaim amount if invalid taxCode" in {
      forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
        val depositIds: Seq[String] = reclaims.map(_._1)

        val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
          reclaims.groupBy(_._1).view.mapValues(_.map { case (_, tc, _, _) => (tc, BigDecimal("0.00")) }).toSeq

        reclaimsBySecurityDepositId should not be empty

        val claimResult =
          emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              reclaimsBySecurityDepositId,
              (claim: SecuritiesClaim) =>
                (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                  claim
                    .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                    .flatMapEach(
                      args._2,
                      (claim: SecuritiesClaim) =>
                        (args2: (TaxCode, BigDecimal)) => {
                          val notSelectedTaxCodes =
                            claim
                              .getSecurityTaxCodesFor(args._1)
                              .takeExcept(claim.getSelectedDutiesFor(args._1).get)

                          if notSelectedTaxCodes.isEmpty then Left("submitCorrectAmount.invalidTaxCode")
                          else
                            claim.submitCorrectAmount(
                              args._1,
                              notSelectedTaxCodes.head,
                              args2._2
                            )
                        }
                    )
            )

        claimResult shouldBe Left("submitCorrectAmount.invalidTaxCode")
      }
    }

    "accept change of the reclaim amount with another valid one" in {
      forAll(completeClaimGen) { claim =>
        val depositId: String                     = claim.getSelectedDepositIds.head
        val taxCode: TaxCode                      = claim.getSecurityTaxCodesFor(depositId).head
        val depositAmount: BigDecimal             = claim.getSecurityDepositAmountFor(depositId, taxCode).get
        val currentAmount: BigDecimal             = claim.getClaimAmountFor(depositId, taxCode).get
        val newCorrectAmount: BigDecimal          = depositAmount / 3
        val currentTotalReclaimAmount: BigDecimal = claim.getTotalClaimAmount
        val newTotalReclaimAmount: BigDecimal     =
          currentTotalReclaimAmount - currentAmount + (depositAmount - newCorrectAmount)

        val modifiedClaim = claim
          .submitCorrectAmount(depositId, taxCode, newCorrectAmount)
          .getOrFail

        modifiedClaim.getTotalClaimAmount shouldBe newTotalReclaimAmount

        modifiedClaim.hasCompleteAnswers             shouldBe true
        modifiedClaim.hasCompleteSecuritiesReclaims  shouldBe true
        modifiedClaim.hasCompleteSupportingEvidences shouldBe true
      }
    }

    "reject change of the reclaim amount with invalid one" in {
      forAll(completeClaimGen) { claim =>
        val depositId: String          = claim.getSelectedDepositIds.head
        val taxCode: TaxCode           = claim.getSecurityTaxCodesFor(depositId).head
        val securityAmount: BigDecimal = claim.getSecurityDepositAmountFor(depositId, taxCode).get

        val claimResult = claim
          .submitCorrectAmount(depositId, taxCode, securityAmount + BigDecimal("0.01"))

        claimResult shouldBe Left("submitCorrectAmount.invalidAmount")
      }
    }

    "reject change of the reclaim amount with full deposit value" in {
      forAll(completeClaimGen) { claim =>
        val depositId: String = claim.getSelectedDepositIds.head
        val taxCode: TaxCode  = claim.getSecurityTaxCodesFor(depositId).head

        val claimResult = claim
          .submitCorrectAmount(depositId, taxCode, claim.getSecurityDepositAmountFor(depositId, taxCode).get)

        claimResult shouldBe Left("submitCorrectAmount.invalidAmount")
      }
    }

    "accept submission of the full amounts reclaim for any valid securityDepositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.get

        val claim =
          emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMapEach(
              depositIds,
              (claim: SecuritiesClaim) => claim.submitFullCorrectedAmounts(_)
            )
            .getOrFail

        val expectedCorrectedAmounts: SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]] =
          SortedMap(
            decl.displayResponseDetail.securityDetails.getOrElse(Nil).map { sd =>
              (
                sd.securityDepositId,
                SortedMap(sd.taxDetails.map(td => (td.getTaxCode, Some(ZERO)))*)
              )
            }*
          )

        claim.answers.movementReferenceNumber.contains(mrn) shouldBe true
        claim.answers.reasonForSecurity.contains(rfs)       shouldBe true
        claim.answers.displayDeclaration.contains(decl)     shouldBe true
        claim.getSelectedDepositIds                           should contain theSameElementsAs depositIds
        claim.answers.correctedAmounts                      shouldBe Some(expectedCorrectedAmounts)
        claim.hasCompleteAnswers                            shouldBe false
        claim.hasCompleteSupportingEvidences                shouldBe false
        claim.hasCompleteSecuritiesReclaims                 shouldBe true
        claim.isFinalized                                   shouldBe false
      }
    }

    "reject submission of the full amounts reclaim for any invalid securityDepositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.get
        val claimResult             =
          emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .flatMap(_.selectSecurityDepositIds(depositIds))
            .flatMap(_.submitFullCorrectedAmounts("invalid-security-deposit-id"))

        claimResult shouldBe Left("submitFullAmountForReclaim.invalidSecurityDepositId")
      }
    }

    "reject submission of the full amounts reclaim for any not selected securityDepositId" in {
      forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
        val depositIds: Seq[String] = decl.getSecurityDepositIds.get
        whenever(depositIds.size > 1) {
          val claimResult =
            emptyClaim
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositIds(depositIds.halfNonEmpty))
              .flatMap(_.submitFullCorrectedAmounts(depositIds.last))

          claimResult shouldBe Left("submitFullAmountForReclaim.securityDepositIdNotSelected")
        }
      }
    }

    "needs declarant and consignee submission if user's eori not matching those of ACC14" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.MissingLicenseQuota.acc14Code,
          declarantEORI = anotherExampleEori,
          consigneeEORI = Some(anotherExampleEori)
        )
      val claim              =
        SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.MissingLicenseQuota, displayDeclaration)
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe true
      claim.getClaimantType                          shouldBe ClaimantType.User
      claim.getClaimantEori                          shouldBe exampleEori
    }

    "needs XI eori submission if user's eori not matching those of ACC14 and ACC14 contains XI eori" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.MissingLicenseQuota.acc14Code,
          declarantEORI = anotherExampleEori,
          consigneeEORI = Some(exampleXIEori)
        )
      val claim              =
        SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.MissingLicenseQuota, displayDeclaration)
          .getOrFail

      exampleXIEori.isXiEori                         shouldBe true
      anotherExampleEori.isXiEori                    shouldBe false
      claim.userHasGBEoriMatchingDeclaration         shouldBe false
      claim.userHasXIEoriMatchingDeclaration         shouldBe false
      claim.needsUserXiEoriSubmission                shouldBe true
      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe true

      val claim2 = claim.submitUserXiEori(UserXiEori(exampleXIEori.value))

      claim2.userHasGBEoriMatchingDeclaration         shouldBe false
      claim2.userHasXIEoriMatchingDeclaration         shouldBe true
      claim2.needsUserXiEoriSubmission                shouldBe false
      claim2.needsDeclarantAndConsigneeEoriSubmission shouldBe false
    }

    "does not need declarant and consignee submission if user's eori is matching that of declarant" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.EndUseRelief.acc14Code,
          declarantEORI = exampleEori,
          consigneeEORI = Some(anotherExampleEori)
        )
      val claim              =
        SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.EndUseRelief, displayDeclaration)
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's eori is matching that of consignee" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.EndUseRelief.acc14Code,
          declarantEORI = anotherExampleEori,
          consigneeEORI = Some(exampleEori)
        )
      val claim              =
        SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.EndUseRelief, displayDeclaration)
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Consignee
      claim.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of declarant, and consignee eori is missing" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.EndUseRelief.acc14Code,
          declarantEORI = exampleXIEori,
          consigneeEORI = None
        )
      val claim              =
        SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.EndUseRelief, displayDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(java.util.Locale.ENGLISH))))
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of declarant, and consignee eori is present" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.EndUseRelief.acc14Code,
          declarantEORI = exampleXIEori,
          consigneeEORI = Some(anotherExampleEori)
        )
      val claim              =
        SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.EndUseRelief, displayDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(java.util.Locale.ENGLISH))))
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of consignee" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.EndUseRelief.acc14Code,
          declarantEORI = anotherExampleEori,
          consigneeEORI = Some(exampleXIEori)
        )
      val claim              =
        SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.EndUseRelief, displayDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(java.util.Locale.ENGLISH))))
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Consignee
      claim.getClaimantEori                          shouldBe exampleXIEori
    }

    "fail building claim if user's eori is not matching those of ACC14 and separate EORIs were not provided by the user" in {
      val claimGen = buildClaimGen(
        acc14DeclarantMatchesUserEori = false,
        acc14ConsigneeMatchesUserEori = false,
        submitDeclarantDetails = false,
        submitConsigneeDetails = false
      )
      forAll(claimGen) { result =>
        result.isLeft shouldBe true
      }
    }

    "fail if submitted consignee EORI is not needed" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.EndUseRelief.acc14Code,
          declarantEORI = exampleEori
        )
      val claimEither        =
        SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.EndUseRelief, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(anotherExampleEori))

      claimEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.EndUseRelief.acc14Code,
          declarantEORI = anotherExampleEori
        )
      val claimEither        =
        SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.EndUseRelief, displayDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
    }

    "fail if submitted declarant EORI is not needed" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.InwardProcessingRelief.acc14Code,
          declarantEORI = exampleEori
        )
      val claimEither        =
        SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.InwardProcessingRelief, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val displayDeclaration =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.ManualOverrideDeposit.acc14Code,
          declarantEORI = anotherExampleEori
        )
      val claimEither        =
        SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.ManualOverrideDeposit, displayDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
    }

    "submit contact details" in {
      forAll(ContactDetailsGen.genMrnContactDetails) { contactDetails =>
        val claim =
          SecuritiesClaim
            .empty(exampleEori)
            .submitContactDetails(Some(contactDetails))

        claim.answers.contactDetails shouldBe Some(contactDetails)
      }
    }

    "change contact details" in {
      forAll(completeClaimGen, ContactDetailsGen.genMrnContactDetails) { (claim, contactDetails) =>
        val modifiedClaim = claim.submitContactDetails(Some(contactDetails))

        modifiedClaim.hasCompleteAnswers     shouldBe true
        modifiedClaim.answers.contactDetails shouldBe Some(
          contactDetails
        )
      }
    }

    "submit contact address" in {
      forAll(ContactAddressGen.genContactAddress) { contactAddress =>
        val claim = SecuritiesClaim.empty(exampleEori).submitContactAddress(contactAddress)

        claim.answers.contactAddress shouldBe Some(contactAddress)
      }
    }

    "change contact address" in {
      forAll(completeClaimGen, ContactAddressGen.genContactAddress) { (claim, contactAddress) =>
        val modifiedClaim = claim.submitContactAddress(contactAddress)

        modifiedClaim.hasCompleteAnswers     shouldBe true
        modifiedClaim.answers.contactAddress shouldBe Some(
          contactAddress.computeChanges(claim.getInitialAddressDetailsFromDeclaration)
        )
      }
    }

    "submit bankAccountDetails and bankAccountType if expected" in {
      val displayDeclarationNotAllGuaranteeEligible =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.EndUseRelief.acc14Code,
          depositDetails = Seq("sid-001" -> Seq(TaxCode.A00 -> BigDecimal("12.34"))),
          allDutiesGuaranteeEligible = false
        )
      val claim                                     =
        SecuritiesClaim
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

      claim.answers.bankAccountDetails shouldBe Some(exampleBankAccountDetails)
    }

    "fail submitting bankAccountDetails if not needed" in {
      val displayDeclarationAllGuaranteeEligible =
        buildSecuritiesDisplayDeclaration(
          securityReason = ReasonForSecurity.EndUseRelief.acc14Code,
          depositDetails = Seq("sid-001" -> Seq(TaxCode.A00 -> BigDecimal("12.34"))),
          allDutiesGuaranteeEligible = true
        )
      val claimEither                            =
        SecuritiesClaim
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

      claimEither shouldBe Left("submitBankAccountDetails.unexpected")
    }

    "allow to change bankAccountDetails in a complete claim not guarantee eligible" in {
      forAll(buildCompleteClaimGen(allDutiesGuaranteeEligibleOpt = Some(false))) { claim =>
        val claimEither =
          claim.submitBankAccountDetails(exampleBankAccountDetails)

        claimEither.isRight shouldBe claim.needsBanksAccountDetailsSubmission
      }
    }

    "reject change of the bankAccountDetails in a complete claim guarantee eligible" in {
      forAll(
        buildCompleteClaimGen(
          allDutiesGuaranteeEligibleOpt = Some(true),
          reasonsForSecurity = ReasonForSecurity.values - ReasonForSecurity.InwardProcessingRelief
        )
      ) { claim =>
        val claimEither =
          claim.submitBankAccountDetails(exampleBankAccountDetails)

        claimEither shouldBe Left("submitBankAccountDetails.unexpected")
      }
    }

    "submit additional details" in {
      val claim = SecuritiesClaim
        .empty(exampleEori)
        .submitAdditionalDetails("foo bar")

      claim.answers.additionalDetails shouldBe Some("foo bar")
    }

    "change additional details" in {
      forAll(completeClaimGen, Gen.asciiPrintableStr) { (claim, additionalDetails) =>
        val modifiedClaim = claim.submitAdditionalDetails(additionalDetails)

        modifiedClaim.hasCompleteAnswers                                                  shouldBe true
        if additionalDetails.isBlank then modifiedClaim.toOutput.map(_.additionalDetails) shouldBe Right(None)
        else modifiedClaim.toOutput.map(_.additionalDetails)                              shouldBe Right(Some(additionalDetails))
      }
    }
  }
}
