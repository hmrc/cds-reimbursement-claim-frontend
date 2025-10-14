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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator
import org.scalacheck.Gen
import org.scalacheck.ShrinkLowPriority
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimValidationErrors.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*

class RejectedGoodsMultipleClaimSpec
    extends AnyWordSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with ShrinkLowPriority {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  val MRNS_SIZE: Int = 5

  "RejectedGoodsMultipleClaim" should {
    "have an empty instance" in {
      emptyClaim.answers.userEoriNumber                                         shouldBe exampleEori
      emptyClaim.answers.bankAccountDetails                                     shouldBe None
      emptyClaim.answers.bankAccountType                                        shouldBe None
      emptyClaim.answers.basisOfClaim                                           shouldBe None
      emptyClaim.answers.basisOfClaimSpecialCircumstances                       shouldBe None
      emptyClaim.answers.contactAddress                                         shouldBe None
      emptyClaim.answers.contactDetails                                         shouldBe None
      emptyClaim.answers.contactAddress                                         shouldBe None
      emptyClaim.answers.eoriNumbersVerification                                shouldBe None
      emptyClaim.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber) shouldBe None
      emptyClaim.answers.detailsOfRejectedGoods                                 shouldBe None
      emptyClaim.getLeadImportDeclaration                                       shouldBe None
      emptyClaim.answers.eoriNumbersVerification.flatMap(_.consigneeEoriNumber) shouldBe None
      emptyClaim.answers.inspectionAddress                                      shouldBe None
      emptyClaim.answers.inspectionDate                                         shouldBe None
      emptyClaim.answers.methodOfDisposal                                       shouldBe None
      emptyClaim.answers.correctedAmounts                                       shouldBe None
      emptyClaim.answers.selectedDocumentType                                   shouldBe None
      emptyClaim.answers.supportingEvidences                                    shouldBe Seq.empty
      emptyClaim.answers.checkYourAnswersChangeMode                             shouldBe false
      emptyClaim.getNdrcDetailsFor(exampleMrn)                                  shouldBe None
      emptyClaim.getSelectedDuties(exampleMrn)                                  shouldBe None
      emptyClaim.getTotalReimbursementAmount                                    shouldBe BigDecimal("0.00")
      emptyClaim.isAllSelectedDutiesAreCMAEligible                              shouldBe false
      emptyClaim.hasCompleteReimbursementClaims                                 shouldBe false
      emptyClaim.hasCompleteSupportingEvidences                                 shouldBe false
      emptyClaim.hasCompleteAnswers                                             shouldBe false
      emptyClaim.toOutput.isLeft                                                shouldBe true
      emptyClaim.isFinalized                                                    shouldBe false
    }

    "check completeness and produce the correct output" in {
      forAll(completeClaimGen) { claim =>
        RejectedGoodsMultipleClaim.validator.apply(claim) shouldBe Right(())
        claim.answers.checkYourAnswersChangeMode          shouldBe true
        claim.hasCompleteReimbursementClaims              shouldBe true
        claim.hasCompleteSupportingEvidences              shouldBe true
        claim.hasCompleteAnswers                          shouldBe true
        claim.isFinalized                                 shouldBe false

        val output = claim.toOutput.getOrElse(fail("Claim output not defined."))

        output.movementReferenceNumbers shouldBe claim.answers.movementReferenceNumbers.get
        output.claimantType             shouldBe claim.getClaimantType
        output.basisOfClaim             shouldBe claim.answers.basisOfClaim.get
        output.methodOfDisposal         shouldBe claim.answers.methodOfDisposal.get
        output.detailsOfRejectedGoods   shouldBe claim.answers.detailsOfRejectedGoods.get
        output.inspectionDate           shouldBe claim.answers.inspectionDate.get
        output.inspectionAddress        shouldBe claim.answers.inspectionAddress.get
        output.reimbursementMethod      shouldBe ReimbursementMethod.BankAccountTransfer
        output.reimbursementClaims      shouldBe claim.getReimbursementClaims
        output.reimbursementClaims.size shouldBe claim.countOfMovementReferenceNumbers
        output.supportingEvidences      shouldBe claim.answers.supportingEvidences.map(EvidenceDocument.from)
        output.bankAccountDetails       shouldBe claim.answers.bankAccountDetails
        output.claimantInformation.eori shouldBe claim.answers.userEoriNumber
      }
    }

    "check incompleteness if less than two MRNs" in {
      forAll(buildCompleteClaimGen(minNumberOfMRNs = 1, maxNumberOfMRNs = 1)) { claim =>
        RejectedGoodsMultipleClaim.validator(claim).headErrorOption shouldBe Some(
          MISSING_SECOND_MOVEMENT_REFERENCE_NUMBER
        )
        claim.answers.checkYourAnswersChangeMode                    shouldBe false
        claim.hasCompleteReimbursementClaims                        shouldBe true
        claim.hasCompleteSupportingEvidences                        shouldBe true
        claim.hasCompleteAnswers                                    shouldBe false
        claim.isFinalized                                           shouldBe false
      }
    }

    "finalize claim with caseNumber" in {
      forAll(completeClaimGen) { claim =>
        claim.hasCompleteReimbursementClaims shouldBe true
        claim.hasCompleteSupportingEvidences shouldBe true
        claim.hasCompleteAnswers             shouldBe true
        claim.isFinalized                    shouldBe false
        val result        = claim.finalizeClaimWith("foo-123-abc")
        val modifiedClaim = result.getOrFail
        modifiedClaim.isFinalized                    shouldBe true
        modifiedClaim.hasCompleteReimbursementClaims shouldBe true
        modifiedClaim.hasCompleteSupportingEvidences shouldBe true
        modifiedClaim.hasCompleteAnswers             shouldBe true
        modifiedClaim.finalizeClaimWith("bar")       shouldBe Left(CLAIM_ALREADY_FINALIZED)
      }
    }

    "accept submission of a first MRN and ImportDeclaration" in {
      forAll(mrnWithImportDeclarationGen) { case (mrn, importDeclaration) =>
        val claim = emptyClaim
          .submitMovementReferenceNumberAndDeclaration(mrn, importDeclaration)
          .getOrFail
        claim.getLeadMovementReferenceNumber shouldBe Some(mrn)
        claim.getLeadImportDeclaration       shouldBe Some(importDeclaration)
        claim.hasCompleteAnswers             shouldBe false
        claim.hasCompleteReimbursementClaims shouldBe false
        claim.hasCompleteSupportingEvidences shouldBe false
        claim.isFinalized                    shouldBe false
      }
    }

    "decline submission of a wrong display declaration" in {
      forAll(mrnWithImportDeclarationGen) { case (mrn, decl) =>
        val claimEither = emptyClaim
          .submitMovementReferenceNumberAndDeclaration(mrn, decl.withDeclarationId("foo"))

        claimEither shouldBe Left("submitMovementReferenceNumber.wrongImportDeclarationMrn")
      }
    }

    "accept submission of multiple MRNs and ImportDeclarations" in {
      def submitData(claim: RejectedGoodsMultipleClaim)(data: ((MRN, ImportDeclaration), Int)) =
        claim.submitMovementReferenceNumberAndDeclaration(data._2, data._1._1, data._1._2)

      forAll(listOfExactlyN(11, mrnWithImportDeclarationGen)) { data =>
        val dataWithIndex = data.zipWithIndex
        val claim         = emptyClaim
          .flatMapEach(dataWithIndex, submitData)
          .getOrFail

        claim.countOfMovementReferenceNumbers shouldBe 11
        claim.getLeadMovementReferenceNumber  shouldBe data.headOption.map(_._1)
        dataWithIndex.foreach { case ((mrn, decl), index) =>
          claim.getNthMovementReferenceNumber(index) shouldBe Some(mrn)
          claim.getImportDeclarationFor(mrn)         shouldBe Some(decl)
        }

        claim.hasCompleteAnswers             shouldBe false
        claim.hasCompleteReimbursementClaims shouldBe false
        claim.hasCompleteSupportingEvidences shouldBe false
        claim.isFinalized                    shouldBe false
      }
    }

    "decline submission of an out-of-order MRN and ImportDeclaration" in {
      forAll(mrnWithImportDeclarationGen) { case (mrn, importDeclaration) =>
        val claimEither = emptyClaim
          .submitMovementReferenceNumberAndDeclaration(1, mrn, importDeclaration)
        claimEither shouldBe Left("submitMovementReferenceNumber.invalidIndex")
      }
    }

    "accept change of the first MRN" in {
      forAll(completeClaimGen, mrnWithImportDeclarationGen) { case (claim, (mrn, importDeclaration)) =>
        val modifiedClaim =
          claim
            .submitMovementReferenceNumberAndDeclaration(0, mrn, importDeclaration)
            .getOrFail
        modifiedClaim.getLeadMovementReferenceNumber  shouldBe Some(mrn)
        modifiedClaim.countOfMovementReferenceNumbers shouldBe 1
        modifiedClaim.getLeadImportDeclaration        shouldBe Some(importDeclaration)
        modifiedClaim.hasCompleteAnswers              shouldBe false
        modifiedClaim.hasCompleteReimbursementClaims  shouldBe false
        modifiedClaim.hasCompleteSupportingEvidences  shouldBe false
        modifiedClaim.answers.correctedAmounts        shouldBe None
        modifiedClaim.answers.inspectionAddress       shouldBe None
        modifiedClaim.answers.inspectionDate          shouldBe None
        modifiedClaim.answers.methodOfDisposal        shouldBe None
        modifiedClaim.answers.correctedAmounts        shouldBe None
        modifiedClaim.answers.selectedDocumentType    shouldBe None
        modifiedClaim.answers.supportingEvidences     shouldBe Seq.empty
      }
    }

    "accept change of the first MRN when user has XI eori" in {
      forAll(
        completeClaimGen.map(_.submitUserXiEori(UserXiEori(exampleXIEori.value))),
        importDeclarationGen
      ) { (claim, decl) =>
        val decl2         = decl.withDeclarationId(exampleMrnAsString)
        val modifiedClaim = claim
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, decl2)
          .getOrFail
        modifiedClaim.getLeadImportDeclaration        shouldBe Some(decl2)
        modifiedClaim.hasCompleteAnswers              shouldBe false
        modifiedClaim.hasCompleteReimbursementClaims  shouldBe false
        modifiedClaim.hasCompleteSupportingEvidences  shouldBe false
        modifiedClaim.answers.eoriNumbersVerification shouldBe Some(
          EoriNumbersVerification(userXiEori = Some(UserXiEori(exampleXIEori.value)))
        )
      }
    }

    "accept change of the second MRN with declarantEori matching" in {
      forAll(completeClaimGen, mrnWithImportDeclarationGen) { case (claim, (mrn, importDeclaration)) =>
        val importDeclarationWithDeclarantEoriMatching =
          importDeclaration.withDeclarantEori(claim.getDeclarantEoriFromACC14.get)
        val modifiedClaim                              =
          claim
            .submitMovementReferenceNumberAndDeclaration(
              1,
              mrn,
              importDeclarationWithDeclarantEoriMatching
            )
            .getOrFail
        modifiedClaim.getNthMovementReferenceNumber(1) shouldBe Some(mrn)
        modifiedClaim.countOfMovementReferenceNumbers  shouldBe claim.countOfMovementReferenceNumbers
        modifiedClaim.getNthImportDeclaration(1)       shouldBe Some(importDeclarationWithDeclarantEoriMatching)
        modifiedClaim.hasCompleteReimbursementClaims   shouldBe false
        modifiedClaim.hasCompleteAnswers               shouldBe false
      }
    }

    "accept change of the second MRN with consigneeEori matching" in {
      forAll(completeClaimGen, mrnWithImportDeclarationGen) { case (claim, (mrn, importDeclaration)) =>
        val importDeclarationWithEoriMatching = claim.getConsigneeEoriFromACC14 match {
          case Some(eori) => importDeclaration.withConsigneeEori(eori)
          case None       => importDeclaration.withDeclarantEori(claim.getDeclarantEoriFromACC14.get)
        }
        val modifiedClaim                     =
          claim
            .submitMovementReferenceNumberAndDeclaration(
              1,
              mrn,
              importDeclarationWithEoriMatching
            )
            .getOrFail
        modifiedClaim.getNthMovementReferenceNumber(1) shouldBe Some(mrn)
        modifiedClaim.countOfMovementReferenceNumbers  shouldBe claim.countOfMovementReferenceNumbers
        modifiedClaim.getNthImportDeclaration(1)       shouldBe Some(importDeclarationWithEoriMatching)
        modifiedClaim.hasCompleteReimbursementClaims   shouldBe false
        modifiedClaim.hasCompleteAnswers               shouldBe false
      }
    }

    "accept submission of the same first MRN" in {
      forAll(completeClaimGen) { claim =>
        val modifiedClaim = claim
          .submitMovementReferenceNumberAndDeclaration(
            0,
            claim.getLeadMovementReferenceNumber.get,
            claim.getLeadImportDeclaration.get
          )
          .getOrFail
        modifiedClaim                                shouldBe claim
        modifiedClaim.hasCompleteAnswers             shouldBe true
        modifiedClaim.hasCompleteReimbursementClaims shouldBe true
        modifiedClaim.hasCompleteSupportingEvidences shouldBe true
      }
    }

    "accept submission of the same nth MRN" in {
      forAll(completeClaimGen) { claim =>
        claim.answers.movementReferenceNumbers.get.zipWithIndex.foreach { case (mrn, index) =>
          val modifiedClaim = claim
            .submitMovementReferenceNumberAndDeclaration(
              index,
              mrn,
              claim.getImportDeclarationFor(mrn).get
            )
            .getOrFail
          modifiedClaim                                shouldBe claim
          modifiedClaim.hasCompleteAnswers             shouldBe true
          modifiedClaim.hasCompleteReimbursementClaims shouldBe true
          modifiedClaim.hasCompleteSupportingEvidences shouldBe true
        }
      }
    }

    "decline removal of a first MRN" in {
      forAll(completeClaimGen) { claim =>
        val modifiedClaimEither = claim
          .removeMovementReferenceNumberAndImportDeclaration(
            claim.getLeadMovementReferenceNumber.get
          )
        modifiedClaimEither shouldBe Left("removeMovementReferenceNumberAndImportDeclaration.cannotRemoveFirstMRN")
      }
    }

    "accept removal of non-first MRN" in {
      forAll(buildCompleteClaimGen(minNumberOfMRNs = 3)) { claim =>
        claim.answers.movementReferenceNumbers.get.drop(1).foreach { mrn =>
          val modifiedClaim = claim
            .removeMovementReferenceNumberAndImportDeclaration(mrn)
            .getOrFail
          modifiedClaim                                  should not be claim
          modifiedClaim.hasCompleteAnswers             shouldBe true
          modifiedClaim.hasCompleteReimbursementClaims shouldBe true
          modifiedClaim.hasCompleteSupportingEvidences shouldBe true
        }
      }
    }

    "decline removal of second MRN if only two" in {
      forAll(buildCompleteClaimGen(minNumberOfMRNs = 2, maxNumberOfMRNs = 2)) { claim =>
        claim.answers.movementReferenceNumbers.get.drop(1).foreach { mrn =>
          claim
            .removeMovementReferenceNumberAndImportDeclaration(mrn)
            .expectFailure("removeMovementReferenceNumberAndImportDeclaration.cannotRemoveSecondMRN")
        }
      }
    }

    "decline removal of non-existent MRN" in {
      forAll(completeClaimGen, IdGen.genMRN) { (claim, mrn) =>
        val modifiedClaimEither = claim
          .removeMovementReferenceNumberAndImportDeclaration(mrn)
        modifiedClaimEither shouldBe Left("removeMovementReferenceNumberAndImportDeclaration.notFound")
      }
    }

    "decline submission of a duplicate MRN" in {
      forAll(mrnWithImportDeclarationGen) { case (mrn, decl) =>
        val acc14 = decl.withDeclarationId(mrn.value)

        val claimEither = emptyClaim
          .submitMovementReferenceNumberAndDeclaration(0, mrn, acc14)
          .flatMap(_.submitMovementReferenceNumberAndDeclaration(1, mrn, acc14))

        claimEither shouldBe Left("submitMovementReferenceNumber.movementReferenceNumberAlreadyExists")
      }
    }

    "accept submission of the same nth MRN and different declaration" in {
      forAll(completeClaimGen, importDeclarationGen) { case (claim, declaration) =>
        val declarationWithMatchingEori = declaration.withDeclarantEori(claim.getDeclarantEoriFromACC14.get)
        claim.answers.movementReferenceNumbers.get.zipWithIndex.foreach { case (mrn, index) =>
          val modifiedClaim = claim
            .submitMovementReferenceNumberAndDeclaration(
              index,
              mrn,
              declarationWithMatchingEori.withDeclarationId(mrn.value)
            )
            .getOrFail
          modifiedClaim                                  should not be claim
          modifiedClaim.hasCompleteAnswers             shouldBe false
          modifiedClaim.hasCompleteReimbursementClaims shouldBe false
        }
      }
    }

    "reject submission of the same nth MRN and different declaration if mrn not matching" in {
      forAll(completeClaimGen, importDeclarationGen) { case (claim, declaration) =>
        claim.answers.movementReferenceNumbers.get.zipWithIndex.foreach { case (mrn, index) =>
          val result = claim
            .submitMovementReferenceNumberAndDeclaration(
              index,
              mrn,
              declaration
            )
          result shouldBe Left("submitMovementReferenceNumber.wrongImportDeclarationMrn")
        }
      }
    }

    "reject submission of the same nth MRN and different declaration if eori not matching" in {
      forAll(completeClaimGen, importDeclarationGen) { case (claim, declaration) =>
        claim.answers.movementReferenceNumbers.get.zipWithIndex.drop(1).foreach { case (mrn, index) =>
          val result = claim
            .submitMovementReferenceNumberAndDeclaration(
              index,
              mrn,
              declaration.withDeclarationId(mrn.value)
            )
          result shouldBe Left("submitMovementReferenceNumber.wrongImportDeclarationEori")
        }
      }
    }

    "decline submission of an existing MRN in different position" in {
      forAll(completeClaimGen) { claim =>
        claim.answers.movementReferenceNumbers.get.zipWithIndex.foreach { case (_, index) =>
          val i                   = if index == 0 then claim.countOfMovementReferenceNumbers - 1 else index - 1
          val existingMrn         = claim.getNthMovementReferenceNumber(i).get
          val modifiedClaimEither = claim
            .submitMovementReferenceNumberAndDeclaration(
              index,
              existingMrn,
              exampleImportDeclaration
                .withDeclarationId(existingMrn.value)
                .withDeclarantEori(claim.getDeclarantEoriFromACC14.get)
            )
          modifiedClaimEither shouldBe Left("submitMovementReferenceNumber.movementReferenceNumberAlreadyExists")
        }
      }
    }

    "needs declarant and consignee submission if user's eori not matching those of ACC14" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(anotherExampleEori))
      val claim             =
        RejectedGoodsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, importDeclaration)
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe true
      claim.getClaimantType                          shouldBe ClaimantType.User
      claim.getClaimantEori                          shouldBe exampleEori
    }

    "needs XI eori submission if user's eori not matching those of ACC14 and ACC14 contains XI eori" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleXIEori))
      val claim             =
        RejectedGoodsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, importDeclaration)
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
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleEori, consigneeEORI = None)
      val claim             =
        RejectedGoodsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, importDeclaration)
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's eori is matching that of consignee" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleEori))
      val claim             =
        RejectedGoodsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, importDeclaration)
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Consignee
      claim.getClaimantEori                          shouldBe exampleEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of declarant, and consignee eori is missing" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleXIEori, consigneeEORI = None)
      val claim             =
        RejectedGoodsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, importDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(java.util.Locale.ENGLISH))))
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of declarant, and consignee eori is present" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleXIEori, consigneeEORI = Some(anotherExampleEori))
      val claim             =
        RejectedGoodsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, importDeclaration)
          .map(_.submitUserXiEori(UserXiEori(exampleXIEori.value.toLowerCase(java.util.Locale.ENGLISH))))
          .getOrFail

      claim.needsDeclarantAndConsigneeEoriSubmission shouldBe false
      claim.getClaimantType                          shouldBe ClaimantType.Declarant
      claim.getClaimantEori                          shouldBe exampleXIEori
    }

    "does not need declarant and consignee submission if user's XI eori is matching that of consignee" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori, consigneeEORI = Some(exampleXIEori))
      val claim             =
        RejectedGoodsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, importDeclaration)
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
        val claim = result.getOrElse(fail("Claim building has failed."))
        claim.hasCompleteAnswers shouldBe false
      }
    }

    "fail if submitted consignee EORI is not needed" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleEori)
      val claimEither       =
        RejectedGoodsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, importDeclaration)
          .getOrFail
          .submitConsigneeEoriNumber(anotherExampleEori)

      claimEither shouldBe Left("submitConsigneeEoriNumber.unexpected")
    }

    "fail if submitted consignee EORI is not matching that of ACC14" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori)
      val claimEither       =
        RejectedGoodsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, importDeclaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_CONSIGNEE_EORI)
    }

    "fail if submitted declarant EORI is not needed" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = exampleEori)
      val claimEither       =
        RejectedGoodsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, importDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(anotherExampleEori))

      claimEither shouldBe Left("submitDeclarantEoriNumber.unexpected")
    }

    "fail if submitted declarant EORI is not matching that of ACC14" in {
      val importDeclaration =
        buildImportDeclaration(declarantEORI = anotherExampleEori)
      val claimEither       =
        RejectedGoodsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, importDeclaration)
          .flatMap(_.submitDeclarantEoriNumber(yetAnotherExampleEori))

      claimEither shouldBe Left(ClaimValidationErrors.SHOULD_MATCH_ACC14_DECLARANT_EORI)
    }

    "submit contact details" in {
      forAll(ContactDetailsGen.genMrnContactDetails) { contactDetails =>
        val claim =
          RejectedGoodsMultipleClaim
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
        val claim = RejectedGoodsMultipleClaim.empty(exampleEori).submitContactAddress(contactAddress)

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

    "submit basis of claim" in {
      forAll(Gen.oneOf(BasisOfRejectedGoodsClaim.values)) { basisOfClaim =>
        val claim = RejectedGoodsMultipleClaim.empty(exampleEori).submitBasisOfClaim(basisOfClaim)
        claim.answers.basisOfClaim shouldBe Some(basisOfClaim)
      }
    }

    "change basis of claim" in {
      forAll(completeClaimGen, Gen.oneOf(BasisOfRejectedGoodsClaim.allButSpecialCircumstances)) {
        (claim, basisOfClaim) =>
          val modifiedClaim = claim.submitBasisOfClaim(basisOfClaim)

          modifiedClaim.hasCompleteAnswers           shouldBe true
          modifiedClaim.toOutput.map(_.basisOfClaim) shouldBe Right(basisOfClaim)
      }
    }

    "change basis of claim if special circumstances" in {
      forAll(completeClaimGenWithoutSpecialCircumstances) { claim =>
        val modifiedClaim = claim.submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)

        modifiedClaim.hasCompleteAnswers           shouldBe false
        modifiedClaim.toOutput.map(_.basisOfClaim) shouldBe Left(
          BASIS_OF_CLAIM_SPECIAL_CIRCUMSTANCES_MUST_BE_DEFINED :: Nil
        )
      }
    }

    "submit basis of claim special circumstances details" in {
      val claimEither = RejectedGoodsMultipleClaim
        .empty(exampleEori)
        .submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)
        .submitBasisOfClaimSpecialCircumstancesDetails(exampleSpecialCircumstancesDetails)

      claimEither.isRight shouldBe true

    }

    "change basis of claim special circumstances details" in {
      forAll(completeClaimGen) { claim =>
        val modifiedClaimEither =
          claim.submitBasisOfClaimSpecialCircumstancesDetails(exampleSpecialCircumstancesDetails)

        modifiedClaimEither.isRight shouldBe claim.needsSpecialCircumstancesBasisOfClaim
      }
    }

    "submit method of disposal" in {
      forAll(Gen.oneOf(MethodOfDisposal.values)) { methodOfDisposal =>
        val claim = RejectedGoodsMultipleClaim.empty(exampleEori).submitMethodOfDisposal(methodOfDisposal)
        claim.answers.methodOfDisposal shouldBe Some(methodOfDisposal)
      }
    }

    "change method of disposal" in {
      forAll(completeClaimGen, Gen.oneOf(MethodOfDisposal.values)) { (claim, methodOfDisposal) =>
        val modifiedClaim = claim.submitMethodOfDisposal(methodOfDisposal)

        modifiedClaim.hasCompleteAnswers               shouldBe true
        modifiedClaim.toOutput.map(_.methodOfDisposal) shouldBe Right(methodOfDisposal)
      }
    }

    "submit details of rejected goods" in {
      val claim = RejectedGoodsMultipleClaim
        .empty(exampleEori)
        .submitDetailsOfRejectedGoods(exampleRejectedGoodsDetails)

      claim.answers.detailsOfRejectedGoods shouldBe Some(exampleRejectedGoodsDetails)
    }

    "change details of rejected goods" in {
      forAll(completeClaimGen, exampleRejectedGoodsDetails) { (claim, rejectedGoodsDetails) =>
        val modifiedClaim = claim.submitDetailsOfRejectedGoods(rejectedGoodsDetails)

        modifiedClaim.hasCompleteAnswers                     shouldBe true
        modifiedClaim.toOutput.map(_.detailsOfRejectedGoods) shouldBe Right(rejectedGoodsDetails)
      }
    }

    "select valid tax codes for reimbursement when none yet selected" in {
      forAll(incompleteClaimWithMrnsGen(MRNS_SIZE)) { case (claim, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val taxCodes         = claim.getAvailableDuties(mrn).map(_._1)
          val selectedTaxCodes = if taxCodes.size > 1 then taxCodes.drop(1) else taxCodes
          val claimEither      = claim
            .selectAndReplaceTaxCodeSetForReimbursement(mrn, selectedTaxCodes)

          claimEither.isRight shouldBe true
        }
      }
    }

    "replace valid tax codes for reimbursement" in {
      forAll(incompleteClaimWithMrnsGen(MRNS_SIZE)) { case (claim, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val taxCodes = claim.getAvailableDuties(mrn).map(_._1).sorted
          taxCodes should not be empty
          val selectedTaxCodes  = if taxCodes.size > 1 then taxCodes.drop(1) else taxCodes
          val selectedTaxCodes2 = if taxCodes.size > 1 then taxCodes.dropRight(1) else taxCodes

          val claimEither = claim
            .selectAndReplaceTaxCodeSetForReimbursement(mrn, selectedTaxCodes)

          claimEither.getOrFail.getSelectedDuties(mrn) shouldBe Some(selectedTaxCodes)

          val claimEither2 =
            claimEither.flatMap(
              _.selectAndReplaceTaxCodeSetForReimbursement(mrn, selectedTaxCodes2)
            )

          claimEither2.getOrFail.getSelectedDuties(mrn) shouldBe Some(selectedTaxCodes2)
        }
      }

    }

    "select invalid tax codes for reimbursement" in {
      forAll(incompleteClaimWithMrnsGen(MRNS_SIZE)) { case (claim, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val availableDuties = claim.getAvailableDuties(mrn).map(_._1).toSet
          val claimEither     = claim
            .selectAndReplaceTaxCodeSetForReimbursement(
              mrn,
              TaxCodes.all.filter(tc => !availableDuties.contains(tc)).take(3)
            )

          claimEither shouldBe Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
        }
      }
    }

    "change tax code for reimbursement with the same set" in {
      forAll(completeClaimGen) { claim =>
        claim.answers.movementReferenceNumbers.get.foreach { mrn =>
          val modifiedClaimEither =
            claim.selectAndReplaceTaxCodeSetForReimbursement(mrn, claim.getSelectedDuties(mrn).get)

          val result = modifiedClaimEither.getOrFail
          result.getSelectedDuties(mrn) shouldBe claim.getSelectedDuties(mrn)
          result.hasCompleteAnswers     shouldBe true
        }
      }
    }

    "change tax code for reimbursement with a new valid set" in {
      forAll(incompleteClaimWithMrnsGen(MRNS_SIZE)) { case (claim, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val taxCodes                    = claim.getAvailableDuties(mrn).map(_._1).sorted
          val newTaxCodeSet: Seq[TaxCode] = taxCodes.take(2)

          val modifiedClaimEither = claim.selectAndReplaceTaxCodeSetForReimbursement(mrn, newTaxCodeSet)

          val result = modifiedClaimEither.getOrFail
          result.getSelectedDuties(mrn).get shouldBe newTaxCodeSet
        }
      }
    }

    "change tax code for reimbursement with a new invalid set" in {
      forAll(incompleteClaimWithMrnsGen(MRNS_SIZE)) { case (claim, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val invalidTaxCodeSet   = TaxCodes.all.takeRight(6)
          val modifiedClaimEither = claim.selectAndReplaceTaxCodeSetForReimbursement(mrn, invalidTaxCodeSet)
          modifiedClaimEither shouldBe Left("selectTaxCodeSetForReimbursement.someTaxCodesNotInACC14")
        }
      }
    }

    "submit valid correct amounts for selected tax codes" in {
      def submitData(mrn: MRN)(claim: RejectedGoodsMultipleClaim)(data: (TaxCode, BigDecimal)) =
        claim.submitCorrectAmount(mrn, data._1, data._2)

      forAll(incompleteClaimWithMrnsGen(MRNS_SIZE)) { case (claim, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val taxCodes            = claim.getAvailableDuties(mrn).map(_._1)
          val paidAmounts         = claim.getNdrcDetailsFor(mrn).map(_.map(_.amount).map(BigDecimal.apply)).getOrElse(Nil)
          val correctAmounts      = paidAmounts.map(_ / 2)
          val taxCodesWithAmounts = taxCodes.zip(correctAmounts)
          val modifiedClaim       = claim
            .selectAndReplaceTaxCodeSetForReimbursement(mrn, taxCodes)
            .flatMapEach(taxCodesWithAmounts, submitData(mrn))
            .getOrFail

          modifiedClaim.hasCompleteAnswers                  shouldBe false
          modifiedClaim.getTotalReimbursementAmountFor(mrn) shouldBe Some(correctAmounts.sum)
          modifiedClaim.getTotalReimbursementAmount         shouldBe correctAmounts.sum
        }
      }
    }

    "submit valid claim amounts for selected tax codes" in {
      def submitData(mrn: MRN)(claim: RejectedGoodsMultipleClaim)(data: (TaxCode, BigDecimal)) =
        claim.submitClaimAmount(mrn, data._1, data._2)

      forAll(incompleteClaimWithMrnsGen(MRNS_SIZE)) { case (claim, mrns) =>
        mrns.size shouldBe MRNS_SIZE

        mrns.foreach { mrn =>
          val taxCodes            = claim.getAvailableDuties(mrn).map(_._1)
          val paidAmounts         = claim.getNdrcDetailsFor(mrn).map(_.map(_.amount).map(BigDecimal.apply)).getOrElse(Nil)
          val claimAmounts        = paidAmounts.map(a => a / 4)
          val taxCodesWithAmounts = taxCodes.zip(claimAmounts)
          val modifiedClaim       = claim
            .selectAndReplaceTaxCodeSetForReimbursement(mrn, taxCodes)
            .flatMapEach(taxCodesWithAmounts, submitData(mrn))
            .getOrFail

          modifiedClaim.hasCompleteAnswers                  shouldBe false
          modifiedClaim.getTotalReimbursementAmountFor(mrn) shouldBe Some(claimAmounts.sum)
          modifiedClaim.getTotalReimbursementAmount         shouldBe claimAmounts.sum
        }
      }
    }

    "submit valid correct amount for wrong tax code" in {
      forAll(incompleteClaimWithMrnsGen(MRNS_SIZE)) { case (claim, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val taxCodes     = claim.getAvailableDuties(mrn).map(_._1)
          val wrongTaxCode = TaxCodes.all.find(tc => !taxCodes.contains(tc)).get
          val claimEither  = claim
            .selectAndReplaceTaxCodeSetForReimbursement(mrn, taxCodes)
            .flatMap(_.submitCorrectAmount(mrn, wrongTaxCode, BigDecimal("5.00")))

          claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
        }
      }
    }

    "submit invalid correct amount for the valid tax code" in {
      forAll(incompleteClaimWithMrnsGen(MRNS_SIZE)) { case (claim, mrns) =>
        mrns.size shouldBe MRNS_SIZE
        mrns.foreach { mrn =>
          val taxCode     = claim.getAvailableDuties(mrn).map(_._1).head
          val paidAmount  = claim.getNdrcDetailsFor(mrn).map(_.map(_.amount).map(BigDecimal.apply)).getOrElse(Nil).head
          val claimEither = claim.selectAndReplaceTaxCodeSetForReimbursement(mrn, Seq(taxCode))

          val claimEitherTestNegative =
            claimEither.flatMap(_.submitCorrectAmount(mrn, taxCode, BigDecimal("-10.00")))
          val claimEitherTestGreater  =
            claimEither.flatMap(_.submitCorrectAmount(mrn, taxCode, paidAmount + 1))
          val claimEitherTestSame     =
            claimEither.flatMap(_.submitCorrectAmount(mrn, taxCode, paidAmount))

          claimEitherTestNegative shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
          claimEitherTestGreater  shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
          claimEitherTestSame     shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
        }
      }
    }

    "submit invalid correct amount for wrong tax code" in {
      val importDeclaration = buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("10.00"), false)))
      val claimEither       = RejectedGoodsMultipleClaim
        .empty(exampleEori)
        .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, importDeclaration)
        .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
        .flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A80, BigDecimal("0.00")))

      claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
    }

    "change to valid correct amount for selected tax code" in {
      forAll(completeClaimGen) { claim =>
        claim.answers.movementReferenceNumbers.get.foreach { mrn =>
          val totalAmount: BigDecimal              = claim.getTotalReimbursementAmount
          val taxCodes: Seq[(TaxCode, BigDecimal)] = claim.getReimbursementClaims(mrn).toSeq
          for (taxCode, reimbursementAmount) <- taxCodes do {
            val paidAmount         = claim.getAmountPaidFor(mrn, taxCode).get
            val newCorrectedAmount = (paidAmount - reimbursementAmount) / 2
            val claimEither        = claim.submitCorrectAmount(mrn, taxCode, newCorrectedAmount)
            val modifiedClaim      = claimEither.getOrFail
            modifiedClaim.getTotalReimbursementAmount shouldBe (totalAmount + newCorrectedAmount)
          }
        }
      }
    }

    "change to invalid correct amount for selected tax code" in {
      forAll(completeClaimGen) { claim =>
        val leadMrn                              = claim.getLeadMovementReferenceNumber.get
        val taxCodes: Seq[(TaxCode, BigDecimal)] = claim.getReimbursementClaims(leadMrn).toSeq
        for (taxCode, _) <- taxCodes do {
          val newAmount   = BigDecimal("-10.00")
          val claimEither = claim.submitCorrectAmount(leadMrn, taxCode, newAmount)

          claimEither shouldBe Left("submitCorrectAmount.invalidReimbursementAmount")
        }
      }
    }

    "change to valid correct amount for the tax code not in ACC14" in {
      forAll(completeClaimGen) { claim =>
        val leadMrn      = claim.getLeadMovementReferenceNumber.get
        val taxCodeSet   =
          claim.getNdrcDetailsFor(leadMrn).map(_.map(_.taxType).map(TaxCode.apply).toSet).getOrElse(Set.empty)
        val wrongTaxCode = TaxCodes.all.find(taxCode => !taxCodeSet.contains(taxCode)).getOrElse(TaxCode.NI633)
        val claimEither  = claim.submitCorrectAmount(leadMrn, wrongTaxCode, BigDecimal("10.00"))
        claimEither shouldBe Left("submitCorrectAmount.taxCodeNotInACC14")
      }
    }

    "submit inspection date" in {
      forAll(DateGen.genDate) { inspectionDate =>
        val claim = RejectedGoodsMultipleClaim.empty(exampleEori).submitInspectionDate(inspectionDate)

        claim.answers.inspectionDate shouldBe Some(inspectionDate)

      }
    }

    "change inspection date" in {
      forAll(completeClaimGen, DateGen.genDate) { (claim, inspectionDate) =>
        val modifiedClaim = claim.submitInspectionDate(inspectionDate)

        modifiedClaim.hasCompleteAnswers             shouldBe true
        modifiedClaim.toOutput.map(_.inspectionDate) shouldBe Right(inspectionDate)

      }
    }

    "submit inspection address" in {
      forAll(InspectionAddressGen.genInspectionAddress) { inspectionAddress =>
        val claim = RejectedGoodsMultipleClaim.empty(exampleEori).submitInspectionAddress(inspectionAddress)

        claim.answers.inspectionAddress shouldBe Some(inspectionAddress)

      }
    }

    "change inspection address" in {
      forAll(completeClaimGen, InspectionAddressGen.genInspectionAddress) { (claim, inspectionAddress) =>
        val modifiedClaim = claim.submitInspectionAddress(inspectionAddress)

        modifiedClaim.hasCompleteAnswers                shouldBe true
        modifiedClaim.toOutput.map(_.inspectionAddress) shouldBe Right(inspectionAddress)
      }
    }

    "submit bankAccountDetails and bankAccountType if reimbursement method is BankAccountTransfer" in {
      val importDeclarationAllCMAEligible =
        buildImportDeclaration(dutyDetails = Seq((TaxCode.A00, BigDecimal("1.00"), true)))
      val claimEither                     =
        RejectedGoodsMultipleClaim
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, importDeclarationAllCMAEligible)
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, Seq(TaxCode.A00)))
          .flatMap(_.submitCorrectAmount(exampleMrn, TaxCode.A00, BigDecimal("0.50")))
          .flatMap(_.submitBankAccountDetails(exampleBankAccountDetails))
          .flatMap(_.submitBankAccountType(BankAccountType.Business))

      claimEither.isRight shouldBe true
    }

    "change bankAccountDetails in a complete claim with all duties CMA eligible" in {
      forAll(completeClaimCMAEligibleGen) { claim =>
        val claimEither =
          claim.submitBankAccountDetails(exampleBankAccountDetails)

        claimEither.isRight shouldBe claim.needsBanksAccountDetailsSubmission
      }
    }

    "change bankAccountDetails in a complete claim not eligible for CMA" in {
      forAll(completeClaimNotCMAEligibleGen) { claim =>
        val claimEither =
          claim.submitBankAccountDetails(exampleBankAccountDetails)

        claimEither.isRight shouldBe true
      }
    }

    "hasCompleteReimbursementClaims" when {
      "return true if all claim amounts are present" in {
        forAll(completeClaimGen) { claim =>
          claim.hasCompleteReimbursementClaims shouldBe true
        }
      }

      "return false if at least one of the claimed tax code do not have a value specified" in {
        forAll(importDeclarationGen, Acc14Gen.genListNdrcDetails()) {
          (importDeclaration: ImportDeclaration, ndrcDetails: List[NdrcDetails]) =>
            whenever(
              ndrcDetails.size > 1 && ndrcDetails.forall(details => BigDecimal(details.amount) > 2) && ndrcDetails
                .map(_.taxType)
                .toSet
                .size == ndrcDetails.size
            ) {
              val taxCodes     = ndrcDetails.map(details => TaxCode(details.taxType))
              val drd          = importDeclaration.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
              val updatedDd    = importDeclaration.copy(displayResponseDetail = drd)
              val initialClaim = RejectedGoodsMultipleClaim
                .empty(exampleEori)
                .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, updatedDd)
                .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(exampleMrn, taxCodes))
                .getOrFail
              val claimToTest  = ndrcDetails.dropRight(1).foldLeft(initialClaim) { case (claim, ndrcDetails) =>
                claim.submitCorrectAmount(exampleMrn, TaxCode(ndrcDetails.taxType), 1).getOrFail
              }
              claimToTest.hasCompleteReimbursementClaims shouldBe false
            }
        }
      }

      "return false if no tax codes have been claimed yet" in {
        forAll(importDeclarationGen, Acc14Gen.genListNdrcDetails()) {
          (importDeclaration: ImportDeclaration, ndrcDetails: List[NdrcDetails]) =>
            whenever(
              ndrcDetails.size > 1 && ndrcDetails.forall(details => BigDecimal(details.amount) > 2) && ndrcDetails
                .map(_.taxType)
                .toSet
                .size == ndrcDetails.size
            ) {
              val drd       = importDeclaration.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
              val updatedDd = importDeclaration.copy(displayResponseDetail = drd)
              val claim     = RejectedGoodsMultipleClaim
                .empty(exampleEori)
                .submitMovementReferenceNumberAndDeclaration(0, exampleMrn, updatedDd)
                .getOrFail

              claim.hasCompleteReimbursementClaims shouldBe false
            }
        }
      }
    }

    "validate if any subsidy payment method is in the declaration" in {

      import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarationSupport.withSomeSubsidiesPaymentMethod

      val declaration =
        buildImportDeclaration(dutyDetails = Seq((TaxCode.A50, 100, false)))
          .withSomeSubsidiesPaymentMethod()

      val claim = RejectedGoodsMultipleClaim
        .empty(
          exampleEori
        )
        .submitMovementReferenceNumberAndDeclaration(exampleMrn, declaration)
        .getOrFail

      RejectedGoodsMultipleClaim.Checks.declarationsHasNoSubsidyPayments.apply(
        claim
      ) shouldBe Validator.Invalid(DISPLAY_DECLARATION_HAS_SUBSIDY_PAYMENT)

    }
  }
}
