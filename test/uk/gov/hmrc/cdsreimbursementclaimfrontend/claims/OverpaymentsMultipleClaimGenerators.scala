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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectEoriAndDan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Dan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EoriNumbersVerification
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimModes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.NewEoriAndDan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.OrderedMap

import scala.jdk.CollectionConverters.*
import scala.util.Random

/** A collection of generators supporting the tests of OverpaymentsMultipleClaim. */
object OverpaymentsMultipleClaimGenerators extends ClaimGenerators with ClaimTestData {

  val emptyClaim: OverpaymentsMultipleClaim =
    OverpaymentsMultipleClaim.empty(exampleEori)

  val claimWithMrnAndDeclaration: OverpaymentsMultipleClaim =
    OverpaymentsMultipleClaim
      .empty(exampleDisplayDeclaration.getDeclarantEori, Nonce.random)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
      .getOrFail

  def claimWithMrnAndDeclarationWithFeatures(
    features: OverpaymentsMultipleClaim.Features
  ): OverpaymentsMultipleClaim =
    OverpaymentsMultipleClaim
      .empty(exampleDisplayDeclaration.getDeclarantEori, Nonce.random, features = Some(features))
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
      .getOrFail

  val completeClaimWithMatchingUserEoriGen: Gen[OverpaymentsMultipleClaim] =
    Gen.oneOf(
      buildCompleteClaimGen(
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = false
      ),
      buildCompleteClaimGen(
        acc14ConsigneeMatchesUserEori = false,
        acc14DeclarantMatchesUserEori = true
      ),
      buildCompleteClaimGen(
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = true
      )
    )

  val completeClaimWithNonNatchingUserEoriGen: Gen[OverpaymentsMultipleClaim] =
    buildCompleteClaimGen(
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeClaimGen: Gen[OverpaymentsMultipleClaim] =
    Gen.oneOf(
      completeClaimWithMatchingUserEoriGen,
      completeClaimWithNonNatchingUserEoriGen
    )

  val completeClaimNotGen: Gen[OverpaymentsMultipleClaim] =
    Gen.oneOf(
      completeClaimWithMatchingUserEoriGen,
      completeClaimWithNonNatchingUserEoriGen
    )

  def buildCompleteClaimGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    minNumberOfMRNs: Int = 2,
    maxNumberOfMRNs: Int = 6,
    maxSize: Int = 5,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[OverpaymentsMultipleClaim.Features] = None,
    payeeType: Option[PayeeType] = Some(PayeeType.Declarant)
  ): Gen[OverpaymentsMultipleClaim] =
    buildClaimGen(
      acc14DeclarantMatchesUserEori,
      acc14ConsigneeMatchesUserEori,
      hasConsigneeDetailsInACC14,
      submitConsigneeDetails = submitConsigneeDetails,
      submitContactDetails = submitContactDetails,
      submitContactAddress = submitContactAddress,
      submitBankAccountType = submitBankAccountType,
      submitBankAccountDetails = submitBankAccountDetails,
      minNumberOfMRNs = minNumberOfMRNs,
      maxNumberOfMRNs = maxNumberOfMRNs,
      maxSize = maxSize,
      generateSubsidyPayments = generateSubsidyPayments,
      features = features,
      payeeType = payeeType
    ).map(
      _.fold(
        error =>
          throw new Exception(
            s"Cannnot build complete OverpaymentsMultipleClaim because of $error, fix the test data generator."
          ),
        identity
      )
    )

  type TaxCodesAndAmounts = (Seq[TaxCode], Seq[TaxCode], List[BigDecimal], Seq[BigDecimal])

  def taxCodesAndAmountsGen(maxSize: Int): Gen[TaxCodesAndAmounts] = for
    numberOfTaxCodes         <- Gen.choose(1, maxSize)
    numberOfSelectedTaxCodes <- Gen.choose(1, numberOfTaxCodes)
    taxCodes                 <- Gen.pick(numberOfTaxCodes, TaxCodes.all).map(_.distinct)
    paidAmounts              <- Gen.listOfN(numberOfTaxCodes, amountNumberGen)
    correctedAmounts         <-
      Gen
        .sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts
            .take(numberOfSelectedTaxCodes)
            .map(a => Gen.choose(BigDecimal("0.00"), a - BigDecimal.exact("0.01")))
        )
  yield (taxCodes.toSeq, taxCodes.take(numberOfSelectedTaxCodes).toSeq, paidAmounts, correctedAmounts)

  def buildClaimGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    minNumberOfMRNs: Int = 2,
    maxNumberOfMRNs: Int = 6,
    maxSize: Int = 5,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[OverpaymentsMultipleClaim.Features] = None,
    payeeType: Option[PayeeType] = None
  ): Gen[Either[String, OverpaymentsMultipleClaim]] =
    buildAnswersGen(
      acc14DeclarantMatchesUserEori,
      acc14ConsigneeMatchesUserEori,
      hasConsigneeDetailsInACC14,
      submitDeclarantDetails,
      submitConsigneeDetails,
      submitContactDetails,
      submitContactAddress,
      submitBankAccountDetails,
      submitBankAccountType,
      minNumberOfMRNs,
      maxNumberOfMRNs,
      maxSize,
      generateSubsidyPayments = generateSubsidyPayments,
      payeeType
    )
      .map(OverpaymentsMultipleClaim.tryBuildFrom(_, features))

  def buildAnswersGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    minNumberOfMRNs: Int = 2,
    maxNumberOfMRNs: Int = 6,
    maxSize: Int = 5,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    payeeType: Option[PayeeType] = None
  ): Gen[OverpaymentsMultipleClaim.Answers] =
    for
      userEoriNumber              <- IdGen.genEori
      numberOfMRNs                <- Gen.choose(minNumberOfMRNs, Math.max(minNumberOfMRNs, maxNumberOfMRNs))
      mrns                        <- Gen.listOfN(numberOfMRNs, IdGen.genMRN)
      declarantEORI               <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      taxCodesWithAmounts         <- Gen.sequence(mrns.map(_ => taxCodesAndAmountsGen(maxSize))).map(_.asScala)
      basisOfClaim                <- Gen.oneOf(BasisOfOverpaymentClaim.values)
      numberOfSupportingEvidences <- Gen.choose(1, 3)
      numberOfDocumentTypes       <- Gen.choose(1, 2)
      documentTypes               <-
        Gen.listOfN(numberOfDocumentTypes, Gen.oneOf(UploadDocumentType.overpaymentsMultipleDocumentTypes))
      supportingEvidences         <-
        Gen
          .sequence[Seq[(UploadDocumentType, Int)], (UploadDocumentType, Int)](
            documentTypes.map(dt => Gen.choose(1, numberOfSupportingEvidences).map(n => (dt, n)))
          )
          .map(_.toMap)
      bankAccountType             <- Gen.oneOf(BankAccountType.values)
      consigneeContact            <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact            <- Gen.option(Acc14Gen.genContactDetails)
      newEoriAndDan                = basisOfClaim match {
                                       case boc if boc == IncorrectEoriAndDan =>
                                         Some(NewEoriAndDan(IdGen.genEori.sample.get, IdGen.genDan.sample.get.value))
                                       case _                                 => None
                                     }
    yield {

      val paidDuties: Seq[(MRN, Seq[(TaxCode, BigDecimal, Boolean)])] =
        mrns.zip(taxCodesWithAmounts).map { case (mrn, (taxCodes, _, paidAmounts, _)) =>
          (mrn, taxCodes.zip(paidAmounts).map { case (t, r) => (t, r, Random.nextBoolean()) })
        }

      val correctedAmounts: OrderedMap[MRN, OrderedMap[TaxCode, Option[BigDecimal]]] =
        OrderedMap.from(
          mrns
            .zip(taxCodesWithAmounts)
            .map { case (mrn, (_, selectedTaxCodes, _, correctedAmounts)) =>
              (
                mrn,
                OrderedMap.from(
                  selectedTaxCodes
                    .zip(correctedAmounts)
                    .map { case (taxCode, correctAmount) => (taxCode, Option(correctAmount)) }
                )
              )
            }
        )

      val displayDeclarations: Seq[DisplayDeclaration] =
        paidDuties.map { case (mrn, paidDutiesPerMrn) =>
          buildDisplayDeclaration(
            mrn.value,
            declarantEORI,
            if hasConsigneeDetailsInACC14 then Some(consigneeEORI) else None,
            paidDutiesPerMrn,
            consigneeContact = if submitConsigneeDetails then consigneeContact else None,
            declarantContact = declarantContact,
            generateSubsidyPayments = generateSubsidyPayments
          )
        }

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val supportingEvidencesExpanded: Seq[UploadedFile] =
        supportingEvidences.flatMap { case (documentType, size) =>
          (0 until size).map(i => buildUploadDocument(s"$i").copy(cargo = Some(documentType)))
        }.toSeq

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if submitConsigneeDetails && !hasMatchingEori then {
          if submitDeclarantDetails then Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      val answers =
        OverpaymentsMultipleClaim.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumbers = Some(mrns),
          payeeType = payeeType,
          displayDeclarations = Some(displayDeclarations),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
          contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          additionalDetails = Some("additional details"),
          correctedAmounts = Some(OrderedMap(correctedAmounts)),
          selectedDocumentType = None,
          supportingEvidences = supportingEvidencesExpanded,
          bankAccountDetails = if submitBankAccountDetails then Some(exampleBankAccountDetails) else None,
          bankAccountType = if submitBankAccountType then Some(bankAccountType) else None,
          modes = ClaimModes(checkYourAnswersChangeMode = true),
          newEori = newEoriAndDan.map(_.eori),
          newDan = newEoriAndDan.map(d => Dan(d.dan))
        )

      answers
    }

  def buildClaimFromAnswersGen(
    answersGen: Gen[OverpaymentsMultipleClaim.Answers]
  ): Gen[OverpaymentsMultipleClaim] =
    answersGen.map(
      OverpaymentsMultipleClaim
        .tryBuildFrom(_)
        .fold(e => throw new Exception(e), identity)
    )

  def incompleteClaimWithMrnsGen(n: Int): Gen[(OverpaymentsMultipleClaim, Seq[MRN])] = {
    def submitData(claim: OverpaymentsMultipleClaim)(data: ((MRN, DisplayDeclaration), Int)) =
      claim.submitMovementReferenceNumberAndDeclaration(data._2, data._1._1, data._1._2)

    listOfExactlyN(n, mrnWithDisplayDeclarationGen).map { data =>
      val dataWithIndex: List[((MRN, DisplayDeclaration), Int)] = data.zipWithIndex
      (
        emptyClaim
          .flatMapEach(dataWithIndex, submitData)
          .getOrFail,
        data.map(_._1)
      )
    }
  }

  private def mrnWithSelectedTaxCodesGen(claim: OverpaymentsMultipleClaim): Seq[Gen[(MRN, Seq[TaxCode])]] =
    claim.answers.movementReferenceNumbers.get.map { mrn =>
      val availableTaxCodes = claim.getAvailableDuties(mrn).map(_._1)
      Gen
        .choose(1, availableTaxCodes.size)
        .map(availableTaxCodes.take)
        .map(seq => (mrn, seq))
    }

  def incompleteClaimWithSelectedDutiesGen(n: Int): Gen[(OverpaymentsMultipleClaim, Seq[MRN])] = {
    def submitData(claim: OverpaymentsMultipleClaim)(data: (MRN, Seq[TaxCode])) =
      claim.selectAndReplaceTaxCodeSetForReimbursement(data._1, data._2)

    incompleteClaimWithMrnsGen(n).flatMap { case (claim, _) =>
      val gen = mrnWithSelectedTaxCodesGen(claim)
      Gen
        .sequence[Seq[(MRN, Seq[TaxCode])], (MRN, Seq[TaxCode])](gen)
        .map { mrnsWithTaxCodesSelection =>
          val modifiedClaim = claim
            .flatMapEach(mrnsWithTaxCodesSelection, submitData)
            .getOrFail
          (
            modifiedClaim,
            modifiedClaim.answers.movementReferenceNumbers.get
          )
        }
    }
  }

  def incompleteClaimWithCompleteClaimsGen(n: Int): Gen[(OverpaymentsMultipleClaim, Seq[MRN])] = {
    def submitData(claim: OverpaymentsMultipleClaim)(data: (MRN, TaxCode, BigDecimal)) =
      claim.submitClaimAmount(data._1, data._2, data._3)

    incompleteClaimWithSelectedDutiesGen(n).map { case (claim, mrns) =>
      val data: Seq[(MRN, TaxCode, BigDecimal)] = mrns.flatMap { mrn =>
        claim.getSelectedDuties(mrn).get.map { taxCode =>
          (mrn, taxCode, claim.getAmountPaidFor(mrn, taxCode).get)
        }
      }
      (claim.flatMapEach(data, submitData).getOrFail, mrns)
    }
  }

  def answersUpToBasisForClaimGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    allDutiesCmaEligible: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    taxCodes: Seq[TaxCode] = TaxCodes.all,
    forcedTaxCodes: Seq[TaxCode] = Seq.empty,
    currentUserEoriNumber: Gen[Eori] = IdGen.genEori
  ): Gen[OverpaymentsMultipleClaim.Answers] =
    for
      userEoriNumber   <- currentUserEoriNumber
      mrns             <- Gen.listOfN(3, IdGen.genMRN)
      declarantEORI    <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI    <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      numberOfTaxCodes <- Gen.choose(1, 5)
      taxCodes         <- Gen.pick(numberOfTaxCodes, taxCodes).map(_ ++ forcedTaxCodes)
      paidAmounts      <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      consigneeContact <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact <- Gen.option(Acc14Gen.genContactDetails)
    yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }.toSeq

      val displayDeclarations: Seq[DisplayDeclaration] = mrns.map { mrn =>
        buildDisplayDeclaration(
          mrn.value,
          declarantEORI,
          if hasConsigneeDetailsInACC14 then Some(consigneeEORI) else None,
          paidDuties,
          consigneeContact = if submitConsigneeDetails then consigneeContact else None,
          declarantContact = declarantContact
        )
      }

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if submitConsigneeDetails && !hasMatchingEori then {
          if submitDeclarantDetails then Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      OverpaymentsMultipleClaim.Answers(
        nonce = Nonce.random,
        userEoriNumber = userEoriNumber,
        movementReferenceNumbers = Some(mrns),
        displayDeclarations = Some(displayDeclarations),
        eoriNumbersVerification = eoriNumbersVerification,
        contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
        contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
        modes = ClaimModes(checkYourAnswersChangeMode = false)
      )
    }

}
