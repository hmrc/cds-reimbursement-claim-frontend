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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import cats.syntax.eq._
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType

import scala.collection.immutable.SortedMap
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes

import scala.collection.JavaConverters._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import scala.util.Random

/** A collection of generators supporting the tests of OverpaymentsSingleJourney. */
object OverpaymentsScheduledJourneyGenerators extends JourneyGenerators with JourneyTestData {

  val ZERO = BigDecimal("0.00")

  val emptyJourney: OverpaymentsScheduledJourney =
    OverpaymentsScheduledJourney.empty(exampleEori)

  val journeyWithMrnAndDeclaration: OverpaymentsScheduledJourney =
    OverpaymentsScheduledJourney
      .empty(exampleDisplayDeclaration.getDeclarantEori, Nonce.random)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
      .getOrFail

  val completeJourneyWithMatchingUserEoriGen: Gen[OverpaymentsScheduledJourney] =
    Gen.oneOf(
      buildCompleteJourneyGen(
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = false
      ),
      buildCompleteJourneyGen(
        acc14ConsigneeMatchesUserEori = false,
        acc14DeclarantMatchesUserEori = true
      ),
      buildCompleteJourneyGen(
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = true
      )
    )

  val completeJourneyWithNonNatchingUserEoriGen: Gen[OverpaymentsScheduledJourney] =
    buildCompleteJourneyGen(
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeJourneyGen: Gen[OverpaymentsScheduledJourney] =
    Gen.oneOf(
      completeJourneyWithMatchingUserEoriGen,
      completeJourneyWithNonNatchingUserEoriGen
    )

  val dutyTypesGen: Gen[Seq[DutyType]] =
    for {
      n   <- Gen.choose(1, DutyTypes.all.size - 1)
      dts <- Gen.pick(n, DutyTypes.all)
    } yield dts.sorted

  def taxCodesGen(dutyType: DutyType): Gen[Seq[TaxCode]] =
    for {
      n   <- Gen.choose(1, dutyType.taxCodes.size - 1)
      tcs <- Gen.pick(n, dutyType.taxCodes)
    } yield tcs.sorted

  val dutyTypesWithTaxCodesGen: Gen[Seq[(DutyType, Seq[TaxCode])]] = dutyTypesGen.flatMap(dutyTypes =>
    Gen.sequence[Seq[(DutyType, Seq[TaxCode])], (DutyType, Seq[TaxCode])](
      dutyTypes.map(dutyType =>
        for {
          n   <- Gen.choose(1, dutyType.taxCodes.size - 1)
          tcs <- Gen.pick(n, dutyType.taxCodes)
        } yield (dutyType, tcs.sorted)
      )
    )
  )

  type TaxCodeWithAmounts = (TaxCode, BigDecimal, BigDecimal)

  def taxCodesWithClaimAmountsGen(dutyType: DutyType): Gen[Seq[TaxCodeWithAmounts]] =
    for {
      n       <- Gen.choose(1, dutyType.taxCodes.size - 1)
      tcs     <- Gen.pick(n, dutyType.taxCodes)
      amounts <- Gen.sequence[Seq[TaxCodeWithAmounts], TaxCodeWithAmounts](
                   tcs.sorted.map(tc =>
                     amountNumberGen
                       .flatMap(paid =>
                         amountNumberInRangeGen(ZERO, paid - BigDecimal("0.01"))
                           .map(correct => (tc, paid, correct))
                       )
                   )
                 )
    } yield amounts

  val dutyTypesWithTaxCodesWithClaimAmountsGen: Gen[Seq[(DutyType, Seq[TaxCodeWithAmounts])]] =
    for {
      dutyTypes <- dutyTypesGen
      result    <-
        Gen.sequence(dutyTypes.map(dutyType => taxCodesWithClaimAmountsGen(dutyType).map(tcs => dutyType -> tcs)))
    } yield result.asScala

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def buildCompleteJourneyGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    taxCodes: Seq[TaxCode] = TaxCodes.all
  ): Gen[OverpaymentsScheduledJourney] =
    buildJourneyGen(
      acc14DeclarantMatchesUserEori,
      acc14ConsigneeMatchesUserEori,
      hasConsigneeDetailsInACC14,
      submitConsigneeDetails = submitConsigneeDetails,
      submitContactDetails = submitContactDetails,
      submitContactAddress = submitContactAddress,
      submitBankAccountType = submitBankAccountType,
      submitBankAccountDetails = submitBankAccountDetails,
      taxCodes = taxCodes
    ).map(
      _.fold(
        error =>
          throw new Exception(
            s"Cannot build complete OverpaymentsScheduledJourney because of $error, fix the test data generator."
          ),
        identity
      )
    )
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def buildJourneyGenWithoutSupportingEvidence(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    taxCodes: Seq[TaxCode] = TaxCodes.all
  ): Gen[OverpaymentsScheduledJourney] =
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
      submitEvidence = true,
      taxCodes = taxCodes,
      emptyDocumentType = true
    ).map(OverpaymentsScheduledJourney.tryBuildFrom(_))
      .map(
        _.fold(
          error =>
            throw new Exception(
              s"Cannot build complete OverpaymentsScheduledJourney because of $error, fix the test data generator."
            ),
          identity
        )
      )

  def buildJourneyGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    taxCodes: Seq[TaxCode] = TaxCodes.all
  ): Gen[Either[String, OverpaymentsScheduledJourney]] =
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
      submitEvidence = true,
      taxCodes = taxCodes
    ).map(OverpaymentsScheduledJourney.tryBuildFrom(_))

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
    submitEvidence: Boolean = true,
    taxCodes: Seq[TaxCode] = TaxCodes.all,
    forcedTaxCodes: Seq[TaxCode] = Seq.empty,
    checkYourAnswersChangeMode: Boolean = true,
    emptyDocumentType: Boolean = false
  ): Gen[OverpaymentsScheduledJourney.Answers] =
    for {
      userEoriNumber              <- IdGen.genEori
      mrn                         <- IdGen.genMRN
      declarantEORI               <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      reimbursements              <- dutyTypesWithTaxCodesWithClaimAmountsGen
      basisOfClaim                <- Gen.oneOf(BasisOfOverpaymentClaim.values)
      numberOfTaxCodes            <- Gen.choose(1, 5)
      taxCodes                    <- Gen
                                       .pick(
                                         numberOfTaxCodes,
                                         if (basisOfClaim === BasisOfOverpaymentClaim.IncorrectExciseValue) TaxCodes.excise else taxCodes
                                       )
                                       .map(_ ++ forcedTaxCodes)
      paidAmounts                 <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      whetherNorthernIreland      <- Gen.oneOf(true, false)
      numberOfSupportingEvidences <- Gen.choose(0, 3)
      numberOfDocumentTypes       <- Gen.choose(1, 2)
      documentTypes               <-
        Gen.listOfN(numberOfDocumentTypes, Gen.oneOf(UploadDocumentType.overpaymentsSingleDocumentTypes))
      supportingEvidences         <-
        Gen
          .sequence[Seq[(UploadDocumentType, Int)], (UploadDocumentType, Int)](
            documentTypes.map(dt => Gen.choose(0, numberOfSupportingEvidences).map(n => (dt, n)))
          )
          .map(_.toMap)
      bankAccountType             <- Gen.oneOf(BankAccountType.values)
      consigneeContact            <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact            <- Gen.option(Acc14Gen.genContactDetails)
    } yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, Random.nextBoolean()) }

      val reimbursementClaims                             =
        SortedMap(reimbursements: _*)
          .mapValues(s =>
            SortedMap(s.map { case (taxCode, paid, correct) =>
              (taxCode, Option(AmountPaidWithCorrect(paid, correct)))
            }: _*)
          )

      val displayDeclaration: DisplayDeclaration =
        buildDisplayDeclaration(
          mrn.value,
          declarantEORI,
          if (hasConsigneeDetailsInACC14) Some(consigneeEORI) else None,
          paidDuties,
          consigneeContact = if (submitConsigneeDetails) consigneeContact else None,
          declarantContact = declarantContact
        )

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val supportingEvidencesExpanded: Seq[UploadedFile] =
        supportingEvidences.flatMap { case (documentType, size) =>
          if (!emptyDocumentType)
            (0 until size).map(i => buildUploadDocument(s"$i").copy(cargo = Some(documentType)))
          else
            (0 until size).map(i => buildUploadDocument(s"$i").copy(cargo = None))
        }.toSeq

      val scheduledDocument: UploadedFile =
        buildUploadDocument("schedule").copy(cargo = Some(UploadDocumentType.ScheduleOfMRNs))

      val answers =
        OverpaymentsScheduledJourney.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          displayDeclaration = Some(displayDeclaration),
          consigneeEoriNumber = if (submitConsigneeDetails && !hasMatchingEori) Some(consigneeEORI) else None,
          declarantEoriNumber = if (submitDeclarantDetails && !hasMatchingEori) Some(declarantEORI) else None,
          contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
          contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          whetherNorthernIreland = Some(whetherNorthernIreland),
          additionalDetails = Some("additional details"),
          reimbursementClaims = Some(reimbursementClaims),
          selectedDocumentType = None,
          scheduledDocument = Some(scheduledDocument),
          supportingEvidences =
            if (submitEvidence) supportingEvidencesExpanded
            else Seq.empty,
          bankAccountDetails =
            if (submitBankAccountDetails)
              Some(exampleBankAccountDetails)
            else None,
          bankAccountType =
            if (submitBankAccountType)
              Some(bankAccountType)
            else None,
          checkYourAnswersChangeMode = checkYourAnswersChangeMode
        )

      answers
    }

  def buildJourneyGen(answersGen: Gen[OverpaymentsScheduledJourney.Answers]): Gen[OverpaymentsScheduledJourney] =
    answersGen.map(
      OverpaymentsScheduledJourney
        .tryBuildFrom(_)
        .fold(e => throw new Exception(e), identity)
    )

}