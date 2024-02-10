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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EoriNumbersVerification
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile

import scala.collection.immutable.SortedMap
import scala.util.Random

/** A collection of generators supporting the tests of OverpaymentsSingleJourney. */
object OverpaymentsScheduledJourneyGenerators extends ScheduledJourneyGenerators with JourneyTestData {

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
    taxCodes: Seq[TaxCode] = TaxCodes.all,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[OverpaymentsScheduledJourney.Features] = None,
    payeeType: PayeeType = PayeeType.Consignee
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
      taxCodes = taxCodes,
      generateSubsidyPayments = generateSubsidyPayments,
      features = features,
      payeeType = Some(payeeType)
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
    taxCodes: Seq[TaxCode] = TaxCodes.all,
    payeeType: PayeeType = PayeeType.Consignee
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
      emptyDocumentType = true,
      payeeType = Some(payeeType)
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
    taxCodes: Seq[TaxCode] = TaxCodes.all,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[OverpaymentsScheduledJourney.Features] = None,
    payeeType: Option[PayeeType] = None
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
      taxCodes = taxCodes,
      generateSubsidyPayments = generateSubsidyPayments,
      payeeType = payeeType
    ).map(OverpaymentsScheduledJourney.tryBuildFrom(_, features))

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
    emptyDocumentType: Boolean = false,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    payeeType: Option[PayeeType] = None
  ): Gen[OverpaymentsScheduledJourney.Answers] =
    for {
      userEoriNumber              <- IdGen.genEori
      mrn                         <- IdGen.genMRN
      declarantEORI               <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      reimbursements              <- dutyTypesWithTaxCodesWithClaimAmountsGen
      basisOfClaim                <- Gen.oneOf(BasisOfOverpaymentClaim.values - BasisOfOverpaymentClaim.DuplicateEntry)
      numberOfTaxCodes            <- Gen.choose(if (generateSubsidyPayments == GenerateSubsidyPayments.Some) 2 else 1, 5)
      taxCodes                    <- Gen
                                       .pick(
                                         numberOfTaxCodes,
                                         if (basisOfClaim === BasisOfOverpaymentClaim.IncorrectExciseValue) TaxCodes.excise else taxCodes
                                       )
                                       .map(_ ++ forcedTaxCodes)
      paidAmounts                 <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      numberOfSupportingEvidences <- Gen.choose(1, 3)
      numberOfDocumentTypes       <- Gen.choose(1, 2)
      documentTypes               <-
        Gen.listOfN(numberOfDocumentTypes, Gen.oneOf(UploadDocumentType.overpaymentsSingleDocumentTypes))
      supportingEvidences         <-
        Gen
          .sequence[Seq[(UploadDocumentType, Int)], (UploadDocumentType, Int)](
            documentTypes.map(dt => Gen.choose(1, numberOfSupportingEvidences).map(n => (dt, n)))
          )
          .map(_.toMap)
      bankAccountType             <- Gen.oneOf(BankAccountType.values)
      consigneeContact            <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact            <- Gen.option(Acc14Gen.genContactDetails)
    } yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, Random.nextBoolean()) }.toSeq

      val correctedAmounts                                =
        SortedMap
          .from(reimbursements)
          .view
          .mapValues(s =>
            SortedMap(s.map { case (taxCode, paid, correct) =>
              (taxCode, Option(AmountPaidWithCorrect(paid, correct)))
            }: _*)
          )
          .to(SortedMap)

      val displayDeclaration: DisplayDeclaration =
        buildDisplayDeclaration(
          mrn.value,
          declarantEORI,
          if (hasConsigneeDetailsInACC14) Some(consigneeEORI) else None,
          paidDuties,
          consigneeContact = if (submitConsigneeDetails) consigneeContact else None,
          declarantContact = declarantContact,
          generateSubsidyPayments = generateSubsidyPayments
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

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if (submitConsigneeDetails && !hasMatchingEori) {
          if (submitDeclarantDetails)
            Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else
            Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      val answers =
        OverpaymentsScheduledJourney.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          displayDeclaration = Some(displayDeclaration),
          payeeType = payeeType,
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
          contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          additionalDetails = Some("additional details"),
          correctedAmounts = Some(correctedAmounts),
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

  def buildJourneyFromAnswersGen(
    answersGen: Gen[OverpaymentsScheduledJourney.Answers],
    features: Option[OverpaymentsScheduledJourney.Features] = None
  ): Gen[OverpaymentsScheduledJourney] =
    answersGen.map(
      OverpaymentsScheduledJourney
        .tryBuildFrom(_, features)
        .fold(e => throw new Exception(e), identity)
    )

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
    forcedTaxCodes: Seq[TaxCode] = Seq.empty
  ): Gen[OverpaymentsScheduledJourney.Answers] =
    for {
      userEoriNumber   <- IdGen.genEori
      mrn              <- IdGen.genMRN
      declarantEORI    <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI    <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      numberOfTaxCodes <- Gen.choose(1, 5)
      taxCodes         <- Gen.pick(numberOfTaxCodes, taxCodes).map(_ ++ forcedTaxCodes)
      paidAmounts      <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      consigneeContact <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact <- Gen.option(Acc14Gen.genContactDetails)
    } yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }.toSeq

      val displayDeclaration: DisplayDeclaration          =
        buildDisplayDeclaration(
          mrn.value,
          declarantEORI,
          if (hasConsigneeDetailsInACC14) Some(consigneeEORI) else None,
          paidDuties,
          consigneeContact = if (submitConsigneeDetails) consigneeContact else None,
          declarantContact = declarantContact
        )

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if (submitConsigneeDetails && !hasMatchingEori) {
          if (submitDeclarantDetails)
            Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else
            Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      OverpaymentsScheduledJourney.Answers(
        nonce = Nonce.random,
        userEoriNumber = userEoriNumber,
        movementReferenceNumber = Some(mrn),
        displayDeclaration = Some(displayDeclaration),
        eoriNumbersVerification = eoriNumbersVerification,
        contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
        contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
        checkYourAnswersChangeMode = false
      )
    }

  def answersWithDutiesSelectedGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    taxCodes: Seq[TaxCode] = TaxCodes.all,
    forcedTaxCodes: Seq[TaxCode] = Seq.empty
  ): Gen[OverpaymentsScheduledJourney.Answers] =
    for {
      userEoriNumber   <- IdGen.genEori
      mrn              <- IdGen.genMRN
      declarantEORI    <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI    <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      reimbursements   <- dutyTypesWithTaxCodesGen
      basisOfClaim     <- Gen.oneOf(BasisOfOverpaymentClaim.values - BasisOfOverpaymentClaim.DuplicateEntry)
      numberOfTaxCodes <- Gen.choose(1, 5)
      taxCodes         <- Gen
                            .pick(
                              numberOfTaxCodes,
                              if (basisOfClaim === BasisOfOverpaymentClaim.IncorrectExciseValue) TaxCodes.excise else taxCodes
                            )
                            .map(_ ++ forcedTaxCodes)
      paidAmounts      <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      consigneeContact <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact <- Gen.option(Acc14Gen.genContactDetails)
    } yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, Random.nextBoolean()) }.toSeq

      val correctedAmounts                                =
        SortedMap(reimbursements: _*).view
          .mapValues(s =>
            SortedMap(s.map { taxCode =>
              (taxCode, None)
            }: _*)
          )
          .to(SortedMap)

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

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if (submitConsigneeDetails && !hasMatchingEori) {
          if (submitDeclarantDetails)
            Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else
            Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      val answers =
        OverpaymentsScheduledJourney.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          displayDeclaration = Some(displayDeclaration),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
          contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          additionalDetails = Some("additional details"),
          correctedAmounts = Some(correctedAmounts),
          checkYourAnswersChangeMode = false
        )

      answers
    }
}
