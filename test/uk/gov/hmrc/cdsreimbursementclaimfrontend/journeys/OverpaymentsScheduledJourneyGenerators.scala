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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BankAccountType, BasisOfOverpaymentClaim, Nonce, ReimbursementMethod, TaxCode, TaxCodes, UploadedFile}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType

import scala.collection.immutable.SortedMap

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

  val completeJourneyWithMatchingUserEoriAndCMAEligibleGen: Gen[OverpaymentsScheduledJourney] =
    Gen.oneOf(
      buildCompleteJourneyGen(
        allDutiesCmaEligible = true,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = false
      ),
      buildCompleteJourneyGen(
        allDutiesCmaEligible = true,
        acc14ConsigneeMatchesUserEori = false,
        acc14DeclarantMatchesUserEori = true
      ),
      buildCompleteJourneyGen(
        allDutiesCmaEligible = true,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = true
      )
    )

  val completeJourneyWithMatchingUserEoriAndNotCMAEligibleGen: Gen[OverpaymentsScheduledJourney] =
    Gen.oneOf(
      buildCompleteJourneyGen(
        allDutiesCmaEligible = false,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = false
      ),
      buildCompleteJourneyGen(
        allDutiesCmaEligible = false,
        acc14ConsigneeMatchesUserEori = false,
        acc14DeclarantMatchesUserEori = true
      ),
      buildCompleteJourneyGen(
        allDutiesCmaEligible = false,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = true
      )
    )

  val completeJourneyWithNonNatchingUserEoriAndCMAEligibleGen: Gen[OverpaymentsScheduledJourney] =
    buildCompleteJourneyGen(
      allDutiesCmaEligible = true,
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeJourneyWithNonNatchingUserEoriAndNotCMAEligibleGen: Gen[OverpaymentsScheduledJourney] =
    buildCompleteJourneyGen(
      allDutiesCmaEligible = false,
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeJourneyCMAEligibleGen: Gen[OverpaymentsScheduledJourney] =
    Gen.oneOf(
      completeJourneyWithMatchingUserEoriAndCMAEligibleGen,
      completeJourneyWithNonNatchingUserEoriAndCMAEligibleGen
    )

  val completeJourneyNotCMAEligibleGen: Gen[OverpaymentsScheduledJourney] =
    Gen.oneOf(
      completeJourneyWithMatchingUserEoriAndNotCMAEligibleGen,
      completeJourneyWithNonNatchingUserEoriAndNotCMAEligibleGen
    )

  val completeJourneyGen: Gen[OverpaymentsScheduledJourney] =
    Gen.oneOf(
      completeJourneyCMAEligibleGen,
      completeJourneyNotCMAEligibleGen
    )

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def buildCompleteJourneyGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    allDutiesCmaEligible: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    reimbursementMethod: Option[ReimbursementMethod] = None,
    taxCodes: Seq[TaxCode] = TaxCodes.all
  ): Gen[OverpaymentsScheduledJourney] =
    buildJourneyGen(
      acc14DeclarantMatchesUserEori,
      acc14ConsigneeMatchesUserEori,
      allDutiesCmaEligible,
      hasConsigneeDetailsInACC14,
      submitConsigneeDetails = submitConsigneeDetails,
      submitContactDetails = submitContactDetails,
      submitContactAddress = submitContactAddress,
      submitBankAccountType = submitBankAccountType,
      submitBankAccountDetails = submitBankAccountDetails,
      reimbursementMethod = reimbursementMethod,
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

  def buildJourneyGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    allDutiesCmaEligible: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    reimbursementMethod: Option[ReimbursementMethod] = None,
    taxCodes: Seq[TaxCode] = TaxCodes.all
  ): Gen[Either[String, OverpaymentsScheduledJourney]] =
    buildAnswersGen(
      acc14DeclarantMatchesUserEori,
      acc14ConsigneeMatchesUserEori,
      allDutiesCmaEligible,
      hasConsigneeDetailsInACC14,
      submitDeclarantDetails,
      submitConsigneeDetails,
      submitContactDetails,
      submitContactAddress,
      submitBankAccountDetails,
      submitBankAccountType,
      true,
      true,
      reimbursementMethod,
      taxCodes
    ).map(OverpaymentsScheduledJourney.tryBuildFrom(_))

  def buildAnswersGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    allDutiesCmaEligible: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true,
    submitBankAccountDetails: Boolean = true,
    submitBankAccountType: Boolean = true,
    submitReimbursementMethod: Boolean = true,
    submitEvidence: Boolean = true,
    reimbursementMethod: Option[ReimbursementMethod] = None,
    taxCodes: Seq[TaxCode] = TaxCodes.all,
    forcedTaxCodes: Seq[TaxCode] = Seq.empty,
    checkYourAnswersChangeMode: Boolean = true
  ): Gen[OverpaymentsScheduledJourney.Answers] =
    for {
      userEoriNumber              <- IdGen.genEori
      mrn                         <- IdGen.genMRN
      declarantEORI               <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      basisOfClaim                <- Gen.oneOf(BasisOfOverpaymentClaim.values)
      numberOfTaxCodes            <- Gen.choose(1, 5)
      taxCodes                    <- Gen
                                       .pick(
                                         numberOfTaxCodes,
                                         if (basisOfClaim === BasisOfOverpaymentClaim.IncorrectExciseValue) TaxCodes.excise else taxCodes
                                       )
                                       .map(_ ++ forcedTaxCodes)
      paidAmounts                 <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      correctAmount               <-
        Gen.sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts.map(a => Gen.choose(ZERO, a - BigDecimal("0.01")))
        )
      whetherNorthernIreland      <- Gen.oneOf(true, false)
      reimbursementMethod         <- reimbursementMethod.map(Gen.const).getOrElse(Gen.oneOf(ReimbursementMethod.values))
      numberOfSelectedTaxCodes    <- Gen.choose(1, numberOfTaxCodes)
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

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)]    =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }

      val correctedAmounts: SortedMap[TaxCode, Option[BigDecimal]] = {
        val mapTaxCodes = taxCodes
          .take(numberOfSelectedTaxCodes)
          .zip(correctAmount)
          .map { case (t, a) =>
            (t, Option(a))
          }
        SortedMap[TaxCode, Option[BigDecimal]]() ++ mapTaxCodes
      }



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
          (0 until size).map(i => buildUploadDocument(s"$i").copy(cargo = Some(documentType)))
        }.toSeq

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
          correctedAmounts = Some(correctedAmounts),
          selectedDocumentType = None,
          supportingEvidences =
            if (submitEvidence) supportingEvidencesExpanded
            else Seq.empty,
          bankAccountDetails =
            if (
              submitBankAccountDetails &&
              (!allDutiesCmaEligible || reimbursementMethod === ReimbursementMethod.BankAccountTransfer)
            )
              Some(exampleBankAccountDetails)
            else None,
          bankAccountType =
            if (
              submitBankAccountType &&
              (!allDutiesCmaEligible || reimbursementMethod === ReimbursementMethod.BankAccountTransfer)
            )
              Some(bankAccountType)
            else None,
          reimbursementMethod =
            if (submitReimbursementMethod && allDutiesCmaEligible) Some(reimbursementMethod)
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
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }

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

      OverpaymentsScheduledJourney.Answers(
        nonce = Nonce.random,
        userEoriNumber = userEoriNumber,
        movementReferenceNumber = Some(mrn),
        displayDeclaration = Some(displayDeclaration),
        consigneeEoriNumber = if (submitConsigneeDetails && !hasMatchingEori) Some(consigneeEORI) else None,
        declarantEoriNumber = if (submitDeclarantDetails && !hasMatchingEori) Some(declarantEORI) else None,
        contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
        contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
        checkYourAnswersChangeMode = false
      )
    }

  def answersWithDutiesSelectedGen(
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
      userEoriNumber           <- IdGen.genEori
      mrn                      <- IdGen.genMRN
      declarantEORI            <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI            <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      basisOfClaim             <- Gen.oneOf(BasisOfOverpaymentClaim.values)
      numberOfTaxCodes         <- Gen.choose(1, 5)
      taxCodes                 <- Gen
                                    .pick(
                                      numberOfTaxCodes,
                                      if (basisOfClaim === BasisOfOverpaymentClaim.IncorrectExciseValue) TaxCodes.excise else taxCodes
                                    )
                                    .map(_ ++ forcedTaxCodes)
      paidAmounts              <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      duplicateMrn             <- if (basisOfClaim == BasisOfOverpaymentClaim.DuplicateEntry) IdGen.genMRN.map(Option.apply)
                                  else Gen.const(None)
      whetherNorthernIreland   <- Gen.oneOf(true, false)
      numberOfSelectedTaxCodes <- Gen.choose(1, numberOfTaxCodes)
      consigneeContact         <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact         <- Gen.option(Acc14Gen.genContactDetails)
    } yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)]    =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }

      val correctedAmounts: Map[TaxCode, Option[BigDecimal]] =
        taxCodes
          .take(numberOfSelectedTaxCodes)
          .map { t =>
            (t, None)
          }
          .toMap

      val displayDeclaration: DisplayDeclaration =
        buildDisplayDeclaration(
          mrn.value,
          declarantEORI,
          if (hasConsigneeDetailsInACC14) Some(consigneeEORI) else None,
          paidDuties,
          consigneeContact = if (submitConsigneeDetails) consigneeContact else None,
          declarantContact = declarantContact
        )

      val duplicateDisplayDeclaration: Option[DisplayDeclaration] =
        duplicateMrn.map(mrn => displayDeclaration.withDeclarationId(mrn.value))

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

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
          duplicateDisplayDeclaration = duplicateDisplayDeclaration,
          whetherNorthernIreland = Some(whetherNorthernIreland),
          additionalDetails = Some("additional details"),
          correctedAmounts = Some(correctedAmounts),
          checkYourAnswersChangeMode = false
        )

      answers
    }

  def answersWithAllAmountsProvidedGen(
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
      userEoriNumber           <- IdGen.genEori
      mrn                      <- IdGen.genMRN
      declarantEORI            <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI            <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      basisOfClaim             <- Gen.oneOf(BasisOfOverpaymentClaim.values)
      numberOfTaxCodes         <- Gen.choose(1, 5)
      taxCodes                 <- Gen
                                    .pick(
                                      numberOfTaxCodes,
                                      if (basisOfClaim === BasisOfOverpaymentClaim.IncorrectExciseValue) TaxCodes.excise else taxCodes
                                    )
                                    .map(_ ++ forcedTaxCodes)
      paidAmounts              <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      correctAmount            <-
        Gen.sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts.map(a => Gen.choose(ZERO, a - BigDecimal("0.01")))
        )
      duplicateMrn             <- if (basisOfClaim == BasisOfOverpaymentClaim.DuplicateEntry) IdGen.genMRN.map(Option.apply)
                                  else Gen.const(None)
      whetherNorthernIreland   <- Gen.oneOf(true, false)
      numberOfSelectedTaxCodes <- Gen.choose(1, numberOfTaxCodes)
      consigneeContact         <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact         <- Gen.option(Acc14Gen.genContactDetails)
    } yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)]    =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }

      val correctedAmounts: Map[TaxCode, Option[BigDecimal]] =
        taxCodes
          .take(numberOfSelectedTaxCodes)
          .zip(correctAmount)
          .map { case (t, a) =>
            (t, Option(a))
          }
          .toMap

      val displayDeclaration: DisplayDeclaration =
        buildDisplayDeclaration(
          mrn.value,
          declarantEORI,
          if (hasConsigneeDetailsInACC14) Some(consigneeEORI) else None,
          paidDuties,
          consigneeContact = if (submitConsigneeDetails) consigneeContact else None,
          declarantContact = declarantContact
        )

      val duplicateDisplayDeclaration: Option[DisplayDeclaration] =
        duplicateMrn.map(mrn => displayDeclaration.withDeclarationId(mrn.value))

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

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
          correctedAmounts = Some(correctedAmounts),
          checkYourAnswersChangeMode = false
        )

      answers
    }
}
