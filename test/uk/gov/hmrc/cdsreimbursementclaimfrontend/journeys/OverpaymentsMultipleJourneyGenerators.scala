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

import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.OrderedMap

import scala.jdk.CollectionConverters._
import scala.util.Random
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EoriNumbersVerification

/** A collection of generators supporting the tests of OverpaymentsMultipleJourney. */
object OverpaymentsMultipleJourneyGenerators extends JourneyGenerators with JourneyTestData {

  val emptyJourney: OverpaymentsMultipleJourney =
    OverpaymentsMultipleJourney.empty(exampleEori)

  val journeyWithMrnAndDeclaration: OverpaymentsMultipleJourney =
    OverpaymentsMultipleJourney
      .empty(exampleDisplayDeclaration.getDeclarantEori, Nonce.random)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
      .getOrFail

  val completeJourneyWithMatchingUserEoriGen: Gen[OverpaymentsMultipleJourney] =
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

  val completeJourneyWithNonNatchingUserEoriGen: Gen[OverpaymentsMultipleJourney] =
    buildCompleteJourneyGen(
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeJourneyGen: Gen[OverpaymentsMultipleJourney] =
    Gen.oneOf(
      completeJourneyWithMatchingUserEoriGen,
      completeJourneyWithNonNatchingUserEoriGen
    )

  val completeJourneyNotGen: Gen[OverpaymentsMultipleJourney] =
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
    minNumberOfMRNs: Int = 2,
    maxNumberOfMRNs: Int = 6,
    maxSize: Int = 5
  ): Gen[OverpaymentsMultipleJourney] =
    buildJourneyGen(
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
      maxSize = maxSize
    ).map(
      _.fold(
        error =>
          throw new Exception(
            s"Cannnot build complete OverpaymentsMultipleJourney because of $error, fix the test data generator."
          ),
        identity
      )
    )

  type TaxCodesAndAmounts = (Seq[TaxCode], Seq[TaxCode], List[BigDecimal], Seq[BigDecimal])

  def taxCodesAndAmountsGen(maxSize: Int): Gen[TaxCodesAndAmounts] = for {
    numberOfTaxCodes         <- Gen.choose(1, maxSize)
    numberOfSelectedTaxCodes <- Gen.choose(1, numberOfTaxCodes)
    taxCodes                 <- Gen.pick(numberOfTaxCodes, TaxCodes.all)
    paidAmounts              <- Gen.listOfN(numberOfTaxCodes, amountNumberGen)
    correctedAmounts         <-
      Gen
        .sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts
            .take(numberOfSelectedTaxCodes)
            .map(a => Gen.choose(BigDecimal("0.00"), a - BigDecimal.exact("0.01")))
        )
  } yield (taxCodes.toSeq, taxCodes.take(numberOfSelectedTaxCodes).toSeq, paidAmounts, correctedAmounts)

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
    minNumberOfMRNs: Int = 2,
    maxNumberOfMRNs: Int = 6,
    maxSize: Int = 5
  ): Gen[Either[String, OverpaymentsMultipleJourney]] =
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
      maxSize
    )
      .map(OverpaymentsMultipleJourney.tryBuildFrom(_))

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
    maxSize: Int = 5
  ): Gen[OverpaymentsMultipleJourney.Answers] =
    for {
      userEoriNumber              <- IdGen.genEori
      numberOfMRNs                <- Gen.choose(minNumberOfMRNs, Math.max(minNumberOfMRNs, maxNumberOfMRNs))
      mrns                        <- Gen.listOfN(numberOfMRNs, IdGen.genMRN)
      declarantEORI               <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      taxCodesWithAmounts         <- Gen.sequence(mrns.map(_ => taxCodesAndAmountsGen(maxSize))).map(_.asScala)
      basisOfClaim                <- Gen.oneOf(BasisOfOverpaymentClaim.values)
      whetherNorthernIreland      <- Gen.oneOf(true, false)
      numberOfSupportingEvidences <- Gen.choose(0, 3)
      numberOfDocumentTypes       <- Gen.choose(1, 2)
      documentTypes               <-
        Gen.listOfN(numberOfDocumentTypes, Gen.oneOf(UploadDocumentType.overpaymentsMultipleDocumentTypes))
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
            if (hasConsigneeDetailsInACC14) Some(consigneeEORI) else None,
            paidDutiesPerMrn,
            consigneeContact = if (submitConsigneeDetails) consigneeContact else None,
            declarantContact = declarantContact
          )
        }

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val supportingEvidencesExpanded: Seq[UploadedFile] =
        supportingEvidences.flatMap { case (documentType, size) =>
          (0 until size).map(i => buildUploadDocument(s"$i").copy(cargo = Some(documentType)))
        }.toSeq

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if (submitConsigneeDetails && !hasMatchingEori) {
          if (submitDeclarantDetails)
            Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else
            Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      val answers =
        OverpaymentsMultipleJourney.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumbers = Some(mrns),
          displayDeclarations = Some(displayDeclarations),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
          contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          whetherNorthernIreland = Some(whetherNorthernIreland),
          additionalDetails = Some("additional details"),
          correctedAmounts = Some(OrderedMap(correctedAmounts)),
          selectedDocumentType = None,
          supportingEvidences = supportingEvidencesExpanded,
          bankAccountDetails = if (submitBankAccountDetails) Some(exampleBankAccountDetails) else None,
          bankAccountType = if (submitBankAccountType) Some(bankAccountType) else None,
          checkYourAnswersChangeMode = true
        )

      answers
    }

  def buildJourneyGen(answersGen: Gen[OverpaymentsMultipleJourney.Answers]): Gen[OverpaymentsMultipleJourney] =
    answersGen.map(
      OverpaymentsMultipleJourney
        .tryBuildFrom(_)
        .fold(e => throw new Exception(e), identity)
    )

  def incompleteJourneyWithMrnsGen(n: Int): Gen[(OverpaymentsMultipleJourney, Seq[MRN])] = {
    def submitData(journey: OverpaymentsMultipleJourney)(data: ((MRN, DisplayDeclaration), Int)) =
      journey.submitMovementReferenceNumberAndDeclaration(data._2, data._1._1, data._1._2)

    listOfExactlyN(n, mrnWithDisplayDeclarationGen).map { data =>
      val dataWithIndex: List[((MRN, DisplayDeclaration), Int)] = data.zipWithIndex
      (
        emptyJourney
          .flatMapEach(dataWithIndex, submitData)
          .getOrFail,
        data.map(_._1)
      )
    }
  }

  private def mrnWithSelectedTaxCodesGen(journey: OverpaymentsMultipleJourney): Seq[Gen[(MRN, Seq[TaxCode])]] =
    journey.answers.movementReferenceNumbers.get.map { mrn =>
      val availableTaxCodes = journey.getAvailableDuties(mrn).map(_._1)
      Gen
        .choose(1, availableTaxCodes.size)
        .map(availableTaxCodes.take)
        .map(seq => (mrn, seq))
    }

  def incompleteJourneyWithSelectedDutiesGen(n: Int): Gen[(OverpaymentsMultipleJourney, Seq[MRN])] = {
    def submitData(journey: OverpaymentsMultipleJourney)(data: (MRN, Seq[TaxCode])) =
      journey.selectAndReplaceTaxCodeSetForReimbursement(data._1, data._2)

    incompleteJourneyWithMrnsGen(n).flatMap { case (journey, _) =>
      val gen = mrnWithSelectedTaxCodesGen(journey)
      Gen
        .sequence[Seq[(MRN, Seq[TaxCode])], (MRN, Seq[TaxCode])](gen)
        .map { mrnsWithTaxCodesSelection =>
          val modifiedJourney = journey
            .flatMapEach(mrnsWithTaxCodesSelection, submitData)
            .getOrFail
          (
            modifiedJourney,
            modifiedJourney.answers.movementReferenceNumbers.get
          )
        }
    }
  }

  def incompleteJourneyWithCompleteClaimsGen(n: Int): Gen[(OverpaymentsMultipleJourney, Seq[MRN])] = {
    def submitData(journey: OverpaymentsMultipleJourney)(data: (MRN, TaxCode, BigDecimal)) =
      journey.submitCorrectAmount(data._1, data._2, data._3)

    incompleteJourneyWithSelectedDutiesGen(n).map { case (journey, mrns) =>
      val data: Seq[(MRN, TaxCode, BigDecimal)] = mrns.flatMap { mrn =>
        journey.getSelectedDuties(mrn).get.map { taxCode =>
          (mrn, taxCode, BigDecimal(formatAmount(journey.getAmountPaidFor(mrn, taxCode).get / 2)))
        }
      }
      (journey.flatMapEach(data, submitData).getOrFail, mrns)
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
    forcedTaxCodes: Seq[TaxCode] = Seq.empty
  ): Gen[OverpaymentsMultipleJourney.Answers] =
    for {
      userEoriNumber   <- IdGen.genEori
      mrns             <- Gen.listOfN(3, IdGen.genMRN)
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

      val displayDeclarations: Seq[DisplayDeclaration]    = mrns.map { mrn =>
        buildDisplayDeclaration(
          mrn.value,
          declarantEORI,
          if (hasConsigneeDetailsInACC14) Some(consigneeEORI) else None,
          paidDuties,
          consigneeContact = if (submitConsigneeDetails) consigneeContact else None,
          declarantContact = declarantContact
        )
      }

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if (submitConsigneeDetails && !hasMatchingEori) {
          if (submitDeclarantDetails)
            Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else
            Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      OverpaymentsMultipleJourney.Answers(
        nonce = Nonce.random,
        userEoriNumber = userEoriNumber,
        movementReferenceNumbers = Some(mrns),
        displayDeclarations = Some(displayDeclarations),
        eoriNumbersVerification = eoriNumbersVerification,
        contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
        contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
        checkYourAnswersChangeMode = false
      )
    }

}
