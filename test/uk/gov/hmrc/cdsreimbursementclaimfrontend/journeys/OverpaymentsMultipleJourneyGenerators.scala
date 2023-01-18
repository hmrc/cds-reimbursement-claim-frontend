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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.OrderedMap

import scala.collection.JavaConverters._

/** A collection of generators supporting the tests of OverpaymentsMultipleJourney. */
object OverpaymentsMultipleJourneyGenerators extends JourneyGenerators with JourneyTestData {

  val emptyJourney: OverpaymentsMultipleJourney =
    OverpaymentsMultipleJourney.empty(exampleEori)

  val journeyWithMrnAndDeclaration: OverpaymentsMultipleJourney =
    OverpaymentsMultipleJourney
      .empty(exampleDisplayDeclaration.getDeclarantEori, Nonce.random)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
      .getOrFail

  val completeJourneyWithMatchingUserEoriAndCMAEligibleGen: Gen[OverpaymentsMultipleJourney] =
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

  val completeJourneyWithMatchingUserEoriAndNotCMAEligibleGen: Gen[OverpaymentsMultipleJourney] =
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

  val completeJourneyWithNonNatchingUserEoriAndCMAEligibleGen: Gen[OverpaymentsMultipleJourney] =
    buildCompleteJourneyGen(
      allDutiesCmaEligible = true,
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeJourneyWithNonNatchingUserEoriAndNotCMAEligibleGen: Gen[OverpaymentsMultipleJourney] =
    buildCompleteJourneyGen(
      allDutiesCmaEligible = false,
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeJourneyCMAEligibleGen: Gen[OverpaymentsMultipleJourney] =
    Gen.oneOf(
      completeJourneyWithMatchingUserEoriAndCMAEligibleGen,
      completeJourneyWithNonNatchingUserEoriAndCMAEligibleGen
    )

  val completeJourneyNotCMAEligibleGen: Gen[OverpaymentsMultipleJourney] =
    Gen.oneOf(
      completeJourneyWithMatchingUserEoriAndNotCMAEligibleGen,
      completeJourneyWithNonNatchingUserEoriAndNotCMAEligibleGen
    )

  val completeJourneyGen: Gen[OverpaymentsMultipleJourney] =
    Gen.oneOf(
      completeJourneyCMAEligibleGen,
      completeJourneyNotCMAEligibleGen
    )

  val completeJourneyGenWithoutDuplicateEntry: Gen[OverpaymentsMultipleJourney] = for {
    journey      <- completeJourneyGen
    basisOfClaim <- Gen.oneOf(BasisOfOverpaymentClaim.values - BasisOfOverpaymentClaim.DuplicateEntry)
  } yield journey.submitBasisOfClaim(basisOfClaim)

  val completeJourneyGenWithDuplicateEntry: Gen[OverpaymentsMultipleJourney] = for {
    journey <- completeJourneyGen
  } yield journey
    .submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry)
  //.fold(error => throw new Exception(error), identity)

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
    minNumberOfMRNs: Int = 2,
    maxNumberOfMRNs: Int = 6,
    maxSize: Int = 5,
    reimbursementMethod: Option[ReimbursementMethod] = None
  ): Gen[OverpaymentsMultipleJourney] =
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
      minNumberOfMRNs = minNumberOfMRNs,
      maxNumberOfMRNs = maxNumberOfMRNs,
      maxSize = maxSize,
      reimbursementMethod = reimbursementMethod
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
    reimbursementAmounts     <-
      Gen
        .sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts.take(numberOfSelectedTaxCodes).map(a => Gen.choose(BigDecimal.exact("0.01"), a))
        )
  } yield (taxCodes, taxCodes.take(numberOfSelectedTaxCodes), paidAmounts, reimbursementAmounts)

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
    minNumberOfMRNs: Int = 2,
    maxNumberOfMRNs: Int = 6,
    maxSize: Int = 5,
    reimbursementMethod: Option[ReimbursementMethod] = None
  ): Gen[Either[String, OverpaymentsMultipleJourney]] =
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
      minNumberOfMRNs,
      maxNumberOfMRNs,
      maxSize,
      reimbursementMethod
    )
      .map(OverpaymentsMultipleJourney.tryBuildFrom(_))

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
    minNumberOfMRNs: Int = 2,
    maxNumberOfMRNs: Int = 6,
    maxSize: Int = 5,
    reimbursementMethod: Option[ReimbursementMethod] = None
  ): Gen[OverpaymentsMultipleJourney.Answers] =
    for {
      userEoriNumber              <- IdGen.genEori
      numberOfMRNs                <- Gen.choose(minNumberOfMRNs, Math.max(minNumberOfMRNs, maxNumberOfMRNs))
      mrns                        <- Gen.listOfN(numberOfMRNs, IdGen.genMRN)
      declarantEORI               <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      numberOfTaxCodes            <- Gen.choose(1, 5)
      taxCodesWithAmounts         <- Gen.sequence(mrns.map(_ => taxCodesAndAmountsGen(maxSize))).map(_.asScala)
      paidAmounts                 <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      reimbursementAmount         <-
        Gen.sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts.map(a => Gen.choose(BigDecimal.exact("0.01"), a))
        )
      basisOfClaim                <- Gen.oneOf(BasisOfOverpaymentClaim.values)
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

      val paidDuties: Seq[(MRN, Seq[(TaxCode, BigDecimal, Boolean)])] =
        mrns.zip(taxCodesWithAmounts).map { case (mrn, (taxCodes, _, paidAmounts, _)) =>
          (mrn, taxCodes.zip(paidAmounts).map { case (t, r) => (t, r, allDutiesCmaEligible) })
        }

      val reimbursementClaims: OrderedMap[MRN, Map[TaxCode, Option[BigDecimal]]] =
        OrderedMap(
          mrns
            .zip(taxCodesWithAmounts)
            .map { case (mrn, (_, selectedTaxCodes, _, reimbursementAmounts)) =>
              (
                mrn,
                selectedTaxCodes
                  .zip(reimbursementAmounts)
                  .map { case (taxCode, amount) => (taxCode, Option(amount)) }
                  .toMap
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

      val answers =
        OverpaymentsMultipleJourney.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumbers = Some(mrns),
          displayDeclarations = Some(displayDeclarations),
          consigneeEoriNumber = if (submitConsigneeDetails && !hasMatchingEori) Some(consigneeEORI) else None,
          declarantEoriNumber = if (submitDeclarantDetails && !hasMatchingEori) Some(declarantEORI) else None,
          contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
          contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          whetherNorthernIreland = Some(whetherNorthernIreland),
          additionalDetails = Some("additional details"),
          reimbursementClaims = Some(OrderedMap(reimbursementClaims)),
          selectedDocumentType = None,
          supportingEvidences = supportingEvidencesExpanded,
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
          reimbursementMethod = if (allDutiesCmaEligible) Some(reimbursementMethod) else None,
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

  def answersUpToBasisForClaimGen(
    acc14DeclarantMatchesUserEori: Boolean = true,
    acc14ConsigneeMatchesUserEori: Boolean = true,
    allDutiesCmaEligible: Boolean = true,
    hasConsigneeDetailsInACC14: Boolean = true,
    submitDeclarantDetails: Boolean = true,
    submitConsigneeDetails: Boolean = true,
    submitContactDetails: Boolean = true,
    submitContactAddress: Boolean = true
  ): Gen[OverpaymentsMultipleJourney.Answers] =
    for {
      userEoriNumber   <- IdGen.genEori
      mrns             <- Gen.listOfN(3, IdGen.genMRN)
      declarantEORI    <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI    <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      numberOfTaxCodes <- Gen.choose(1, 5)
      taxCodes         <- Gen.pick(numberOfTaxCodes, TaxCodes.all)
      paidAmounts      <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      consigneeContact <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact <- Gen.option(Acc14Gen.genContactDetails)
    } yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }

      val displayDeclarations: Seq[DisplayDeclaration]    =
        mrns.map(mrn =>
          buildDisplayDeclaration(
            mrn.value,
            declarantEORI,
            if (hasConsigneeDetailsInACC14) Some(consigneeEORI) else None,
            paidDuties,
            consigneeContact = if (submitConsigneeDetails) consigneeContact else None,
            declarantContact = declarantContact
          )
        )

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      OverpaymentsMultipleJourney.Answers(
        nonce = Nonce.random,
        userEoriNumber = userEoriNumber,
        movementReferenceNumbers = Some(mrns),
        displayDeclarations = Some(displayDeclarations),
        consigneeEoriNumber = if (submitConsigneeDetails && !hasMatchingEori) Some(consigneeEORI) else None,
        declarantEoriNumber = if (submitDeclarantDetails && !hasMatchingEori) Some(declarantEORI) else None,
        contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
        contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
        checkYourAnswersChangeMode = false
      )
    }

}
