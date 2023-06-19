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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EoriNumbersVerification

/** A collection of generators supporting the tests of RejectedGoodsSingleJourney. */
object RejectedGoodsSingleJourneyGenerators extends JourneyGenerators with JourneyTestData {

  val emptyJourney: RejectedGoodsSingleJourney =
    RejectedGoodsSingleJourney.empty(exampleEori)

  val journeyWithMrnAndDeclaration: RejectedGoodsSingleJourney =
    RejectedGoodsSingleJourney
      .empty(exampleDisplayDeclaration.getDeclarantEori, Nonce.random)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
      .getOrFail

  val completeJourneyWithMatchingUserEoriAndCMAEligibleGen: Gen[RejectedGoodsSingleJourney] =
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

  val completeJourneyWithMatchingUserEoriAndNotCMAEligibleGen: Gen[RejectedGoodsSingleJourney] =
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

  val completeJourneyWithNonNatchingUserEoriAndCMAEligibleGen: Gen[RejectedGoodsSingleJourney] =
    buildCompleteJourneyGen(
      allDutiesCmaEligible = true,
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeJourneyWithNonNatchingUserEoriAndNotCMAEligibleGen: Gen[RejectedGoodsSingleJourney] =
    buildCompleteJourneyGen(
      allDutiesCmaEligible = false,
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeJourneyCMAEligibleGen: Gen[RejectedGoodsSingleJourney] =
    Gen.oneOf(
      completeJourneyWithMatchingUserEoriAndCMAEligibleGen,
      completeJourneyWithNonNatchingUserEoriAndCMAEligibleGen
    )

  val completeJourneyNotCMAEligibleGen: Gen[RejectedGoodsSingleJourney] =
    Gen.oneOf(
      completeJourneyWithMatchingUserEoriAndNotCMAEligibleGen,
      completeJourneyWithNonNatchingUserEoriAndNotCMAEligibleGen
    )

  val completeJourneyGen: Gen[RejectedGoodsSingleJourney] =
    Gen.oneOf(
      completeJourneyCMAEligibleGen,
      completeJourneyNotCMAEligibleGen
    )

  val completeJourneyWithOnlySubsidiesGen: Gen[RejectedGoodsSingleJourney] =
    buildCompleteJourneyGen(
      allDutiesCmaEligible = false,
      generateSubsidyPayments = GenerateSubsidyPayments.All,
      acc14ConsigneeMatchesUserEori = true,
      acc14DeclarantMatchesUserEori = false,
      submitBankAccountDetails = false,
      submitBankAccountType = false,
      features = Some(RejectedGoodsSingleJourney.Features(false, true))
    )

  val completeJourneyWithSomeSubsidiesGen: Gen[RejectedGoodsSingleJourney] =
    buildCompleteJourneyGen(
      allDutiesCmaEligible = false,
      generateSubsidyPayments = GenerateSubsidyPayments.Some,
      acc14ConsigneeMatchesUserEori = true,
      acc14DeclarantMatchesUserEori = false,
      submitBankAccountDetails = false,
      submitBankAccountType = false,
      features = Some(RejectedGoodsSingleJourney.Features(false, true))
    )

  val completeJourneyGenWithoutSpecialCircumstances: Gen[RejectedGoodsSingleJourney] = for {
    journey      <- completeJourneyGen
    basisOfClaim <- Gen.oneOf(BasisOfRejectedGoodsClaim.values - BasisOfRejectedGoodsClaim.SpecialCircumstances)
  } yield journey.submitBasisOfClaim(basisOfClaim)

  val completeJourneyGenWithSpecialCircumstances: Gen[RejectedGoodsSingleJourney] = for {
    journey                          <- completeJourneyGen
    basisOfClaimSpecialCircumstances <- genStringWithMaxSizeOfN(500)
  } yield journey
    .submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)
    .submitBasisOfClaimSpecialCircumstancesDetails(basisOfClaimSpecialCircumstances)
    .fold(error => throw new Exception(error), identity)

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
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[RejectedGoodsSingleJourney.Features] = None
  ): Gen[RejectedGoodsSingleJourney] =
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
      generateSubsidyPayments = generateSubsidyPayments,
      features = features
    ).map(
      _.fold(
        error =>
          throw new Exception(
            s"Cannnot build complete RejectedGoodsSingleJourney because of $error, fix the test data generator."
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
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[RejectedGoodsSingleJourney.Features] = None
  ): Gen[Either[String, RejectedGoodsSingleJourney]] =
    for {
      userEoriNumber              <- IdGen.genEori
      mrn                         <- IdGen.genMRN
      declarantEORI               <- if (acc14DeclarantMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if (acc14ConsigneeMatchesUserEori) Gen.const(userEoriNumber) else IdGen.genEori
      numberOfTaxCodes            <- Gen.choose(if (generateSubsidyPayments == GenerateSubsidyPayments.Some) 2 else 1, 5)
      taxCodes                    <- Gen.pick(numberOfTaxCodes, TaxCodes.all)
      paidAmounts                 <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      reimbursementAmount         <-
        Gen.sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts.map(a => Gen.choose(BigDecimal.exact("0.01"), a))
        )
      basisOfClaim                <- Gen.oneOf(BasisOfRejectedGoodsClaim.values)
      methodOfDisposal            <- Gen.oneOf(MethodOfDisposal.values)
      reimbursementMethod         <-
        reimbursementMethod.map(Gen.const).getOrElse(Gen.oneOf(ReimbursementMethod.nonSubsidyValues))
      numberOfSelectedTaxCodes    <- Gen.choose(1, numberOfTaxCodes)
      numberOfSupportingEvidences <- Gen.choose(0, 3)
      numberOfDocumentTypes       <- Gen.choose(1, 2)
      documentTypes               <-
        Gen.listOfN(numberOfDocumentTypes, Gen.oneOf(UploadDocumentType.rejectedGoodsSingleDocumentTypes))
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

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)]       =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }.toSeq

      val reimbursementClaims: Map[TaxCode, Option[BigDecimal]] =
        taxCodes
          .take(numberOfSelectedTaxCodes)
          .zip(reimbursementAmount)
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
          declarantContact = declarantContact,
          generateSubsidyPayments = generateSubsidyPayments
        )

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
        RejectedGoodsSingleJourney.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          displayDeclaration = Some(displayDeclaration),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if (submitContactDetails) Some(exampleContactDetails) else None,
          contactAddress = if (submitContactAddress) Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          basisOfClaimSpecialCircumstances =
            if (basisOfClaim === BasisOfRejectedGoodsClaim.SpecialCircumstances) Some("special circumstances details")
            else None,
          methodOfDisposal = Some(methodOfDisposal),
          detailsOfRejectedGoods = Some("rejected goods details"),
          reimbursementClaims = Some(reimbursementClaims),
          inspectionDate = Some(exampleInspectionDate),
          inspectionAddress = Some(exampleInspectionAddress),
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

      RejectedGoodsSingleJourney.tryBuildFrom(answers, features)
    }

}
