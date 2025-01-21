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

import cats.syntax.eq.*
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*

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
      features =
        Some(RejectedGoodsSingleJourney.Features(shouldBlockSubsidies = false, shouldAllowSubsidyOnlyPayments = true))
    )

  val completeJourneyWithSomeSubsidiesGen: Gen[RejectedGoodsSingleJourney] =
    buildCompleteJourneyGen(
      allDutiesCmaEligible = false,
      generateSubsidyPayments = GenerateSubsidyPayments.Some,
      acc14ConsigneeMatchesUserEori = true,
      acc14DeclarantMatchesUserEori = false,
      submitBankAccountDetails = false,
      submitBankAccountType = false,
      features =
        Some(RejectedGoodsSingleJourney.Features(shouldBlockSubsidies = false, shouldAllowSubsidyOnlyPayments = true))
    )

  val completeJourneyGenWithoutSpecialCircumstances: Gen[RejectedGoodsSingleJourney] = for
    journey      <- completeJourneyGen
    basisOfClaim <- Gen.oneOf(BasisOfRejectedGoodsClaim.values - BasisOfRejectedGoodsClaim.SpecialCircumstances)
  yield journey.submitBasisOfClaim(basisOfClaim)

  val completeJourneyGenWithSpecialCircumstances: Gen[RejectedGoodsSingleJourney] = for
    journey                          <- completeJourneyGen
    basisOfClaimSpecialCircumstances <- genStringWithMaxSizeOfN(500)
  yield journey
    .submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)
    .submitBasisOfClaimSpecialCircumstancesDetails(basisOfClaimSpecialCircumstances)
    .fold(error => throw new Exception(error), identity)

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
    features: Option[RejectedGoodsSingleJourney.Features] = None,
    payeeType: PayeeType = PayeeType.Consignee
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
      features = features,
      payeeType = Some(payeeType)
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
    features: Option[RejectedGoodsSingleJourney.Features] = None,
    payeeType: Option[PayeeType] = None
  ): Gen[Either[String, RejectedGoodsSingleJourney]] =
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
      reimbursementMethod,
      generateSubsidyPayments,
      payeeType
    ).map(answers =>
      RejectedGoodsSingleJourney
        .tryBuildFrom(answers, features)
    )

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
    reimbursementMethod: Option[ReimbursementMethod] = None,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    payeeType: Option[PayeeType] = None
  ): Gen[RejectedGoodsSingleJourney.Answers] =
    for
      userEoriNumber              <- IdGen.genEori
      mrn                         <- IdGen.genMRN
      declarantEORI               <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      numberOfTaxCodes            <- Gen.choose(if generateSubsidyPayments == GenerateSubsidyPayments.Some then 2 else 1, 5)
      taxCodes                    <- Gen.pick(numberOfTaxCodes, TaxCodes.all).map(_.distinct)
      paidAmounts                 <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      correctAmount               <-
        Gen.sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts.map(a => Gen.choose(ZERO, a - BigDecimal("0.01")))
        )
      basisOfClaim                <- Gen.oneOf(BasisOfRejectedGoodsClaim.values)
      methodOfDisposal            <- Gen.oneOf(MethodOfDisposal.values)
      reimbursementMethod         <-
        reimbursementMethod.map(Gen.const).getOrElse(Gen.oneOf(ReimbursementMethod.nonSubsidyValues))
      numberOfSelectedTaxCodes    <- Gen.choose(1, numberOfTaxCodes)
      numberOfSupportingEvidences <- Gen.choose(1, 3)
      numberOfDocumentTypes       <- Gen.choose(1, 2)
      documentTypes               <-
        Gen.listOfN(numberOfDocumentTypes, Gen.oneOf(UploadDocumentType.rejectedGoodsSingleDocumentTypes))
      supportingEvidences         <-
        Gen
          .sequence[Seq[(UploadDocumentType, Int)], (UploadDocumentType, Int)](
            documentTypes.map(dt => Gen.choose(1, numberOfSupportingEvidences).map(n => (dt, n)))
          )
          .map(_.toMap)
      bankAccountType             <- Gen.oneOf(BankAccountType.values)
      consigneeContact            <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact            <- Gen.option(Acc14Gen.genContactDetails)
    yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }.toSeq

      val correctedAmounts: Map[TaxCode, Option[ReimbursementClaim]] =
        taxCodes
          .take(numberOfSelectedTaxCodes)
          .zip(correctAmount)
          .map { case (t, a) =>
            (t, Some(DefaultMethodReimbursementClaim(a)))
          }
          .toMap

      val displayDeclaration: DisplayDeclaration =
        buildDisplayDeclaration(
          mrn.value,
          declarantEORI,
          if hasConsigneeDetailsInACC14 then Some(consigneeEORI) else None,
          paidDuties,
          consigneeContact = if submitConsigneeDetails then consigneeContact else None,
          declarantContact = declarantContact,
          generateSubsidyPayments = generateSubsidyPayments
        )

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

      RejectedGoodsSingleJourney.Answers(
        nonce = Nonce.random,
        userEoriNumber = userEoriNumber,
        movementReferenceNumber = Some(mrn),
        displayDeclaration = Some(displayDeclaration),
        payeeType = payeeType,
        eoriNumbersVerification = eoriNumbersVerification,
        contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
        contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
        basisOfClaim = Some(basisOfClaim),
        basisOfClaimSpecialCircumstances =
          if basisOfClaim === BasisOfRejectedGoodsClaim.SpecialCircumstances then Some("special circumstances details")
          else None,
        methodOfDisposal = Some(methodOfDisposal),
        detailsOfRejectedGoods = Some("rejected goods details"),
        correctedAmounts = Some(correctedAmounts),
        inspectionDate = Some(exampleInspectionDate),
        inspectionAddress = Some(exampleInspectionAddress),
        selectedDocumentType = None,
        supportingEvidences = supportingEvidencesExpanded,
        bankAccountDetails =
          if submitBankAccountDetails then Some(exampleBankAccountDetails)
          else None,
        bankAccountType =
          if submitBankAccountType then Some(bankAccountType)
          else None,
        reimbursementMethod = if allDutiesCmaEligible then Some(reimbursementMethod) else None,
        modes = JourneyModes(checkYourAnswersChangeMode = true)
      )
    }

  def buildJourneyFromAnswersGen(
    answersGen: Gen[RejectedGoodsSingleJourney.Answers],
    features: Option[RejectedGoodsSingleJourney.Features] = None
  ): Gen[RejectedGoodsSingleJourney] =
    answersGen.map(
      RejectedGoodsSingleJourney
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
  ): Gen[RejectedGoodsSingleJourney.Answers] =
    for
      userEoriNumber   <- IdGen.genEori
      mrn              <- IdGen.genMRN
      declarantEORI    <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI    <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      numberOfTaxCodes <- Gen.choose(1, 5)
      taxCodes         <- Gen
                            .pick(
                              numberOfTaxCodes,
                              taxCodes
                            )
                            .map(_ ++ forcedTaxCodes)
      paidAmounts      <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      consigneeContact <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact <- Gen.option(Acc14Gen.genContactDetails)
    yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }.toSeq

      val displayDeclaration: DisplayDeclaration =
        buildDisplayDeclaration(
          mrn.value,
          declarantEORI,
          if hasConsigneeDetailsInACC14 then Some(consigneeEORI) else None,
          paidDuties,
          consigneeContact = if submitConsigneeDetails then consigneeContact else None,
          declarantContact = declarantContact
        )

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if submitConsigneeDetails && !hasMatchingEori then {
          if submitDeclarantDetails then Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      val answers =
        RejectedGoodsSingleJourney.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          displayDeclaration = Some(displayDeclaration),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
          contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
          modes = JourneyModes(checkYourAnswersChangeMode = false)
        )

      answers
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
  ): Gen[RejectedGoodsSingleJourney.Answers] =
    for
      userEoriNumber           <- IdGen.genEori
      mrn                      <- IdGen.genMRN
      declarantEORI            <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI            <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      numberOfTaxCodes         <- Gen.choose(1, 5)
      taxCodes                 <- Gen
                                    .pick(
                                      numberOfTaxCodes,
                                      taxCodes
                                    )
                                    .map(_ ++ forcedTaxCodes)
      paidAmounts              <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      basisOfClaim             <- Gen.oneOf(BasisOfRejectedGoodsClaim.values)
      methodOfDisposal         <- Gen.oneOf(MethodOfDisposal.values)
      numberOfSelectedTaxCodes <- Gen.choose(1, numberOfTaxCodes)
      consigneeContact         <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact         <- Gen.option(Acc14Gen.genContactDetails)
    yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }.toSeq

      val correctedAmounts: Map[TaxCode, Option[ReimbursementClaim]] =
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
          if hasConsigneeDetailsInACC14 then Some(consigneeEORI) else None,
          paidDuties,
          consigneeContact = if submitConsigneeDetails then consigneeContact else None,
          declarantContact = declarantContact
        )

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if submitConsigneeDetails && !hasMatchingEori then {
          if submitDeclarantDetails then Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      val answers =
        RejectedGoodsSingleJourney.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          displayDeclaration = Some(displayDeclaration),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
          contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          methodOfDisposal = Some(methodOfDisposal),
          correctedAmounts = Some(correctedAmounts),
          modes = JourneyModes(checkYourAnswersChangeMode = false)
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
  ): Gen[RejectedGoodsSingleJourney.Answers] =
    for
      userEoriNumber           <- IdGen.genEori
      mrn                      <- IdGen.genMRN
      declarantEORI            <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI            <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      numberOfTaxCodes         <- Gen.choose(1, 5)
      taxCodes                 <- Gen
                                    .pick(
                                      numberOfTaxCodes,
                                      taxCodes
                                    )
                                    .map(_ ++ forcedTaxCodes)
      paidAmounts              <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      correctAmount            <-
        Gen.sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts.map(a => Gen.choose(ZERO, a - BigDecimal("0.01")))
        )
      basisOfClaim             <- Gen.oneOf(BasisOfRejectedGoodsClaim.values)
      methodOfDisposal         <- Gen.oneOf(MethodOfDisposal.values)
      numberOfSelectedTaxCodes <- Gen.choose(1, numberOfTaxCodes)
      consigneeContact         <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact         <- Gen.option(Acc14Gen.genContactDetails)
    yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }.toSeq

      val correctedAmounts: Map[TaxCode, Option[ReimbursementClaim]] =
        taxCodes
          .take(numberOfSelectedTaxCodes)
          .zip(correctAmount)
          .map { case (t, a) =>
            (t, Some(DefaultMethodReimbursementClaim(a)))
          }
          .toMap

      val displayDeclaration: DisplayDeclaration =
        buildDisplayDeclaration(
          mrn.value,
          declarantEORI,
          if hasConsigneeDetailsInACC14 then Some(consigneeEORI) else None,
          paidDuties,
          consigneeContact = if submitConsigneeDetails then consigneeContact else None,
          declarantContact = declarantContact
        )

      val hasMatchingEori = acc14DeclarantMatchesUserEori || acc14ConsigneeMatchesUserEori

      val eoriNumbersVerification: Option[EoriNumbersVerification] =
        if submitConsigneeDetails && !hasMatchingEori then {
          if submitDeclarantDetails then Some(EoriNumbersVerification(Some(consigneeEORI), Some(declarantEORI)))
          else Some(EoriNumbersVerification(Some(consigneeEORI)))
        } else None

      val answers =
        RejectedGoodsSingleJourney.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          displayDeclaration = Some(displayDeclaration),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
          contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          methodOfDisposal = Some(methodOfDisposal),
          correctedAmounts = Some(correctedAmounts),
          modes = JourneyModes(checkYourAnswersChangeMode = false)
        )

      answers
    }

}
