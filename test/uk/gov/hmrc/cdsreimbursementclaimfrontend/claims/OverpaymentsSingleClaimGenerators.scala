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

import cats.syntax.eq.*
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectEoriAndDan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Dan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori

/** A collection of generators supporting the tests of OverpaymentsSingleClaim. */
object OverpaymentsSingleClaimGenerators extends ClaimGenerators with ClaimTestData {

  val emptyClaim: OverpaymentsSingleClaim =
    OverpaymentsSingleClaim.empty(exampleEori)

  val claimWithMrnAndDeclaration: OverpaymentsSingleClaim =
    OverpaymentsSingleClaim
      .empty(exampleImportDeclaration.getDeclarantEori, Nonce.random)
      .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleImportDeclaration)
      .getOrFail

  val completeClaimWithMatchingUserEoriAndCMAEligibleGen: Gen[OverpaymentsSingleClaim] =
    Gen.oneOf(
      buildCompleteClaimGen(
        allDutiesCmaEligible = true,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = false
      ),
      buildCompleteClaimGen(
        allDutiesCmaEligible = true,
        acc14ConsigneeMatchesUserEori = false,
        acc14DeclarantMatchesUserEori = true
      ),
      buildCompleteClaimGen(
        allDutiesCmaEligible = true,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = true
      )
    )

  val completeClaimWithMatchingUserEoriAndNotCMAEligibleGen: Gen[OverpaymentsSingleClaim] =
    Gen.oneOf(
      buildCompleteClaimGen(
        allDutiesCmaEligible = false,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = false
      ),
      buildCompleteClaimGen(
        allDutiesCmaEligible = false,
        acc14ConsigneeMatchesUserEori = false,
        acc14DeclarantMatchesUserEori = true
      ),
      buildCompleteClaimGen(
        allDutiesCmaEligible = false,
        acc14ConsigneeMatchesUserEori = true,
        acc14DeclarantMatchesUserEori = true
      )
    )

  val completeClaimWithNonNatchingUserEoriAndCMAEligibleGen: Gen[OverpaymentsSingleClaim] =
    buildCompleteClaimGen(
      allDutiesCmaEligible = true,
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeClaimWithNonNatchingUserEoriAndNotCMAEligibleGen: Gen[OverpaymentsSingleClaim] =
    buildCompleteClaimGen(
      allDutiesCmaEligible = false,
      acc14DeclarantMatchesUserEori = false,
      acc14ConsigneeMatchesUserEori = false
    )

  val completeClaimCMAEligibleGen: Gen[OverpaymentsSingleClaim] =
    Gen.oneOf(
      completeClaimWithMatchingUserEoriAndCMAEligibleGen,
      completeClaimWithNonNatchingUserEoriAndCMAEligibleGen
    )

  val completeClaimNotCMAEligibleGen: Gen[OverpaymentsSingleClaim] =
    Gen.oneOf(
      completeClaimWithMatchingUserEoriAndNotCMAEligibleGen,
      completeClaimWithNonNatchingUserEoriAndNotCMAEligibleGen
    )

  val completeClaimGen: Gen[OverpaymentsSingleClaim] =
    Gen.oneOf(
      completeClaimCMAEligibleGen,
      completeClaimNotCMAEligibleGen
    )

  val completeClaimGenWithoutDuplicateEntryAndIncorrectExciseValue: Gen[OverpaymentsSingleClaim] = for
    claim        <- completeClaimGen
    basisOfClaim <-
      Gen.oneOf(
        BasisOfOverpaymentClaim.values - BasisOfOverpaymentClaim.DuplicateEntry - BasisOfOverpaymentClaim.IncorrectExciseValue
      )
  yield claim.submitBasisOfClaim(basisOfClaim)

  val completeClaimGenWithDuplicateEntry: Gen[OverpaymentsSingleClaim] =
    for claim <- completeClaimGen
    yield claim
      .submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry)
  // .fold(error => throw new Exception(error), identity)

  def buildCompleteClaimGen(
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
    taxCodes: Seq[TaxCode] = TaxCodes.all,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[OverpaymentsSingleClaim.Features] = None
  ): Gen[OverpaymentsSingleClaim] =
    buildClaimGen(
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
      taxCodes = taxCodes,
      generateSubsidyPayments = generateSubsidyPayments,
      features = features
    ).map(
      _.fold(
        error =>
          throw new Exception(
            s"Cannnot build complete OverpaymentsSingleClaim because of $error, fix the test data generator."
          ),
        identity
      )
    )

  def buildClaimGen(
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
    taxCodes: Seq[TaxCode] = TaxCodes.all,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None,
    features: Option[OverpaymentsSingleClaim.Features] = None
  ): Gen[Either[String, OverpaymentsSingleClaim]] =
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
      taxCodes,
      generateSubsidyPayments = generateSubsidyPayments
    ).map(OverpaymentsSingleClaim.tryBuildFrom(_, features))

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
    checkYourAnswersChangeMode: Boolean = true,
    generateSubsidyPayments: GenerateSubsidyPayments = GenerateSubsidyPayments.None
  ): Gen[OverpaymentsSingleClaim.Answers] =
    for
      userEoriNumber              <- IdGen.genEori
      mrn                         <- IdGen.genMRN
      declarantEORI               <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI               <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      basisOfClaim                <- Gen.oneOf(BasisOfOverpaymentClaim.values)
      numberOfTaxCodes            <- Gen.choose(if generateSubsidyPayments == GenerateSubsidyPayments.Some then 2 else 1, 5)
      taxCodes                    <-
        Gen
          .pick(
            numberOfTaxCodes,
            if basisOfClaim === BasisOfOverpaymentClaim.IncorrectExciseValue then TaxCodes.excise else taxCodes
          )
          .map(_ ++ forcedTaxCodes)
      paidAmounts                 <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      correctAmount               <-
        Gen.sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts.map(a => Gen.choose(ZERO, a - BigDecimal("0.01")))
        )
      duplicateMrn                <- if basisOfClaim == BasisOfOverpaymentClaim.DuplicateEntry then IdGen.genMRN.map(Option.apply)
                                     else Gen.const(None)
      reimbursementMethod         <-
        reimbursementMethod.map(Gen.const).getOrElse(Gen.oneOf(ReimbursementMethod.values))
      numberOfSelectedTaxCodes    <- Gen.choose(1, numberOfTaxCodes)
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
      payeeType                   <- Gen.oneOf(PayeeType.values)
      consigneeContact            <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact            <- Gen.option(Acc14Gen.genContactDetails)
      newEoriAndDan                = basisOfClaim match {
                                       case boc if boc == IncorrectEoriAndDan =>
                                         Some(NewEoriAndDan(IdGen.genEori.sample.get, IdGen.genDan.sample.get.value))
                                       case _                                 => None
                                     }
    yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }.toSeq

      val correctedAmounts: Map[TaxCode, Option[BigDecimal]] =
        taxCodes
          .take(numberOfSelectedTaxCodes)
          .zip(correctAmount)
          .map { case (t, a) =>
            (t, Some(a))
          }
          .toMap

      val importDeclaration: ImportDeclaration =
        buildImportDeclaration(
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

      val answers =
        OverpaymentsSingleClaim.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          importDeclaration = Some(importDeclaration),
          payeeType = Some(payeeType),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
          contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          duplicateDeclaration =
            duplicateMrn.map(mrn => DuplicateDeclaration(mrn, importDeclaration.withDeclarationId(mrn.value))),
          additionalDetails = Some("additional details"),
          correctedAmounts = Some(correctedAmounts),
          selectedDocumentType = None,
          supportingEvidences =
            if submitEvidence then supportingEvidencesExpanded
            else Seq.empty,
          bankAccountDetails =
            if submitBankAccountDetails then Some(exampleBankAccountDetails)
            else None,
          bankAccountType =
            if submitBankAccountType then Some(bankAccountType)
            else None,
          reimbursementMethod =
            if submitReimbursementMethod && allDutiesCmaEligible then Some(reimbursementMethod)
            else None,
          modes = ClaimModes(checkYourAnswersChangeMode = checkYourAnswersChangeMode),
          newEori = newEoriAndDan.map(_.eori),
          newDan = newEoriAndDan.map(d => Dan(d.dan))
        )

      answers
    }

  def buildClaimFromAnswersGen(
    answersGen: Gen[OverpaymentsSingleClaim.Answers],
    features: Option[OverpaymentsSingleClaim.Features] = None
  ): Gen[OverpaymentsSingleClaim] =
    answersGen.map(
      OverpaymentsSingleClaim
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
    forcedTaxCodes: Seq[TaxCode] = Seq.empty,
    currentUserEoriNumber: Gen[Eori] = IdGen.genEori
  ): Gen[OverpaymentsSingleClaim.Answers] =
    for
      userEoriNumber   <- currentUserEoriNumber
      mrn              <- IdGen.genMRN
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

      val importDeclaration: ImportDeclaration =
        buildImportDeclaration(
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

      OverpaymentsSingleClaim.Answers(
        nonce = Nonce.random,
        userEoriNumber = userEoriNumber,
        movementReferenceNumber = Some(mrn),
        importDeclaration = Some(importDeclaration),
        eoriNumbersVerification = eoriNumbersVerification,
        contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
        contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
        modes = ClaimModes(checkYourAnswersChangeMode = false)
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
  ): Gen[OverpaymentsSingleClaim.Answers] =
    for
      userEoriNumber           <- IdGen.genEori
      mrn                      <- IdGen.genMRN
      declarantEORI            <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI            <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      basisOfClaim             <- Gen.oneOf(BasisOfOverpaymentClaim.values)
      numberOfTaxCodes         <- Gen.choose(1, 5)
      taxCodes                 <-
        Gen
          .pick(
            numberOfTaxCodes,
            if basisOfClaim === BasisOfOverpaymentClaim.IncorrectExciseValue then TaxCodes.excise else taxCodes
          )
          .map(_ ++ forcedTaxCodes)
      paidAmounts              <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      duplicateMrn             <- if basisOfClaim == BasisOfOverpaymentClaim.DuplicateEntry then IdGen.genMRN.map(Option.apply)
                                  else Gen.const(None)
      numberOfSelectedTaxCodes <- Gen.choose(1, numberOfTaxCodes)
      consigneeContact         <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact         <- Gen.option(Acc14Gen.genContactDetails)
    yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }.toSeq

      val correctedAmounts: Map[TaxCode, Option[BigDecimal]] =
        taxCodes
          .take(numberOfSelectedTaxCodes)
          .map { t =>
            (t, None)
          }
          .toMap

      val importDeclaration: ImportDeclaration =
        buildImportDeclaration(
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
        OverpaymentsSingleClaim.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          importDeclaration = Some(importDeclaration),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
          contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          duplicateDeclaration =
            duplicateMrn.map(mrn => DuplicateDeclaration(mrn, importDeclaration.withDeclarationId(mrn.value))),
          additionalDetails = Some("additional details"),
          correctedAmounts = Some(correctedAmounts),
          modes = ClaimModes(checkYourAnswersChangeMode = false)
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
  ): Gen[OverpaymentsSingleClaim.Answers] =
    for
      userEoriNumber           <- IdGen.genEori
      mrn                      <- IdGen.genMRN
      declarantEORI            <- if acc14DeclarantMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      consigneeEORI            <- if acc14ConsigneeMatchesUserEori then Gen.const(userEoriNumber) else IdGen.genEori
      basisOfClaim             <- Gen.oneOf(BasisOfOverpaymentClaim.values)
      numberOfTaxCodes         <- Gen.choose(1, 5)
      taxCodes                 <-
        Gen
          .pick(
            numberOfTaxCodes,
            if basisOfClaim === BasisOfOverpaymentClaim.IncorrectExciseValue then TaxCodes.excise else taxCodes
          )
          .map(_ ++ forcedTaxCodes)
      paidAmounts              <- listOfExactlyN(numberOfTaxCodes, amountNumberGen)
      correctAmount            <-
        Gen.sequence[Seq[BigDecimal], BigDecimal](
          paidAmounts.map(a => Gen.choose(ZERO, a - BigDecimal("0.01")))
        )
      duplicateMrn             <- if basisOfClaim == BasisOfOverpaymentClaim.DuplicateEntry then IdGen.genMRN.map(Option.apply)
                                  else Gen.const(None)
      numberOfSelectedTaxCodes <- Gen.choose(1, numberOfTaxCodes)
      consigneeContact         <- Gen.option(Acc14Gen.genContactDetails)
      declarantContact         <- Gen.option(Acc14Gen.genContactDetails)
    yield {

      val paidDuties: Seq[(TaxCode, BigDecimal, Boolean)] =
        taxCodes.zip(paidAmounts).map { case (t, a) => (t, a, allDutiesCmaEligible) }.toSeq

      val correctedAmounts: Map[TaxCode, Option[BigDecimal]] =
        taxCodes
          .take(numberOfSelectedTaxCodes)
          .zip(correctAmount)
          .map { case (t, a) =>
            (t, Some(a))
          }
          .toMap

      val importDeclaration: ImportDeclaration =
        buildImportDeclaration(
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
        OverpaymentsSingleClaim.Answers(
          nonce = Nonce.random,
          userEoriNumber = userEoriNumber,
          movementReferenceNumber = Some(mrn),
          importDeclaration = Some(importDeclaration),
          eoriNumbersVerification = eoriNumbersVerification,
          contactDetails = if submitContactDetails then Some(exampleContactDetails) else None,
          contactAddress = if submitContactAddress then Some(exampleContactAddress) else None,
          basisOfClaim = Some(basisOfClaim),
          duplicateDeclaration =
            duplicateMrn.map(mrn => DuplicateDeclaration(mrn, importDeclaration.withDeclarationId(mrn.value))),
          additionalDetails = Some("additional details"),
          correctedAmounts = Some(correctedAmounts),
          modes = ClaimModes(checkYourAnswersChangeMode = false)
        )

      answers
    }
}
