/*
 * Copyright 2022 HM Revenue & Customs
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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._

trait SecuritiesJourneyTestData extends JourneyTestData {

  final val emptyJourney: SecuritiesJourney =
    SecuritiesJourney.empty(exampleEori)

  final def securitiesJourneyWithMrnAndRfsAndDeclaration(rfs: ReasonForSecurity) = SecuritiesJourney
    .empty(exampleEori)
    .submitMovementReferenceNumber(exampleMrn)
    .submitReasonForSecurityAndDeclaration(
      rfs,
      buildSecuritiesDisplayDeclaration(
        securityReason = rfs.acc14Code,
        declarantContact = Some(exampleDeclarationContactDetails)
      )
    )
    .flatMap(_.submitClaimDuplicateCheckStatus(false))
    .getOrFail

  final def tryBuildSecuritiesJourney(
    userEoriNumber: Eori,
    mrn: MRN,
    reasonForSecurity: ReasonForSecurity,
    displayDeclaration: DisplayDeclaration,
    similarClaimExistAlreadyInCDFPay: Boolean,
    reclaims: Seq[(String, TaxCode, BigDecimal)],
    exportMrn: Option[MRN],
    consigneeEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None,
    supportingEvidences: Map[UploadDocumentType, Int] = Map.empty,
    methodOfDisposal: Option[TemporaryAdmissionMethodOfDisposal] = None
  ): Either[String, SecuritiesJourney] = {

    val supportingEvidencesExpanded: Map[UploadDocumentType, Seq[UploadedFile]] =
      supportingEvidences.map { case (documentType, size) =>
        (documentType, (0 until size).map(i => buildUploadDocument(s"$i")))
      }

    def receiveUploadedFiles(journey: SecuritiesJourney)(
      documentTypeAndUploadedFiles: (UploadDocumentType, Seq[UploadedFile])
    ): Either[String, SecuritiesJourney] = {
      val (documentType, uploadedFiles) = documentTypeAndUploadedFiles
      val allUploadedFiles              = journey.answers.supportingEvidences ++ uploadedFiles
      journey.receiveUploadedFiles(documentType, journey.answers.nonce, allUploadedFiles)
    }

    SecuritiesJourney
      .empty(userEoriNumber)
      .submitMovementReferenceNumber(mrn)
      .submitReasonForSecurityAndDeclaration(reasonForSecurity, displayDeclaration)
      .flatMap(_.submitClaimDuplicateCheckStatus(similarClaimExistAlreadyInCDFPay))
      .flatMapWhenDefined(methodOfDisposal)(_.submitTemporaryAdmissionMethodOfDisposal _)
      .flatMapWhenDefined(exportMrn)(_.submitExportMovementReferenceNumber _)
      .flatMapWhenDefined(consigneeEoriNumber)(_.submitConsigneeEoriNumber _)
      .flatMapWhenDefined(declarantEoriNumber)(_.submitDeclarantEoriNumber _)
      .map(_.submitContactDetails(contactDetails))
      .mapWhenDefined(contactAddress)(_.submitContactAddress _)
      .flatMapEach(reclaims.map(_._1).distinct, (journey: SecuritiesJourney) => journey.selectSecurityDepositId(_))
      .flatMapEach(
        reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, amount) => (tc, amount) }).toSeq,
        (journey: SecuritiesJourney) =>
          (args: (String, Seq[(TaxCode, BigDecimal)])) =>
            journey
              .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
              .flatMapEach(
                args._2,
                (journey: SecuritiesJourney) =>
                  (args2: (TaxCode, BigDecimal)) => {
                    journey.submitAmountForReclaim(args._1, args2._1, args2._2)
                  }
              )
      )
      .map(_.submitCheckClaimDetailsChangeMode(true))
      .flatMapWhenDefined(bankAccountDetails)(_.submitBankAccountDetails _)
      .flatMapWhenDefined(bankAccountType)(_.submitBankAccountType _)
      .flatMapEach(supportingEvidencesExpanded, receiveUploadedFiles)
      .map(_.submitCheckYourAnswersChangeMode(true))
  }

  final def buildSecuritiesJourneyReadyForSelectingSecurities(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration)
  ): SecuritiesJourney = testParams match {
    case (mrn, rfs, decl) =>
      emptyJourney
        .submitMovementReferenceNumber(mrn)
        .submitReasonForSecurityAndDeclaration(rfs, decl)
        .flatMap(_.submitClaimDuplicateCheckStatus(false))
        .getOrFail
  }

  final def buildSecuritiesJourneyWithDocumentTypeSelected(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration, UploadDocumentType)
  ): SecuritiesJourney = testParams match {
    case (mrn, rfs, decl, dt) =>
      emptyJourney
        .submitMovementReferenceNumber(mrn)
        .submitReasonForSecurityAndDeclaration(rfs, decl)
        .flatMap(_.submitClaimDuplicateCheckStatus(false))
        .flatMap(_.submitDocumentTypeSelection(dt))
        .getOrFail
  }

  final def buildSecuritiesJourneyWithSomeSecuritiesSelected(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration)
  ): SecuritiesJourney = {
    val journey    = buildSecuritiesJourneyReadyForSelectingSecurities(testParams)
    val depositIds = journey.getSecurityDepositIds
    journey
      .submitCheckDeclarationDetailsChangeMode(false)
      .selectSecurityDepositIds(depositIds.secondHalfNonEmpty)
      .getOrFail
  }

  final def buildSecuritiesJourneyWithSomeSecuritiesSelectedGeneratedMfd(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration, TemporaryAdmissionMethodOfDisposal)
  ): SecuritiesJourney = testParams match {
    case (mrn: MRN, rfs: ReasonForSecurity, acc14: DisplayDeclaration, _: TemporaryAdmissionMethodOfDisposal) =>
      buildSecuritiesJourneyWithSomeSecuritiesSelected((mrn, rfs, acc14))
  }

  final def buildSecuritiesJourneyWithSomeSecuritiesSelectedWithMehodOfDisposal(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration, TemporaryAdmissionMethodOfDisposal)
  ): SecuritiesJourney = testParams match {
    case (mrn: MRN, rfs: ReasonForSecurity, acc14: DisplayDeclaration, mfd: TemporaryAdmissionMethodOfDisposal) =>
      buildSecuritiesJourneyWithSomeSecuritiesSelected((mrn, rfs, acc14))
        .submitTemporaryAdmissionMethodOfDisposal(mfd).getOrFail
  }
  final def buildSecuritiesJourneyInChangeDeclarationDetailsMode(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration)
  ): SecuritiesJourney =
    buildSecuritiesJourneyWithSomeSecuritiesSelected(testParams)
      .submitCheckDeclarationDetailsChangeMode(true)

  final def buildSecuritiesJourneyWithDutiesPartiallySelected(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal)])
  ): SecuritiesJourney = testParams match {
    case (mrn, rfs, decl, reclaims) =>
      val availableDepositIds: Seq[String] = decl.getSecurityDepositIds.get

      val depositIdsWithSomeDutiesSelected: Seq[String] = reclaims.map(_._1).distinct

      val depositIdsWithoutDutiesSelected: Seq[String] =
        availableDepositIds.filterNot(depositIdsWithSomeDutiesSelected.contains).halfNonEmpty

      val taxCodesPerDepositId: Seq[(String, Seq[TaxCode])] =
        reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, _) => tc }).toSeq

      buildSecuritiesJourneyReadyForSelectingSecurities((mrn, rfs, decl))
        .flatMapEach(
          depositIdsWithSomeDutiesSelected ++ depositIdsWithoutDutiesSelected,
          (journey: SecuritiesJourney) => journey.selectSecurityDepositId(_)
        )
        .flatMapEach(
          taxCodesPerDepositId,
          (journey: SecuritiesJourney) =>
            (args: (String, Seq[TaxCode])) =>
              journey
                .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2)
        )
        .getOrFail
  }

  final def buildSecuritiesJourneyReadyForEnteringClaimAmounts(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal)])
  ): SecuritiesJourney = testParams match {
    case (mrn, rfs, decl, reclaims) =>
      val depositIds: Seq[String]                           = reclaims.map(_._1).distinct
      val taxCodesPerDepositId: Seq[(String, Seq[TaxCode])] =
        reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, _) => tc }).toSeq

      buildSecuritiesJourneyReadyForSelectingSecurities((mrn, rfs, decl))
        .flatMapEach(
          depositIds,
          (journey: SecuritiesJourney) => journey.selectSecurityDepositId(_)
        )
        .flatMapEach(
          taxCodesPerDepositId,
          (journey: SecuritiesJourney) =>
            (args: (String, Seq[TaxCode])) =>
              journey
                .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2)
        )
        .getOrFail
  }

  final def buildSecuritiesJourneyWithClaimsEntered(
    testParams: (MRN, ReasonForSecurity, DisplayDeclaration, Seq[(String, TaxCode, BigDecimal)])
  ): SecuritiesJourney = testParams match {
    case (mrn, rfs, decl, reclaims) =>
      val depositIds = reclaims.map(_._1).distinct

      buildSecuritiesJourneyReadyForSelectingSecurities((mrn, rfs, decl))
        .flatMapEach(
          depositIds,
          (journey: SecuritiesJourney) => journey.selectSecurityDepositId(_)
        )
        .flatMapEach(
          reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, amount) => (tc, amount) }).toSeq,
          (journey: SecuritiesJourney) =>
            (args: (String, Seq[(TaxCode, BigDecimal)])) =>
              journey
                .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                .flatMapEach(
                  args._2,
                  (journey: SecuritiesJourney) =>
                    (args2: (TaxCode, BigDecimal)) => journey.submitAmountForReclaim(args._1, args2._1, args2._2)
                )
        )
        .getOrFail
  }

}
