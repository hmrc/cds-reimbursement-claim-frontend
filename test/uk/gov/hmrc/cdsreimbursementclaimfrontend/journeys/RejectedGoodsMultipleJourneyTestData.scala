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

trait RejectedGoodsMultipleJourneyTestData extends JourneyTestData {

  final val emptyJourney: RejectedGoodsMultipleJourney =
    RejectedGoodsMultipleJourney.empty(exampleEori)

  final def tryBuildRejectedGoodsMultipleJourney(
    userEoriNumber: Eori,
    mrns: Seq[MRN],
    displayDeclarations: Seq[DisplayDeclaration],
    basisOfClaim: BasisOfRejectedGoodsClaim,
    detailsOfRejectedGoods: String,
    specialCircumstancesDetails: String,
    inspectionDate: InspectionDate,
    inspectionAddress: InspectionAddress,
    methodOfDisposal: MethodOfDisposal,
    reimbursementClaims: Seq[(MRN, Seq[(TaxCode, BigDecimal, Boolean)])],
    supportingEvidences: Map[UploadDocumentType, Int],
    consigneeEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None
  ): Either[String, RejectedGoodsMultipleJourney] = {

    val mrnsWithDisplayDeclaration: Seq[(Int, MRN, DisplayDeclaration)]          =
      mrns.zip(displayDeclarations).zipWithIndex.map { case ((mrn, acc14), index) => (index, mrn, acc14) }

    val mrnsWithTaxCodes: Seq[(MRN, Seq[TaxCode])]                               =
      reimbursementClaims.map { case (mrn, rc) => mrn -> rc.map(_._1) }

    val mrnsWithTaxCodesWithReimbursementAmount: Seq[(MRN, TaxCode, BigDecimal)] =
      reimbursementClaims.flatMap { case (mrn, rc) => rc.map(c => (mrn, c._1, c._2)) }

    val supportingEvidencesExpanded: Map[UploadDocumentType, Seq[UploadedFile]]  =
      supportingEvidences.map { case (documentType, size) =>
        (documentType, (0 until size).map(i => buildUploadDocument(s"$i")))
      }

    def submitAmountForReimbursement(journey: RejectedGoodsMultipleJourney)(
      data: (MRN, TaxCode, BigDecimal)
    ): Either[String, RejectedGoodsMultipleJourney] =
      journey.submitAmountForReimbursement(data._1, data._2, data._3)

    def receiveUploadedFiles(journey: RejectedGoodsMultipleJourney)(
      documentTypeAndUploadedFiles: (UploadDocumentType, Seq[UploadedFile])
    ): Either[String, RejectedGoodsMultipleJourney] = {
      val (documentType, uploadedFiles) = documentTypeAndUploadedFiles
      val allUploadedFiles              = journey.answers.supportingEvidences ++ uploadedFiles
      journey.receiveUploadedFiles(documentType, journey.answers.nonce, allUploadedFiles)
    }

    def submitMovementReferenceNumberAndDeclaration(journey: RejectedGoodsMultipleJourney)(
      data: (Int, MRN, DisplayDeclaration)
    ): Either[String, RejectedGoodsMultipleJourney] =
      journey.submitMovementReferenceNumberAndDeclaration(data._1, data._2, data._3)

    def selectAndReplaceTaxCodeSetForReimbursement(journey: RejectedGoodsMultipleJourney)(
      data: (MRN, Seq[TaxCode])
    ): Either[String, RejectedGoodsMultipleJourney] =
      journey.selectAndReplaceTaxCodeSetForReimbursement(data._1, data._2)

    RejectedGoodsMultipleJourney
      .empty(userEoriNumber)
      .flatMapEach(mrnsWithDisplayDeclaration, submitMovementReferenceNumberAndDeclaration)
      .flatMapWhenDefined(consigneeEoriNumber)(_.submitConsigneeEoriNumber _)
      .flatMapWhenDefined(declarantEoriNumber)(_.submitDeclarantEoriNumber _)
      .map(_.submitContactDetails(contactDetails))
      .mapWhenDefined(contactAddress)(_.submitContactAddress _)
      .map(_.submitBasisOfClaim(basisOfClaim))
      .flatMapWhen(basisOfClaim == BasisOfRejectedGoodsClaim.SpecialCircumstances)(
        _.submitBasisOfClaimSpecialCircumstancesDetails(specialCircumstancesDetails)
      )
      .map(_.submitMethodOfDisposal(methodOfDisposal))
      .map(_.submitDetailsOfRejectedGoods(detailsOfRejectedGoods))
      .flatMapEach(mrnsWithTaxCodes, selectAndReplaceTaxCodeSetForReimbursement)
      .flatMapEach(mrnsWithTaxCodesWithReimbursementAmount, submitAmountForReimbursement)
      .map(_.submitInspectionDate(inspectionDate))
      .map(_.submitInspectionAddress(inspectionAddress))
      .flatMapWhenDefined(bankAccountDetails)(_.submitBankAccountDetails _)
      .flatMapWhenDefined(bankAccountType)(_.submitBankAccountType _)
      .flatMapEach(supportingEvidencesExpanded, receiveUploadedFiles)
      .map(_.submitCheckYourAnswersChangeMode(true))
  }

}
