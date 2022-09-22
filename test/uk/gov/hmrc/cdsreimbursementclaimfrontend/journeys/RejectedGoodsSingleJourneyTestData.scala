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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._

trait RejectedGoodsSingleJourneyTestData extends JourneyTestData {

  val emptyJourney: RejectedGoodsSingleJourney =
    RejectedGoodsSingleJourney.empty(exampleEori)

  def tryBuildRejectedGoodsSingleJourney(
    userEoriNumber: Eori,
    mrn: MRN,
    displayDeclaration: DisplayDeclaration,
    basisOfClaim: BasisOfRejectedGoodsClaim,
    detailsOfRejectedGoods: String,
    specialCircumstancesDetails: String,
    inspectionDate: InspectionDate,
    inspectionAddress: InspectionAddress,
    methodOfDisposal: MethodOfDisposal,
    reimbursementClaims: Seq[(TaxCode, BigDecimal, Boolean)],
    supportingEvidences: Map[UploadDocumentType, Int],
    reimbursementMethod: Option[ReimbursementMethod] = None,
    consigneeEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None
  ): Either[String, RejectedGoodsSingleJourney] = {
    val taxCodes: Seq[TaxCode]                                      =
      reimbursementClaims.map(_._1)
    val taxCodesWithReimbursementAmount: Seq[(TaxCode, BigDecimal)] =
      reimbursementClaims.map(e => (e._1, e._2))

    val supportingEvidencesExpanded: Map[UploadDocumentType, Seq[UploadedFile]] =
      supportingEvidences.map { case (documentType, size) =>
        (documentType, (0 until size).map(i => buildUploadDocument(s"$i")))
      }

    def submitAmountForReimbursement(journey: RejectedGoodsSingleJourney)(
      taxCodesWithReimbursementAmount: (TaxCode, BigDecimal)
    ): Either[String, RejectedGoodsSingleJourney] =
      journey.submitAmountForReimbursement(taxCodesWithReimbursementAmount._1, taxCodesWithReimbursementAmount._2)

    def receiveUploadedFiles(journey: RejectedGoodsSingleJourney)(
      documentTypeAndUploadedFiles: (UploadDocumentType, Seq[UploadedFile])
    ): Either[String, RejectedGoodsSingleJourney] = {
      val (documentType, uploadedFiles) = documentTypeAndUploadedFiles
      val allUploadedFiles              = journey.answers.supportingEvidences ++ uploadedFiles
      journey.receiveUploadedFiles(documentType, journey.answers.nonce, allUploadedFiles)
    }

    RejectedGoodsSingleJourney
      .empty(userEoriNumber)
      .submitMovementReferenceNumberAndDeclaration(mrn, displayDeclaration)
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
      .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(taxCodes))
      .flatMapEach(taxCodesWithReimbursementAmount, submitAmountForReimbursement)
      .map(_.submitInspectionDate(inspectionDate))
      .map(_.submitInspectionAddress(inspectionAddress))
      .flatMapWhenDefined(reimbursementMethod)(_.submitReimbursementMethod _)
      .flatMapWhenDefined(bankAccountDetails)(_.submitBankAccountDetails _)
      .flatMapWhenDefined(bankAccountType)(_.submitBankAccountType _)
      .flatMapEach(supportingEvidencesExpanded, receiveUploadedFiles)
      .map(_.submitCheckYourAnswersChangeMode(true))
  }

}
