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

trait RejectedGoodsScheduledJourneyTestData extends JourneyTestData {

  val emptyJourney: RejectedGoodsScheduledJourney =
    RejectedGoodsScheduledJourney.empty(exampleEori)

  def tryBuildRejectedGoodsScheduledJourney(
    userEoriNumber: Eori,
    mrn: MRN,
    displayDeclaration: DisplayDeclaration,
    basisOfClaim: BasisOfRejectedGoodsClaim,
    detailsOfRejectedGoods: String,
    specialCircumstancesDetails: String,
    inspectionDate: InspectionDate,
    inspectionAddress: InspectionAddress,
    methodOfDisposal: MethodOfDisposal,
    reimbursementClaims: Seq[(DutyType, Seq[(TaxCode, BigDecimal, BigDecimal)])],
    supportingEvidences: Map[UploadDocumentType, Int],
    consigneeEoriNumber: Option[Eori] = None,
    declarantEoriNumber: Option[Eori] = None,
    contactDetails: Option[MrnContactDetails] = None,
    contactAddress: Option[ContactAddress] = None,
    bankAccountDetails: Option[BankAccountDetails] = None,
    bankAccountType: Option[BankAccountType] = None
  ): Either[String, RejectedGoodsScheduledJourney] = {

    val dutyTypes: Seq[DutyType] =
      reimbursementClaims.map(_._1)

    val dutyTypesWithTaxCodes: Seq[(DutyType, Seq[TaxCode])]                    =
      reimbursementClaims.map { case (dt, tcs) => dt -> tcs.map(_._1) }

    val taxCodesWithAmounts: Seq[(DutyType, TaxCode, BigDecimal, BigDecimal)]   =
      reimbursementClaims.flatMap { case (dt, tca) => tca.map { case (tc, ra, pa) => (dt, tc, ra, pa) } }

    val supportingEvidencesExpanded: Map[UploadDocumentType, Seq[UploadedFile]] =
      supportingEvidences.map { case (documentType, size) =>
        (documentType, (0 until size).map(i => buildUploadDocument(s"$i")))
      }

    val scheduledDocument: UploadedFile = buildUploadDocument(s"schedule")

    def receiveUploadedFiles(journey: RejectedGoodsScheduledJourney)(
      documentTypeAndUploadedFiles: (UploadDocumentType, Seq[UploadedFile])
    ): Either[String, RejectedGoodsScheduledJourney] = {
      val (documentType, uploadedFiles) = documentTypeAndUploadedFiles
      val allUploadedFiles              = journey.answers.supportingEvidences ++ uploadedFiles
      journey.receiveUploadedFiles(documentType, journey.answers.nonce, allUploadedFiles)
    }

    RejectedGoodsScheduledJourney
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
      .flatMap(_.selectAndReplaceDutyTypeSetForReimbursement(dutyTypes))
      .flatMapEach(
        dutyTypesWithTaxCodes,
        j => (d: (DutyType, Seq[TaxCode])) => j.selectAndReplaceTaxCodeSetForReimbursement(d._1, d._2)
      )
      .flatMapEach(
        taxCodesWithAmounts,
        j => (d: (DutyType, TaxCode, BigDecimal, BigDecimal)) => j.submitAmountForReimbursement(d._1, d._2, d._3, d._4)
      )
      .map(_.submitInspectionDate(inspectionDate))
      .map(_.submitInspectionAddress(inspectionAddress))
      .flatMapWhenDefined(bankAccountDetails)(_.submitBankAccountDetails _)
      .flatMapWhenDefined(bankAccountType)(_.submitBankAccountType _)
      .flatMapEach(supportingEvidencesExpanded, receiveUploadedFiles)
      .flatMap(j => j.receiveScheduledDocument(j.answers.nonce, scheduledDocument))
      .map(_.submitCheckYourAnswersChangeMode(true))
  }

}
