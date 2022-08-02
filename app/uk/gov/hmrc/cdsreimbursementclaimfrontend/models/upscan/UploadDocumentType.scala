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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.EnumerationFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity

sealed trait UploadDocumentType

object UploadDocumentType extends EnumerationFormat[UploadDocumentType] {

  case object AirWayBill extends UploadDocumentType
  case object BillOfLading extends UploadDocumentType
  case object CommercialInvoice extends UploadDocumentType
  case object CorrespondenceTrader extends UploadDocumentType
  case object ImportDeclaration extends UploadDocumentType
  case object ExportDeclaration extends UploadDocumentType
  case object ImportAndExportDeclaration extends UploadDocumentType
  case object PackingList extends UploadDocumentType
  case object ExportPackingList extends UploadDocumentType
  case object ImportPackingList extends UploadDocumentType
  case object ProofOfAuthority extends UploadDocumentType
  case object ProofOfEligibility extends UploadDocumentType
  case object ProofOfOrigin extends UploadDocumentType
  case object SubstituteEntry extends UploadDocumentType
  case object SubstituteOrDiversionEntry extends UploadDocumentType
  case object Other extends UploadDocumentType
  case object ScheduleOfMRNs extends UploadDocumentType
  case object CalculationWorksheet extends UploadDocumentType
  case object CalculationWorksheetOrFinalSalesFigures extends UploadDocumentType
  case object DocumentaryProofFaultyOrNotWhatOrdered extends UploadDocumentType
  case object ProofOfExportOrDestruction extends UploadDocumentType
  case object AdditionalSupportingDocuments extends UploadDocumentType
  case object LetterOfAuthority extends UploadDocumentType
  case object SupportingEvidence extends UploadDocumentType
  case object BillOfDischarge3 extends UploadDocumentType
  case object BillOfDischarge4 extends UploadDocumentType
  case object QuotaLicense extends UploadDocumentType

  override val values: Set[UploadDocumentType] =
    Set[UploadDocumentType](
      AirWayBill,
      BillOfLading,
      CommercialInvoice,
      CorrespondenceTrader,
      ImportDeclaration,
      ExportDeclaration,
      ImportAndExportDeclaration,
      PackingList,
      ExportPackingList,
      ImportPackingList,
      ProofOfAuthority,
      ProofOfEligibility,
      ProofOfOrigin,
      SubstituteEntry,
      SubstituteOrDiversionEntry,
      ScheduleOfMRNs,
      Other,
      CalculationWorksheet,
      CalculationWorksheetOrFinalSalesFigures,
      DocumentaryProofFaultyOrNotWhatOrdered,
      ProofOfExportOrDestruction,
      AdditionalSupportingDocuments,
      LetterOfAuthority,
      SupportingEvidence,
      BillOfDischarge3,
      BillOfDischarge4,
      QuotaLicense
    )

  val c285EvidenceTypes: Seq[UploadDocumentType] =
    Seq[UploadDocumentType](
      AirWayBill,
      BillOfLading,
      CommercialInvoice,
      CorrespondenceTrader,
      ImportAndExportDeclaration,
      PackingList,
      ProofOfAuthority,
      SubstituteEntry,
      Other
    )

  val rejectedGoodsSingleTypes: Seq[UploadDocumentType] =
    Seq[UploadDocumentType](
      AdditionalSupportingDocuments,
      CalculationWorksheet,
      CommercialInvoice,
      CorrespondenceTrader,
      DocumentaryProofFaultyOrNotWhatOrdered,
      ImportAndExportDeclaration,
      LetterOfAuthority,
      ProofOfExportOrDestruction
    )

  val rejectedGoodsMultipleTypes: Seq[UploadDocumentType] =
    rejectedGoodsSingleTypes

  val rejectedGoodsScheduledTypes: Seq[UploadDocumentType] =
    rejectedGoodsSingleTypes

  val securitiesTypes: ReasonForSecurity => Option[Seq[UploadDocumentType]] = {
    val pf: PartialFunction[ReasonForSecurity, Seq[UploadDocumentType]] = {
      case ReasonForSecurity.InwardProcessingRelief =>
        Seq[UploadDocumentType](
          ImportDeclaration,
          ExportDeclaration,
          ExportPackingList,
          SubstituteOrDiversionEntry,
          BillOfDischarge3,
          Other
        )

      case ReasonForSecurity.EndUseRelief =>
        Seq[UploadDocumentType](
          ImportDeclaration,
          ExportDeclaration,
          ExportPackingList,
          SubstituteOrDiversionEntry,
          BillOfDischarge4,
          Other
        )

      case ReasonForSecurity.OutwardProcessingRelief | ReasonForSecurity.TemporaryAdmission2M |
          ReasonForSecurity.TemporaryAdmission3M | ReasonForSecurity.TemporaryAdmission6M |
          ReasonForSecurity.TemporaryAdmission2Y =>
        Seq[UploadDocumentType](
          CommercialInvoice,
          ImportDeclaration,
          ExportDeclaration,
          ImportPackingList,
          ExportPackingList,
          SubstituteOrDiversionEntry,
          Other
        )

      case ReasonForSecurity.AccountSales =>
        Seq[UploadDocumentType](
          CommercialInvoice,
          ImportDeclaration,
          CalculationWorksheetOrFinalSalesFigures,
          Other
        )

      case ReasonForSecurity.MissingPreferenceCertificate =>
        Seq[UploadDocumentType](
          CommercialInvoice,
          ImportDeclaration,
          ProofOfOrigin,
          Other
        )

      case ReasonForSecurity.MissingLicenseQuota =>
        Seq[UploadDocumentType](
          CommercialInvoice,
          ImportDeclaration,
          QuotaLicense,
          Other
        )

      case ReasonForSecurity.CommunitySystemsOfDutyRelief =>
        Seq[UploadDocumentType](
          CommercialInvoice,
          ImportDeclaration,
          ProofOfEligibility,
          Other
        )
    }
    pf.lift
  }
}
