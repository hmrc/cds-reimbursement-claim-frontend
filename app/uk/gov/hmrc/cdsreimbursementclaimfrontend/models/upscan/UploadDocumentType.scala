/*
 * Copyright 2021 HM Revenue & Customs
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

sealed trait UploadDocumentType

object UploadDocumentType extends EnumerationFormat[UploadDocumentType] {

  case object AirWayBill extends UploadDocumentType
  case object BillOfLading extends UploadDocumentType
  case object CommercialInvoice extends UploadDocumentType
  case object CorrespondenceTrader extends UploadDocumentType
  case object ImportAndExportDeclaration extends UploadDocumentType
  case object PackingList extends UploadDocumentType
  case object ProofOfAuthority extends UploadDocumentType
  case object SubstituteEntry extends UploadDocumentType
  case object Other extends UploadDocumentType
  case object ScheduleOfMRNs extends UploadDocumentType

  override val values: Set[UploadDocumentType] = Set[UploadDocumentType](
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

  val c285EvidenceTypes: Seq[UploadDocumentType] = Seq[UploadDocumentType](
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
}
