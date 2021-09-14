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

import julienrf.json.derived
import play.api.libs.json.OFormat

sealed abstract class UploadDocumentType(val index: Int) extends Product with Serializable

object UploadDocumentType {

  case object CommercialInvoice extends UploadDocumentType(0)
  case object ImportAndExportDeclaration extends UploadDocumentType(1)
  case object C88E2 extends UploadDocumentType(2)
  case object PackingList extends UploadDocumentType(3)
  case object AirWayBill extends UploadDocumentType(4)
  case object BillOfLading extends UploadDocumentType(5)
  case object SubstituteEntry extends UploadDocumentType(6)
  case object ProofOfAuthority extends UploadDocumentType(7)
  case object CorrespondenceTrader extends UploadDocumentType(8)
  case object Other extends UploadDocumentType(9)
  case object ScheduleOfMRNs extends UploadDocumentType(10)

  private lazy val completeListOfEvidencesTypes = Seq(
    CommercialInvoice,
    ImportAndExportDeclaration,
    C88E2,
    PackingList,
    AirWayBill,
    BillOfLading,
    SubstituteEntry,
    ProofOfAuthority,
    CorrespondenceTrader,
    Other
  )

  private lazy val mrnJourneyEvidenceTypes: Seq[UploadDocumentType] =
    completeListOfEvidencesTypes.diff(Seq(C88E2))

  def getListOfEvidenceTypes(isEntryNumberJourneyEnabled: Boolean): Seq[UploadDocumentType] =
    if (isEntryNumberJourneyEnabled) completeListOfEvidencesTypes else mrnJourneyEvidenceTypes

  val evidenceIndicesToTypes: Map[Int, UploadDocumentType] =
    completeListOfEvidencesTypes.map(doc => doc.index -> doc).toMap

  val evidenceTypesToIndices: Map[UploadDocumentType, Int] =
    completeListOfEvidencesTypes.map(doc => doc -> doc.index).toMap

  implicit val format: OFormat[UploadDocumentType]         = derived.oformat[UploadDocumentType]()
}
