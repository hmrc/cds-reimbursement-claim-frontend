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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import java.time.LocalDateTime
import cats.Eq
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocument

final case class EvidenceDocument(
  checksum: String,
  downloadUrl: String,
  fileName: String,
  fileMimeType: String,
  size: Long,
  uploadedOn: LocalDateTime,
  documentType: UploadDocumentType
)

object EvidenceDocument {

  def from(uploadedDocument: UploadDocument): EvidenceDocument =
    EvidenceDocument(
      checksum = uploadedDocument.upscanSuccess.uploadDetails.checksum,
      downloadUrl = uploadedDocument.upscanSuccess.downloadUrl,
      fileName = uploadedDocument.fileName,
      fileMimeType = uploadedDocument.upscanSuccess.uploadDetails.fileMimeType,
      size = uploadedDocument.upscanSuccess.uploadDetails.size,
      uploadedOn = uploadedDocument.uploadedOn,
      documentType = uploadedDocument.documentType.getOrElse(UploadDocumentType.Other)
    )

  implicit val equality: Eq[EvidenceDocument]   = Eq.fromUniversalEquals
  implicit val format: Format[EvidenceDocument] = Json.format

}
