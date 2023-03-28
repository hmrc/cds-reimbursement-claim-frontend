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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.Eq
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType

import java.time.LocalDateTime

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

  def from(uploadedFile: UploadedFile): EvidenceDocument =
    EvidenceDocument(
      checksum = uploadedFile.checksum,
      downloadUrl = uploadedFile.downloadUrl,
      fileName = uploadedFile.fileName,
      fileMimeType = uploadedFile.fileMimeType,
      size = uploadedFile.fileSize.getOrElse(0L),
      uploadedOn = uploadedFile.uploadTimestamp.toLocalDateTime,
      documentType = uploadedFile.documentType.getOrElse(UploadDocumentType.Other)
    )

  implicit val equality: Eq[EvidenceDocument]   = Eq.fromUniversalEquals
  implicit val format: Format[EvidenceDocument] = Json.format

}
