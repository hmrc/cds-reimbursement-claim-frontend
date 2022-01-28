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

import play.api.libs.json.Format
import play.api.libs.json.Json
import java.time.ZonedDateTime
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType

/** DTO between upload-documents-frontend and this microservice.
  * Do NOT rename fields!
  */
final case class UploadedFile(
  upscanReference: String,
  downloadUrl: String,
  uploadTimestamp: ZonedDateTime,
  checksum: String,
  fileName: String,
  fileMimeType: String,
  fileSize: Option[Long],
  cargo: Option[UploadDocumentType] = None,
  description: Option[String] = None,
  previewUrl: Option[String] = None
) {
  def documentType: Option[UploadDocumentType] = cargo
}

object UploadedFile {
  implicit val formats: Format[UploadedFile] = Json.format[UploadedFile]
}
