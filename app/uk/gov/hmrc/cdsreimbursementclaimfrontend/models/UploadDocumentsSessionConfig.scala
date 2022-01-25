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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType

final case class UploadDocumentsSessionConfig(
  nonce: Nonce, // unique secret shared by the host and upload microservices
  continueUrl: String, // url to continue after uploading the files
  backlinkUrl: String, // backlink url
  callbackUrl: String, // url where to post uploaded files
  cargo: UploadDocumentType, // type of the document to assign to the newly added files
  serviceId: Option[String] = None // client ID used by upscan configuration
)

object UploadDocumentsSessionConfig {
  implicit val formats: Format[UploadDocumentsSessionConfig] =
    Json.format[UploadDocumentsSessionConfig]
}
