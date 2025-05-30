/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.http.HeaderCarrier
import org.scalamock.scalatest.MockFactory
import scala.concurrent.Future

trait WafErrorMitigationTestHelper {
  self: MockFactory =>

  val mockUploadDocumentsConnector: UploadDocumentsConnector = mock[UploadDocumentsConnector]

  val expectedUploadDocumentsLocation: String = "http://foo:123/bar/upload-mrn-list"

  def mockInitializeCall(existingFile: Option[UploadedFile] = None) =
    (mockUploadDocumentsConnector
      .initialize(_: UploadDocumentsConnector.Request)(_: HeaderCarrier))
      .expects(where[UploadDocumentsConnector.Request, HeaderCarrier] { case (request, _) =>
        request.existingFiles.map(_.upscanReference) == existingFile.toList.map(_.upscanReference)
      })
      .returning(Future.successful(Some(expectedUploadDocumentsLocation)))

  def mockUploadFileCall(uploadedFile: UploadedFile) =
    (mockUploadDocumentsConnector
      .uploadFile(_: UploadDocumentsConnector.FileToUpload)(_: HeaderCarrier))
      .expects(where[UploadDocumentsConnector.FileToUpload, HeaderCarrier] { case (fileToUpload, _) =>
        fileToUpload.contentType == "text/plain"
      })
      .returning(Future.successful(Some(uploadedFile)))

}
