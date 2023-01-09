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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors

import cats.implicits.catsSyntaxEq
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.Logger
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpClient
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

@Singleton
class UploadDocumentsCallbackSender @Inject() (http: HttpClient) {

  import UploadDocumentsCallbackSender._

  def sendUploadedFiles(
    url: String,
    nonce: Nonce,
    uploadedFiles: Seq[UploadedFile],
    cargo: Option[UploadDocumentType]
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    http
      .POST[Payload, HttpResponse](url, Payload(nonce, uploadedFiles, cargo))
      .transformWith[Unit] {
        case Success(response)  =>
          Future.successful(
            if (response.status === 204) ()
            else {
              val msg =
                s"Failure pushing uploaded files to $url: ${response.body.take(1024)}"
              Logger(getClass).error(msg)
              ()
            }
          )
        case Failure(exception) =>
          Logger(getClass).error(exception.getMessage())
          Future.successful(())
      }

}

object UploadDocumentsCallbackSender {

  final case class Payload(nonce: Nonce, uploadedFiles: Seq[UploadedFile], cargo: Option[UploadDocumentType])

  object Payload {
    implicit val format: Format[Payload] = Json.format[Payload]
  }
}
