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

import cats.syntax.eq.*
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import org.apache.pekko.actor.ActorSystem
import play.api.http.HeaderNames
import play.api.libs.json.Format
import play.api.libs.json.Json
import play.api.Configuration
import play.api.Logger
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.UploadDocumentsConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.http.HttpReads.Implicits.*
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpClient
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.AppName

import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[UploadDocumentsConnectorImpl])
trait UploadDocumentsConnector {

  /** Initializes upload-documents-frontend session. Might be called multiple times to re-initialize.
    */
  def initialize(request: Request)(implicit
    hc: HeaderCarrier
  ): Future[Response]

  /** Wipes-out upload-documents-frontend session state with related file information, prevents futher file preview.
    * Upscan uploads remain intact.
    */
  def wipeOut(implicit hc: HeaderCarrier): Future[Unit]
}

object UploadDocumentsConnector {

  type Response = Option[String]

  final case class Request(
    config: UploadDocumentsSessionConfig,
    existingFiles: Seq[UploadedFile]
  )

  implicit val requestFormat: Format[Request] = Json.format[Request]
}

@Singleton
class UploadDocumentsConnectorImpl @Inject() (
  http: HttpClient,
  val uploadDocumentsConfig: UploadDocumentsConfig,
  configuration: Configuration,
  val actorSystem: ActorSystem
)(implicit
  ec: ExecutionContext
) extends UploadDocumentsConnector
    with Retries {

  // TODO: unsure if still needed, find out what configuration does
  val serviceId: String = AppName.fromConfiguration(configuration)

  override def initialize(request: Request)(implicit
    hc: HeaderCarrier
  ): Future[Response] =
    retry(uploadDocumentsConfig.retryIntervals: _*)(shouldRetry, retryReason)(
      http
        .POST[Request, HttpResponse](java.net.URL(uploadDocumentsConfig.initializationUrl), request)
    ).flatMap[Response](response =>
      if response.status === 201 then {
        val maybeUrl: Option[String] =
          response
            .header(HeaderNames.LOCATION)
            .map(location => uploadDocumentsConfig.publicUrl + location)
        Future.successful(maybeUrl)
      } else
        Future.failed(
          new Exception(
            s"Request to POST ${uploadDocumentsConfig.initializationUrl} failed because of $response ${response.body.take(1024)}"
          )
        )
    )

  override def wipeOut(implicit hc: HeaderCarrier): Future[Unit] =
    retry(uploadDocumentsConfig.retryIntervals: _*)(shouldRetry, retryReason)(
      http
        .POST[String, HttpResponse](java.net.URL(uploadDocumentsConfig.wipeOutUrl), "")
    ).map[Unit](response =>
      if response.status === 204 then ()
      else {
        Logger(getClass).error(
          s"Request to POST ${uploadDocumentsConfig.wipeOutUrl} failed because of $response ${response.body.take(1024)}"
        )
        ()
      }
    )
}
