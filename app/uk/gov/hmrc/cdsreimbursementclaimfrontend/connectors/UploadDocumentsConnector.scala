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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors

import akka.actor.ActorSystem
import cats.syntax.eq._
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import play.api.Configuration
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpClient
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._

import UploadDocumentsConnector._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.play.bootstrap.config.AppName

@ImplementedBy(classOf[UploadDocumentsConnectorImpl])
trait UploadDocumentsConnector {

  def initialize(request: Request)(implicit
    hc: HeaderCarrier
  ): Future[Response]
}

object UploadDocumentsConnector {

  type Response = Unit

  final case class Request(
    config: UploadDocumentsSessionConfig,
    existingFiles: Seq[UploadedFile]
  )

  implicit val requestFormat: Format[Request] = Json.format[Request]
}

@Singleton
class UploadDocumentsConnectorImpl @Inject() (
  http: HttpClient,
  servicesConfig: ServicesConfig,
  configuration: Configuration,
  val actorSystem: ActorSystem
)(implicit
  ec: ExecutionContext
) extends UploadDocumentsConnector
    with Retries {

  val serviceId: String                   = AppName.fromConfiguration(configuration)
  val baseUrl: String                     = servicesConfig.baseUrl("upload-documents-frontend")
  val contextPath: String                 =
    servicesConfig.getConfString("upload-documents-frontend.context-path", "upload-documents-frontend")
  val url: String                         = s"$baseUrl$contextPath/initialize"
  val retryIntervals: Seq[FiniteDuration] = getConfIntervals("upload-documents-frontend", configuration)

  override def initialize(request: Request)(implicit
    hc: HeaderCarrier
  ): Future[Response] =
    retry(retryIntervals: _*)(shouldRetry, retryReason)(
      http
        .POST[Request, HttpResponse](url, request.copy(config = request.config.copy(serviceId = Some(serviceId))))
    ).flatMap[Response](response =>
      if (response.status === 201)
        Future.successful(())
      else
        Future.failed(new Exception(s"Request to POST $url failed because of $response ${response.body}"))
    )
}
