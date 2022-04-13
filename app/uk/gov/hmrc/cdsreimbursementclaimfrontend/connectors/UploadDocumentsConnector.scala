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
import cats.data.EitherT
import cats.syntax.eq._
import com.google.inject.Inject
import com.google.inject.ProvidedBy
import com.google.inject.Provider
import play.api.Configuration
import play.api.Logger
import play.api.http.HeaderNames
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.UploadDocumentsConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionModel
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpClient
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.AppName
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import UploadDocumentsConnector._

@ProvidedBy(classOf[UploadDocumentsConnectorProvider])
trait UploadDocumentsConnector {

  /** Initializes upload-documents-frontend session.
    * Might be called multiple times to re-initialize.
    */
  def initialize(request: Request)(implicit
    hc: HeaderCarrier
  ): Future[Response]

  /** Wipes-out upload-documents-frontend session state with related file information,
    * prevents futher file preview. Upscan uploads remain intact.
    */
  def wipeOut(implicit hc: HeaderCarrier): Future[Unit]
}

object UploadDocumentsConnector {

  type Response = Option[String]

  final case class Request(
    config: UploadDocumentsSessionConfig,
    existingFiles: Seq[UploadedFile],
    maybeInternalKey: Option[String]
  )

  implicit val requestFormat: Format[Request] = Json.format[Request]
}

@Singleton
class UploadDocumentsConnectorProvider @Inject() (
  features: FeatureSwitchService,
  internal: InternalUploadDocumentsConnector,
  external: ExternalUploadDocumentsConnector
) extends Provider[UploadDocumentsConnector] {

  override def get(): UploadDocumentsConnector =
    if (features.isEnabled(Feature.InternalUploadDocuments))
      internal
    else
      external

}

@Singleton
class ExternalUploadDocumentsConnector @Inject() (
  http: HttpClient,
  val uploadDocumentsConfig: UploadDocumentsConfig,
  configuration: Configuration,
  val actorSystem: ActorSystem
)(implicit
  ec: ExecutionContext
) extends UploadDocumentsConnector
    with Retries {

  val serviceId: String = AppName.fromConfiguration(configuration)

  override def initialize(request: Request)(implicit
    hc: HeaderCarrier
  ): Future[Response] =
    retry(uploadDocumentsConfig.retryIntervals: _*)(shouldRetry, retryReason)(
      http
        .POST[Request, HttpResponse](uploadDocumentsConfig.initializationUrl, request)
    ).flatMap[Response](response =>
      if (response.status === 201) {
        val maybeUrl: Option[String] =
          response
            .header(HeaderNames.LOCATION)
            .map(location => uploadDocumentsConfig.publicUrl + location)
        println(maybeUrl)
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
        .POST[String, HttpResponse](uploadDocumentsConfig.wipeOutUrl, "")
    ).map[Unit](response =>
      if (response.status === 204) ()
      else {
        Logger(getClass).error(
          s"Request to POST ${uploadDocumentsConfig.wipeOutUrl} failed because of $response ${response.body.take(1024)}"
        )
        ()
      }
    )
}
@Singleton
class InternalUploadDocumentsConnector @Inject() (
  sessionCache: SessionCache,
  servicesConfig: ServicesConfig
)(implicit ec: ExecutionContext)
    extends UploadDocumentsConnector {

  val chooseFileUrl: String =
    servicesConfig.getString(
      "self.url"
    ) + cdsreimbursementclaimfrontend.controllers.fileupload.routes.UploadDocumentsController.show().url

  override def initialize(request: Request)(implicit
    hc: HeaderCarrier
  ): Future[Response] =
    EitherT(sessionCache.get)
      .flatMap {
        case Some(session) =>
          EitherT(
            sessionCache.store(
              session
                .copy(
                  uploadDocumentsSessionModel = Some(
                    UploadDocumentsSessionModel(
                      request.config,
                      request.existingFiles,
                      request.maybeInternalKey.getOrElse("upload-documents")
                    )
                  )
                )
            )
          )

        case None =>
          EitherT.apply[Future, cdsreimbursementclaimfrontend.models.Error, Unit](
            Future.successful(
              Left(
                new cdsreimbursementclaimfrontend.models.Error(
                  "Could not initialize upload document because no claim exists yet.",
                  None,
                  Map.empty
                )
              )
            )
          )
      }
      .fold(
        _ => None,
        _ => Some(chooseFileUrl)
      )

  override def wipeOut(implicit hc: HeaderCarrier): Future[Unit] =
    EitherT(sessionCache.get)
      .flatMap {
        case Some(session) =>
          EitherT(
            sessionCache.store(
              session
                .copy(
                  uploadDocumentsSessionModel = None
                )
            )
          )

        case None =>
          EitherT.apply[Future, cdsreimbursementclaimfrontend.models.Error, Unit](Future.successful(Right(())))
      }
      .fold(_ => (), _ => ())

}
