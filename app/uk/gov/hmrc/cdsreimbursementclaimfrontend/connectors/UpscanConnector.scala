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

import cats.data.EitherT
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpClient
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.control.NonFatal

@ImplementedBy(classOf[DefaultUpscanConnector])
trait UpscanConnector {

  def getUpscanUpload(
    uploadReference: UploadReference
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def saveUpscanUpload(upscanUpload: UpscanUpload)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def initiate(
    errorRedirect: Call,
    successRedirect: Call,
    uploadReference: UploadReference,
    maxFileSize: Long
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

}

@Singleton
class DefaultUpscanConnector @Inject() (
  http: HttpClient,
  fileUploadConfig: FileUploadConfig,
  servicesConfig: ServicesConfig,
  viewConfig: ViewConfig
)(implicit
  ec: ExecutionContext
) extends UpscanConnector
    with Logging {

  private val upscanInitiateUrl: String = {
    val protocol = fileUploadConfig.readUpscanInitServiceProtocol
    val host     = fileUploadConfig.readUpscanInitServiceHost
    val port     = fileUploadConfig.readUpscanInitServicePort
    s"$protocol://$host:$port/upscan/v2/initiate"
  }

  private val baseUrl: String = servicesConfig.baseUrl("cds-reimbursement-claim")

  override def initiate(
    errorRedirect: Call,
    successRedirect: Call,
    uploadReference: UploadReference,
    maxFileSize: Long
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {

    val payload = UpscanInitiateRequest(
      baseUrl + s"/cds-reimbursement-claim/upscan-call-back/upload-reference/${uploadReference.value}",
      viewConfig.selfBaseUrl + successRedirect.url,
      viewConfig.selfBaseUrl + errorRedirect.url,
      0,
      maxFileSize
    )

    logger.info(
      "make upscan initiate call with " +
        s"call back url ${payload.callbackUrl}" +
        s"| success redirect url ${payload.successRedirect} " +
        s"| error redirect url ${payload.errorRedirect}"
    )

    EitherT[Future, Error, HttpResponse](
      http
        .POST[UpscanInitiateRequest, HttpResponse](
          upscanInitiateUrl,
          payload
        )
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
  }

  override def getUpscanUpload(
    uploadReference: UploadReference
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {

    val url = baseUrl + s"/cds-reimbursement-claim/upscan/upload-reference/${uploadReference.value}"

    EitherT[Future, Error, HttpResponse](
      http
        .GET[HttpResponse](url)
        .map(Right(_))
        .recover { case NonFatal(e) => Left(Error(e)) }
    )
  }

  override def saveUpscanUpload(
    upscanUpload: UpscanUpload
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] = {
    val url = baseUrl + "/cds-reimbursement-claim/upscan"

    EitherT[Future, Error, HttpResponse](
      http
        .POST[UpscanUpload, HttpResponse](url, upscanUpload)
        .map(Right(_))
        .recover { case NonFatal(e) => Left(Error(e)) }
    )
  }

}
