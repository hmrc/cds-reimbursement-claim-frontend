/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import cats.data.EitherT
import cats.instances.future._
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.ImplementedBy
import com.google.inject.Singleton
import play.api.http.Status.OK
import play.api.mvc.Call
import play.mvc.Http.Status
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadReference
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanUpload
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanUploadMeta
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.HttpResponseOps._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse

import java.time.Clock
import java.time.LocalDateTime
import java.util.UUID
import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[UpscanServiceImpl])
trait UpscanService {

  def initiate(
    errorRedirect: Call,
    successRedirect: UploadReference => Call,
    maxFileSize: Long
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, UpscanUpload]

  def getUpscanUpload(
    uploadReference: UploadReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanUpload]

}

@Singleton
class UpscanServiceImpl @Inject() (upscanConnector: UpscanConnector)(implicit ec: ExecutionContext)
    extends UpscanService
    with Logging {

  def initiate(
    errorRedirect: Call,
    successRedirect: UploadReference => Call,
    maxFileSize: Long
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanUpload] =
    for {
      uploadReference   <- EitherT.pure(UploadReference(UUID.randomUUID().toString))
      maybeHttpResponse <- upscanConnector
                             .initiate(
                               errorRedirect,
                               successRedirect(uploadReference),
                               uploadReference,
                               maxFileSize
                             )
                             .map[Either[Error, HttpResponse]] { response =>
                               if (response.status =!= Status.OK) {
                                 logger.warn(
                                   s"could not initiate upscan: received http " +
                                     s"status ${response.status} and body ${response.body}"
                                 )
                                 Left(Error("could not initiate upscan"))
                               } else
                                 Right(response)
                             }
                             .recover { case e => Left(e) }
      httpResponse      <- EitherT.fromEither(maybeHttpResponse)
      upscanUploadMeta  <- EitherT.fromOption(
                             httpResponse.json.validate[UpscanUploadMeta].asOpt,
                             Error("could not parse http response")
                           )
      upscanUpload      <- EitherT.pure(
                             UpscanUpload(uploadReference, upscanUploadMeta, LocalDateTime.now(Clock.systemUTC()), None)
                           )
      _                 <- upscanConnector.saveUpscanUpload(upscanUpload).map { response =>
                             response.status match {
                               case Status.OK                                         => Right(response)
                               case Status.BAD_REQUEST | Status.INTERNAL_SERVER_ERROR =>
                                 logger.warn("could not save upscan upload")
                                 Left(Error(s"failed to save upscan upload"))
                             }
                           }
    } yield upscanUpload

  override def getUpscanUpload(
    uploadReference: UploadReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanUpload] =
    upscanConnector.getUpscanUpload(uploadReference).subflatMap { response =>
      if (response.status === OK)
        response
          .parseJSON[UpscanUpload]()
          .leftMap(Error(_))
      else {
        logger.warn(
          s"could not get upscan upload: received http " +
            s"status ${response.status} and body ${response.body}"
        )
        Left(Error(s"call to get upscan upload failed ${response.status}"))
      }
    }
}
