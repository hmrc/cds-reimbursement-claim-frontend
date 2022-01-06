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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload

import cats.data.EitherT
import org.scalamock.handlers.CallHandler2
import org.scalamock.handlers.CallHandler4
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Call
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanSuccess
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadReference
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanUpload
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanUploadMeta
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

abstract class FileUploadControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  val mockUpscanService: UpscanService = mock[UpscanService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[UpscanService].toInstance(mockUpscanService)
    )

  protected def mockUpscanInitiate[A](errorRedirectCall: Call, successRedirectCall: UploadReference => Call)(
    result: Either[Error, UpscanUpload]
  ): CallHandler4[Call, UploadReference => Call, Long, HeaderCarrier, EitherT[
    Future,
    Error,
    UpscanUpload
  ]] =
    (mockUpscanService
      .initiate(_: Call, _: UploadReference => Call, _: Long)(_: HeaderCarrier))
      .expects(
        where {
          (
            actualErrorRedirectCall: Call,
            actualSuccessRedirectCall: UploadReference => Call,
            _: Long,
            _: HeaderCarrier
          ) =>
            val uploadReference = sample[UploadReference]
            actualErrorRedirectCall                    shouldBe errorRedirectCall
            actualSuccessRedirectCall(uploadReference) shouldBe successRedirectCall(uploadReference)
            true
        }
      )
      .returning(EitherT.fromEither[Future](result))

  protected def mockGetUpscanUpload(uploadReference: UploadReference)(
    result: Either[Error, UpscanUpload]
  ): CallHandler2[UploadReference, HeaderCarrier, EitherT[Future, Error, UpscanUpload]] =
    (mockUpscanService
      .getUpscanUpload(_: UploadReference)(_: HeaderCarrier))
      .expects(uploadReference, *)
      .returning(EitherT.fromEither[Future](result))

  protected def genUpscanUpload(uploadReference: UploadReference): UpscanUpload = {
    val uploadRequest    = sample[UploadRequest]
    val upscanUploadMeta = UpscanUploadMeta(
      uploadReference.value,
      uploadRequest
    )
    val upscanSuccess    = sample[UpscanSuccess]
    UpscanUpload(
      uploadReference,
      upscanUploadMeta,
      LocalDateTime.now,
      Some(upscanSuccess)
    )
  }

}
