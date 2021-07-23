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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import play.api.Logging
import play.api.mvc.{Request, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{RequestWithSessionData, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.{UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.{UploadReference, UpscanUpload}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

trait FileUploadController {
  this: FrontendController with WithAuthAndSessionDataAction with Logging with SessionUpdates =>

  val errorHandler: ErrorHandler
  val upscanService: UpscanService
  val sessionStore: SessionCache

  def initiateUpload[A](
    answer: Option[A]
  )(
    showUploadPage: UpscanUpload => Result
  )(implicit fileUpload: FileUploadHelper[A], request: Request[_], ec: ExecutionContext): Future[Result] =
    if (answer.hasReachedUploadThreshold)
      Future.successful(Redirect(fileUpload.reviewPage))
    else
      upscanService
        .initiate(
          fileUpload.uploadErrorPage,
          fileUpload.handleUploadCallback
        )
        .fold(_ => errorHandler.errorResult(), showUploadPage)

  def handleUploadCallback[A](uploadReference: UploadReference, claim: FillingOutClaim, answer: Option[A])(
    showScanPage: UpscanUpload => Result
  )(implicit
    fileUpload: FileUploadHelper[A],
    request: RequestWithSessionData[_],
    ec: ExecutionContext
  ): Future[Result] = {
    val result = for {
      upscanUpload <- upscanService getUpscanUpload uploadReference
      _            <- upscanUpload.upscanCallBack match {
                        case Some(upscanSuccess: UpscanSuccess) if !answer.containsReference(uploadReference) =>
                          EitherT(
                            updateSession(sessionStore, request)(
                              _.copy(journeyStatus = fileUpload.add(upscanUpload, upscanSuccess, answer, claim).some)
                            )
                          )
                        case _                                                                                =>
                          EitherT.pure[Future, Error](())
                      }
    } yield upscanUpload

    result.fold(
      _ => errorHandler.errorResult(),
      upscanUpload =>
        upscanUpload.upscanCallBack match {
          case Some(_: UpscanSuccess) =>
            Redirect(fileUpload.scanSuccessPage(uploadReference))
          case Some(_: UpscanFailure) =>
            Redirect(fileUpload.scanErrorPage)
          case None                   =>
            showScanPage(upscanUpload)
        }
    )
  }
}
