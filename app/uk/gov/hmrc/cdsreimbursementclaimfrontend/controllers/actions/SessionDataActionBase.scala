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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions

import cats.syntax.either.*
import play.api.mvc.ActionRefiner
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging.*
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait SessionDataActionBase[R[_] <: Request[?], P[_] <: Request[?]] extends ActionRefiner[R, P] with Logging {

  val sessionStore: SessionCache

  val errorHandler: ErrorHandler

  val headersFromRequestOnly: Boolean

  implicit val executionContext: ExecutionContext

  def sessionDataAction[A](
    sessionData: Option[SessionData],
    request: R[A]
  ): P[A]

  override protected def refine[A](request: R[A]): Future[Either[Result, P[A]]] = {

    implicit val hc: HeaderCarrier =
      if headersFromRequestOnly then
        HeaderCarrierConverter
          .fromRequest(request)
      else
        HeaderCarrierConverter
          .fromRequestAndSession(request, request.session)

    sessionStore
      .get()
      .map(
        _.bimap(
          { e =>
            logger.warn("Could not get session data", e)
            errorHandler.errorResult()(request)
          },
          sessionDataAction(_, request)
        )
      )

  }

}
