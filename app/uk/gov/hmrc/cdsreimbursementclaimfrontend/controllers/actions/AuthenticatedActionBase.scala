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

import play.api.Configuration
import play.api.mvc.Results.Redirect
import play.api.mvc.ActionRefiner
import play.api.mvc.Call
import play.api.mvc.MessagesRequest
import play.api.mvc.Result
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.AuthorisedFunctions
import uk.gov.hmrc.auth.core.NoActiveSession
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes.UnauthorisedController.unauthorised
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait AuthenticatedActionBase[P[_]] extends ActionRefiner[MessagesRequest, P] with Logging { self =>

  val authConnector: AuthConnector
  val config: Configuration
  val errorHandler: ErrorHandler
  val sessionStore: SessionCache
  implicit val executionContext: ExecutionContext

  final def unauthorizedErrorPage: Call = unauthorised()

  def authorisedFunction[A](
    auth: AuthorisedFunctions,
    request: MessagesRequest[A]
  ): Future[Either[Result, P[A]]]

  private val authorisedFunctions: AuthorisedFunctions =
    new AuthorisedFunctions {
      override def authConnector: AuthConnector = self.authConnector
    }

  private def getString(key: String): String = config.underlying.getString(key)

  private val signInUrl: String = getString("bas-gateway.signInUrl")

  private val origin: String = getString("gg.origin")

  private val selfBaseUrl: String = getString("self.url")

  override protected def refine[A](
    request: MessagesRequest[A]
  ): Future[Either[Result, P[A]]] =
    authorisedFunction[A](authorisedFunctions, request).recoverWith { case _: NoActiveSession =>
      Future.successful(
        Left(
          Redirect(
            signInUrl,
            Map(
              "continue" -> Seq(selfBaseUrl + request.uri),
              "origin"   -> Seq(origin)
            )
          )
        )
      )
    }
}
