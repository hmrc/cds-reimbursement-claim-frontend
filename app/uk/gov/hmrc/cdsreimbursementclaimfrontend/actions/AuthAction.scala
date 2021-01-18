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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.actions

import com.google.inject.Inject
import play.api.mvc.Results._
import play.api.mvc._
import play.api.{Configuration, Environment}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.AppConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.model.{Eori, SignedInUser}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.play.bootstrap.config.AuthRedirects
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes
import scala.concurrent.{ExecutionContext, Future}

final case class AuthenticatedRequest[A](request: Request[A], user: SignedInUser) extends WrappedRequest[A](request)

class AuthAction[B] @Inject() (
  val authConnector: AuthConnector,
  appConfig: AppConfig,
  mcc: MessagesControllerComponents
)(implicit
  val executionContext: ExecutionContext
) extends ActionBuilder[AuthenticatedRequest, AnyContent]
    with ActionRefiner[Request, AuthenticatedRequest]
    with AuthorisedFunctions
    with AuthRedirects {

  override lazy val config: Configuration     = appConfig.config
  override lazy val env: Environment          = appConfig.environment
  override val parser: BodyParser[AnyContent] = mcc.parsers.defaultBodyParser

  override protected def refine[A](request: Request[A]): Future[Either[Result, AuthenticatedRequest[A]]] = {
    implicit val hc: HeaderCarrier =
      HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

    authorised().retrieve(
      Retrievals.credentials and Retrievals.name and Retrievals.email and Retrievals.allEnrolments
    ) { case credentials ~ name ~ email ~ allEnrolments =>
      val authResult = allEnrolments.getEnrolment("HMRC-CUS-ORG").flatMap(_.getIdentifier("EORINumber")) match {
        case Some(eori) =>
          val cdsLoggedInUser = SignedInUser(credentials, name, email, Eori(eori.value))
          Right(AuthenticatedRequest(request, cdsLoggedInUser))
        //case None       => Left(Redirect(routes.UnauthorisedController.onPageLoad())) //TODO Why does this not work
        case None       => Left(Redirect("/claim-for-reimbursement-of-import-duties/not-subscribed-for-cds"))
      }
      Future.successful(authResult)
    }
  } recover { case _: NoActiveSession =>
    Left(toGGLogin(if (appConfig.isDevEnv) s"http://${request.host}${request.uri}" else s"${request.uri}"))
  }
}
