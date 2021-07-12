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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.data.EitherT
import cats.syntax.eq._
import com.google.inject.Inject
import shapeless._
import play.api.{Configuration, Environment, Mode}
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.i18n.Messages
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import play.twirl.api.HtmlFormat.Appendable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckEoriDetailsController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, Error, SessionData, SignedInUserDetails, VerifiedEmail}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{CustomsDataStoreService, FeatureSwitchService}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.check_eori_details
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckEoriDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  val featureSwitch: FeatureSwitchService,
  cc: MessagesControllerComponents,
  val customsDataStoreService: CustomsDataStoreService,
  val servicesConfig: ServicesConfig,
  val config: Configuration,
  val env: Environment,
  checkEoriDetailsPage: check_eori_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  protected def getPage(
    signedInUserDetails: SignedInUserDetails,
    form: Form[CheckEoriDetailsAnswer]
  )(implicit
    request: RequestWithSessionData[_],
    messages: Messages
  ): Appendable = checkEoriDetailsPage(
    signedInUserDetails,
    form,
    routes.CheckEoriDetailsController.submit()
  )

  private val runMode              = config.getOptional[String]("run.mode")
  private val isRunningLocal       =
    if (env.mode.toString === Mode.Test.toString) true else runMode.forall(_ === Mode.Dev.toString)
  private val customsEmailFrontend = "customs-email-frontend"

  private val customsEmailFrontendUrl: String = {
    val startPage = servicesConfig.getString(s"microservice.services.$customsEmailFrontend.start-page")
    if (isRunningLocal)
      s"${servicesConfig.baseUrl(customsEmailFrontend)}$startPage"
    else s"${servicesConfig.getString("self.url")}$startPage"
  }

  private val emailLens = lens[FillingOutClaim].signedInUserDetails.verifiedEmail

  def show(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.signedInUserDetails
      .fold(Redirect(baseRoutes.StartController.start()))(user => Ok(getPage(user, checkEoriDetailsAnswerForm)))
  }

  def submit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSessionData { (_, fillingOutClaim) =>
      request.signedInUserDetails
        .map { user =>
          checkEoriDetailsAnswerForm
            .bindFromRequest()
            .fold(
              formWithErrors => Future.successful(BadRequest(getPage(user, formWithErrors))),
              {
                case EoriDetailsAreCorrect =>
                  import Logging._

                  val logError: Error => Unit = error => logger.warn(s"Error submitting Eori check", error)

                  val returnErrorPage: Unit => Result = _ => errorHandler.errorResult()

                  def saveSession(verifiedEmail: VerifiedEmail): SessionData => SessionData =
                    _.copy(journeyStatus = Some(emailLens.set(fillingOutClaim)(verifiedEmail.toEmail)))

                  val eitherErrorOrNextPage = for {
                    maybeVerifiedEmail <-
                      customsDataStoreService.getEmailByEori(user.eori).leftMap(logError.andThen(returnErrorPage))
                    verifiedEmail      <- EitherT.fromOption[Future](maybeVerifiedEmail, Redirect(customsEmailFrontendUrl))
                    _                  <- EitherT(updateSession(sessionStore, request)(saveSession(verifiedEmail)))
                                            .leftMap(logError.andThen(returnErrorPage))
                    result             <- EitherT.rightT[Future, Result](
                                            if (featureSwitch.BulkClaim.isEnabled())
                                              Redirect(routes.SelectNumberOfClaimsController.show())
                                            else
                                              Redirect(
                                                routes.EnterMovementReferenceNumberController
                                                  .enterJourneyMrn(JourneyBindable.Single)
                                              )
                                          )
                  } yield result

                  eitherErrorOrNextPage.merge

                case EoriDetailsAreIncorrect => Future.successful(Redirect(viewConfig.ggSignOut))
              }
            )
        }
        .getOrElse(Future.successful(Redirect(baseRoutes.StartController.start())))
    }
  }

  private def withSessionData(
    f: (SessionData, FillingOutClaim) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({ case (sessionData, fillingOutClaim @ FillingOutClaim(_, _, _: DraftClaim)) =>
      f(sessionData, fillingOutClaim)
    })
}

object CheckEoriDetailsController {

  sealed trait CheckEoriDetailsAnswer extends Product with Serializable

  case object EoriDetailsAreCorrect extends CheckEoriDetailsAnswer
  case object EoriDetailsAreIncorrect extends CheckEoriDetailsAnswer

  val dataKey: String = "check-eori-details"

  val checkEoriDetailsAnswerForm: Form[CheckEoriDetailsAnswer] =
    Form(
      mapping(
        dataKey -> number
          .verifying("invalid", a => a === 0 || a === 1)
          .transform[CheckEoriDetailsAnswer](
            value =>
              if (value === 0) EoriDetailsAreCorrect
              else EoriDetailsAreIncorrect,
            {
              case EoriDetailsAreCorrect   => 0
              case EoriDetailsAreIncorrect => 1
            }
          )
      )(identity)(Some(_))
    )

}
