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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common

import cats.data.EitherT
import com.google.inject.Inject
import play.api.Configuration
import play.api.Environment
import play.api.data.Form
import play.api.i18n.Messages
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import play.twirl.api.HtmlFormat.Appendable
import shapeless._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.EnvironmentOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.{routes => commonRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.CheckEoriDetailsController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.VerifiedEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.CustomsDataStoreService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_eori_details
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

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
  val env: Environment,
  checkEoriDetailsPage: check_eori_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, config: Configuration)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  protected def getPage(
    signedInUserDetails: SignedInUserDetails,
    form: Form[YesNo]
  )(implicit
    request: RequestWithSessionData[_],
    messages: Messages
  ): Appendable = checkEoriDetailsPage(
    signedInUserDetails,
    form,
    routes.CheckEoriDetailsController.submit()
  )

  private val customsEmailFrontendUrl: String = {
    val customsEmailFrontend = "customs-email-frontend"
    val startPage            = servicesConfig.getString(s"microservice.services.$customsEmailFrontend.start-page")

    if (env.isLocal)
      s"${servicesConfig.baseUrl(customsEmailFrontend)}$startPage"
    else s"${servicesConfig.getString("self.url")}$startPage"
  }

  private val emailLens = lens[FillingOutClaim].signedInUserDetails.verifiedEmail

  def show(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.signedInUserDetails
      .fold(Redirect(baseRoutes.StartController.start()))(user => Ok(getPage(user, whetherEoriDetailsCorrect)))
  }

  def submit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSessionData { fillingOutClaim =>
      request.signedInUserDetails
        .map { user =>
          whetherEoriDetailsCorrect
            .bindFromRequest()
            .fold(
              formWithErrors => Future.successful(BadRequest(getPage(user, formWithErrors))),
              {
                case Yes =>
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
                                            if (featureSwitch.isEnabled(Feature.RejectedGoods))
                                              Redirect(commonRoutes.ChooseClaimTypeController.show())
                                            else
                                              Redirect(commonRoutes.SelectTypeOfClaimController.show())
                                          )
                  } yield result

                  eitherErrorOrNextPage.merge

                case No => Future.successful(Redirect(viewConfig.ggSignOut))
              }
            )
        }
        .getOrElse(Future.successful(Redirect(baseRoutes.StartController.start())))
    }
  }

  private def withSessionData(
    f: FillingOutClaim => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.using({ case fillingOutClaim @ FillingOutClaim(_, _, _: DraftClaim) =>
      f(fillingOutClaim)
    })
}

object CheckEoriDetailsController {

  val checkEoriDetailsKey: String = "check-eori-details"

  val whetherEoriDetailsCorrect: Form[YesNo] = YesOrNoQuestionForm(checkEoriDetailsKey)
}
