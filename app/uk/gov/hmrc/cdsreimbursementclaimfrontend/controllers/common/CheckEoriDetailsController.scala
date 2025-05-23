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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common

import com.google.inject.Inject
import play.api.Environment
import play.api.data.Form
import play.api.i18n.Messages
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Request
import play.twirl.api.HtmlFormat.Appendable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthRetrievalsAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.CheckEoriDetailsController.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.routes as commonRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.VerifiedEmailAddressService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_eori_details
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class CheckEoriDetailsController @Inject() (
  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  val featureSwitch: FeatureSwitchService,
  val controllerComponents: MessagesControllerComponents,
  val verifiedEmailAddressService: VerifiedEmailAddressService,
  val servicesConfig: ServicesConfig,
  val env: Environment,
  checkEoriDetailsPage: check_eori_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendBaseController
    with WithAuthRetrievalsAndSessionDataAction
    with SessionUpdates
    with Logging {

  protected def getPage(
    eori: Eori,
    name: Option[String],
    form: Form[YesNo]
  )(implicit
    request: Request[?],
    messages: Messages
  ): Appendable = checkEoriDetailsPage(
    eori,
    name.getOrElse("No name"),
    form,
    routes.CheckEoriDetailsController.submit
  )

  final val show: Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
      request.whenAuthorisedUser {
        (
          eori: Eori,
          name: Option[String]
        ) => // fixme fetch name from sub09 (eoriDetailsConnector.getCurrentUserEoriDetails....)
          Future.successful(Ok(getPage(eori, name, whetherEoriDetailsCorrect)))
      }(resultIfUnsupportedUser = Redirect(baseRoutes.StartController.start()))
    }

  final val submit: Action[AnyContent] = authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
    request.whenAuthorisedUser { (eori: Eori, name: Option[String]) =>
      whetherEoriDetailsCorrect
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(getPage(eori, name, formWithErrors))),
          {
            case Yes =>
              verifiedEmailAddressService
                .getVerifiedEmailAddress(eori)
                .map {
                  case Left(error)    =>
                    logger.warn("Error submitting a verified email", error.toException)
                    errorHandler.errorResult()
                  case Right(None)    =>
                    Redirect(viewConfig.customsEmailFrontendUrl)
                  case Right(Some(_)) =>
                    Redirect(commonRoutes.ChooseClaimTypeController.show)
                }
            case No  =>
              Future.successful(Redirect(baseRoutes.StartController.start()).withNewSession)
          }
        )
    }(resultIfUnsupportedUser = Redirect(baseRoutes.StartController.start()))
  }
}

object CheckEoriDetailsController {

  val checkEoriDetailsKey: String = "check-eori-details"

  val whetherEoriDetailsCorrect: Form[YesNo] = YesOrNoQuestionForm(checkEoriDetailsKey)
}
