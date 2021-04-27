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

import cats.syntax.eq._
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.i18n.Messages
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import play.twirl.api.HtmlFormat.Appendable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckEoriDetailsController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.check_eori_details
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.Singleton

@Singleton
class CheckEoriDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  val featureSwitch: FeatureSwitchService,
  cc: MessagesControllerComponents,
  checkEoriDetailsPage: check_eori_details
)(implicit viewConfig: ViewConfig)
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
    routes.CheckEoriDetailsController.submit(),
    baseRoutes.LandingPageController.landing()
  )

  def show(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.signedInUserDetails
      .fold(Redirect(baseRoutes.StartController.start()))(user => Ok(getPage(user, checkEoriDetailsAnswerForm)))
  }

  def submit(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.signedInUserDetails
      .map { user =>
        checkEoriDetailsAnswerForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(getPage(user, formWithErrors)),
            formOk =>
              formOk match {
                case EoriDetailsAreCorrect   =>
                  featureSwitch.BulkClaim.isEnabled() match {
                    case true  => Redirect(routes.SelectNumberOfClaimsController.show(false))
                    case false => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                  }
                case EoriDetailsAreIncorrect => Redirect(viewConfig.ggSignOut)
              }
          )
      }
      .getOrElse(Redirect(baseRoutes.StartController.start()))
  }

}

object CheckEoriDetailsController {

  sealed trait CheckEoriDetailsAnswer extends Product with Serializable

  case object EoriDetailsAreCorrect extends CheckEoriDetailsAnswer
  case object EoriDetailsAreIncorrect extends CheckEoriDetailsAnswer

  val dataKey = "check-eori-details"

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
