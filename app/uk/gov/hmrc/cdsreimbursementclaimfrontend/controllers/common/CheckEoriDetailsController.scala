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

import com.google.inject.Inject
import play.api.Configuration
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthRetrievalsAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.{routes => commonRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.CheckEoriDetailsController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
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
  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData,
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
    with WithAuthRetrievalsAndSessionDataAction
    with SessionUpdates
    with Logging {

  protected def getPage(
    signedInUserDetails: SignedInUserDetails,
    form: Form[YesNo]
  )(implicit
    request: Request[_],
    messages: Messages
  ): Appendable = checkEoriDetailsPage(
    signedInUserDetails,
    form,
    routes.CheckEoriDetailsController.submit()
  )

  def show(): Action[AnyContent] = authenticatedActionWithRetrievedDataAndSessionData { implicit request =>
    request.signedInUserDetails
      .fold(Redirect(baseRoutes.StartController.start()))(user => Ok(getPage(user, whetherEoriDetailsCorrect)))
  }

  def submit(): Action[AnyContent] = authenticatedActionWithRetrievedDataAndSessionData.async { implicit request =>
    request.signedInUserDetails
      .map { user =>
        whetherEoriDetailsCorrect
          .bindFromRequest()
          .fold(
            formWithErrors => Future.successful(BadRequest(getPage(user, formWithErrors))),
            {
              case Yes =>
                Future.successful(
                  Redirect(
                    if (featureSwitch.isEnabled(Feature.RejectedGoods))
                      commonRoutes.ChooseClaimTypeController.show()
                    else
                      commonRoutes.SelectTypeOfClaimController.show()
                  )
                )
              case No  => Future.successful(Redirect(viewConfig.ggSignOut))
            }
          )
      }
      .getOrElse(Future.successful(Redirect(baseRoutes.StartController.start())))
  }
}

object CheckEoriDetailsController {

  val checkEoriDetailsKey: String = "check-eori-details"

  val whetherEoriDetailsCorrect: Form[YesNo] = YesOrNoQuestionForm(checkEoriDetailsKey)
}
