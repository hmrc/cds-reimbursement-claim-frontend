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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.CheckDeclarationDetailsController.checkDeclarationDetailsAnswerForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.Future

@Singleton
class CheckDeclarationDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val controllerComponents: MessagesControllerComponents,
  checkDeclarationDetailsPage: pages.check_declaration_details
)(implicit viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendBaseController
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val journey: JourneyBindable = JourneyBindable.Scheduled

  implicit val declarationExtractor: DraftClaim => Option[DisplayDeclaration] = _.displayDeclaration

  def show(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswersAndRoutes[DisplayDeclaration] { (_, maybeDeclaration, router) =>
      val postAction: Call                = router.submitUrlForCheckDeclarationDetails()
      implicit val subKey: Option[String] = router.subKey
      maybeDeclaration.fold(Redirect(baseRoutes.IneligibleController.ineligible()))(declaration =>
        Ok(
          checkDeclarationDetailsPage(
            declaration,
            checkDeclarationDetailsAnswerForm,
            isDuplicate = false,
            postAction
          )
        )
      )
    }
  }

  def submit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DisplayDeclaration] { (fillingOutClaim, answer, router) =>
        val postAction: Call                = router.submitUrlForCheckDeclarationDetails()
        implicit val subKey: Option[String] = router.subKey
        CheckDeclarationDetailsController.checkDeclarationDetailsAnswerForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              answer
                .map(declaration =>
                  Future.successful(
                    BadRequest(
                      checkDeclarationDetailsPage(
                        declaration,
                        formWithErrors,
                        isDuplicate = false,
                        postAction
                      )
                    )
                  )
                )
                .getOrElse(Future.successful(errorHandler.errorResult())),
            answer =>
              Redirect(
                router.nextPageForCheckDeclarationDetails(
                  answer,
                  fillingOutClaim.draftClaim.associatedMRNsAnswer.isDefined
                )
              )
          )
      }
    }
}

object CheckDeclarationDetailsController {

  val checkDeclarationDetailsKey: String = "check-declaration-details"

  val checkDeclarationDetailsAnswerForm: Form[YesNo] =
    YesOrNoQuestionForm(checkDeclarationDetailsKey)
}
