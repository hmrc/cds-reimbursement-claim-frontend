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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms

import scala.concurrent.Future

@Singleton
class CheckDuplicateDeclarationDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  checkDeclarationDetailsPage: pages.check_declaration_details,
  enterDuplicateMovementReferenceNumberPage: pages.enter_duplicate_movement_reference_number
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val duplicateDeclarationExtractor: DraftClaim => Option[DisplayDeclaration] =
    _.duplicateDisplayDeclaration

  def show(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DisplayDeclaration] { (_, maybeDeclaration, router) =>
        val postAction: Call                = router.submitUrlForCheckDuplicateDeclarationDetails()
        implicit val subKey: Option[String] = router.subKey
        maybeDeclaration.fold(Redirect(baseRoutes.IneligibleController.ineligible()))(declaration =>
          Ok(
            checkDeclarationDetailsPage(
              declaration,
              checkDeclarationDetailsAnswerForm,
              isDuplicate = true,
              postAction
            )
          )
        )
      }
    }

  def submit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DisplayDeclaration] { (_, answer, router) =>
        val postAction: Call                = router.submitUrlForCheckDuplicateDeclarationDetails()
        implicit val subKey: Option[String] = router.subKey
        checkDeclarationDetailsAnswerForm
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
                        isDuplicate = true,
                        postAction
                      )
                    )
                  )
                )
                .getOrElse(Future.successful(errorHandler.errorResult())),
            {
              case YesNo.No  =>
                Ok(
                  enterDuplicateMovementReferenceNumberPage(
                    Forms.enterDuplicateMrnWithNoCheck,
                    router.refNumberKey,
                    OverpaymentsRoutes.EnterDuplicateMovementReferenceNumberController
                      .enterDuplicateMrnSubmit(router.journeyBindable)
                  )
                )
              case YesNo.Yes => Redirect(router.nextPageForCheckDuplicateDeclarationDetails())
            }
          )
      }
    }
}
