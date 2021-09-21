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

import com.google.inject.{Inject, Singleton}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

@Singleton
class CheckDuplicateDeclarationDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  checkDeclarationDetailsPage: pages.check_declaration_details
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val duplicateDeclarationExtractor: DraftC285Claim => Option[DisplayDeclaration] =
    _.duplicateDisplayDeclaration

  def show(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      val isDuplicate: Boolean = true
      withAnswersAndRoutes[DisplayDeclaration] { (_, maybeDeclaration, router) =>
        implicit val reimbursementRoutes: ReimbursementRoutes = router
        maybeDeclaration.fold(
          Redirect(routes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds())
        )(declaration => Ok(checkDeclarationDetailsPage(declaration, checkDeclarationDetailsAnswerForm, isDuplicate)))
      }
    }

  def submit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DisplayDeclaration] { (_, _, router) =>
        Redirect(router.nextPageForCheckDuplicateDeclarationDetails())
      }
    }

}
