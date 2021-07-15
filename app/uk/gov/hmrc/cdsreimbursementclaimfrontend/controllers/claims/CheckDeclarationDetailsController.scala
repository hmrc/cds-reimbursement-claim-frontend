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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, TemporaryJourneyExtractor}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, SessionData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.Future

@Singleton
class CheckDeclarationDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  checkDeclarationDetailsPage: pages.check_declaration_details
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withPossibleDeclaration(
    f: (
      SessionData,
      FillingOutClaim,
      Option[DisplayDeclaration]
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({ case (s, r @ FillingOutClaim(_, _, c: DraftClaim)) =>
      val maybeDisplayDeclaration = c.fold(_.displayDeclaration)
      f(s, r, maybeDisplayDeclaration)
    })

  private def withDuplicateDeclaration(
    f: (
      SessionData,
      FillingOutClaim,
      Option[DisplayDeclaration]
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({ case (s, r @ FillingOutClaim(_, _, c: DraftClaim)) =>
      val maybeDisplayDeclaration = c.fold(_.duplicateDisplayDeclaration)
      f(s, r, maybeDisplayDeclaration)
    })

  def checkDetails(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withPossibleDeclaration { (_, _, maybeDeclaration) =>
      maybeDeclaration.fold(
        Redirect(routes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds())
      )(declaration =>
        Ok(
          checkDeclarationDetailsPage(
            declaration,
            routes.CheckDeclarationDetailsController.checkDetailsSubmit()
          )
        )
      )
    }
  }

  def checkDetailsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType(TemporaryJourneyExtractor.extractJourney))
  }

  def checkDuplicateDetails(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withDuplicateDeclaration { (_, _, maybeDeclaration) =>
      maybeDeclaration.fold(
        Redirect(routes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds())
      )(declaration =>
        Ok(
          checkDeclarationDetailsPage(
            declaration,
            routes.CheckDeclarationDetailsController.checkDuplicateDetailsSubmit()
          )
        )
      )
    }
  }

  def checkDuplicateDetailsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Redirect(routes.EnterCommoditiesDetailsController.enterCommoditiesDetails(TemporaryJourneyExtractor.extractJourney))
  }

}
