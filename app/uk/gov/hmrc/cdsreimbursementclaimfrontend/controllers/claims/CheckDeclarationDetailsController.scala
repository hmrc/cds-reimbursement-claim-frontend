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
import cats.implicits._
import cats.implicits.catsSyntaxEq
import com.google.inject.Inject
import com.google.inject.Singleton
import julienrf.json.derived
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.number
import play.api.libs.json.OFormat
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class CheckDeclarationDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  checkDeclarationDetailsPage: pages.check_declaration_details
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val declarationExtractor: DraftC285Claim => Option[DisplayDeclaration] = _.displayDeclaration

  def show(implicit journey: JourneyBindable): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      val isDuplicate: Boolean = false
      withAnswersAndRoutes[DisplayDeclaration] { (_, maybeDeclaration, router) =>
        implicit val reimbursementRoutes: ReimbursementRoutes = router
        maybeDeclaration.fold(
          Redirect(routes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds())
        )(declaration =>
          Ok(
            checkDeclarationDetailsPage(declaration, checkDeclarationDetailsAnswerForm, isDuplicate)
          )
        )
      }
  }

  def submit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      val isDuplicate: Boolean = false
      withAnswersAndRoutes[DisplayDeclaration] { (fillingOutClaim, answer, router) =>
        implicit val reimbursementRoutes: ReimbursementRoutes = router
        CheckDeclarationDetailsController.checkDeclarationDetailsAnswerForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              answer
                .map(declaration =>
                  Future.successful(
                    BadRequest(checkDeclarationDetailsPage(declaration, formWithErrors, isDuplicate))
                  )
                )
                .getOrElse(Future.successful(errorHandler.errorResult())),
            { answer =>
              val updatedJourney =
                FillingOutClaim.of(fillingOutClaim)(_.copy(checkDeclarationDetailsAnswer = Some(answer)))

              val result = EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap(_ => Error("could not update session"))

              result.fold(
                logAndDisplayError("could not get radio button details"),
                _ =>
                  Redirect(
                    router.nextPageForCheckDeclarationDetails(
                      answer,
                      fillingOutClaim.draftClaim.associatedMRNsAnswer.isDefined
                    )
                  )
              )
            }
          )
      }
    }
}

object CheckDeclarationDetailsController {

  sealed trait CheckDeclarationDetailsAnswer extends Product with Serializable

  case object DeclarationAnswersAreCorrect extends CheckDeclarationDetailsAnswer
  case object DeclarationAnswersAreIncorrect extends CheckDeclarationDetailsAnswer

  implicit val checkDeclarationDetailsAnswerFormat: OFormat[CheckDeclarationDetailsAnswer] =
    derived.oformat[CheckDeclarationDetailsAnswer]()

  val checkDeclarationDetailsKey: String = "check-declaration-details"

  val checkDeclarationDetailsAnswerForm: Form[CheckDeclarationDetailsAnswer] =
    Form(
      mapping(
        checkDeclarationDetailsKey -> number
          .verifying("error.invalid", a => a === 0 || a === 1)
          .transform[CheckDeclarationDetailsAnswer](
            value =>
              if (value === 0) DeclarationAnswersAreCorrect
              else DeclarationAnswersAreIncorrect,
            {
              case DeclarationAnswersAreCorrect   => 0
              case DeclarationAnswersAreIncorrect => 1
            }
          )
      )(identity)(Some(_))
    )
}
