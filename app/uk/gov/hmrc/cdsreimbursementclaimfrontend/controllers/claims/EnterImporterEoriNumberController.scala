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
import play.api.data.Forms.{mapping, text}
import play.api.data._
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ImporterEoriNumberAnswer.{CompleteImporterEoriNumberAnswer, IncompleteImporterEoriNumberAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterImporterEoriNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  enterImporterEoriNumberPage: pages.enter_importer_eori_number
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withImporterEoriNumberAnswer(
    f: (
      SessionData,
      FillingOutClaim,
      ImporterEoriNumberAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({
      case (
            sessionData,
            fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
          ) =>
        val maybeImporterEoriNumberAnswer = draftClaim.fold(
          _.importerEoriNumberAnswer
        )
        maybeImporterEoriNumberAnswer.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteImporterEoriNumberAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
    })

  def enterImporterEoriNumber(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withImporterEoriNumberAnswer { (_, _, answers) =>
      answers.fold(
        ifIncomplete =>
          ifIncomplete.importerEoriNumber match {
            case Some(importerEoriNumber) =>
              Ok(enterImporterEoriNumberPage(EnterImporterEoriNumberController.eoriNumberForm.fill(importerEoriNumber)))
            case None                     =>
              Ok(enterImporterEoriNumberPage(EnterImporterEoriNumberController.eoriNumberForm))
          },
        ifComplete =>
          Ok(
            enterImporterEoriNumberPage(
              EnterImporterEoriNumberController.eoriNumberForm.fill(ifComplete.importerEoriNumber)
            )
          )
      )
    }
  }

  def enterImporterEoriNumberSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withImporterEoriNumberAnswer { (_, fillingOutClaim, answers) =>
        EnterImporterEoriNumberController.eoriNumberForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterImporterEoriNumberPage(
                  requestFormWithErrors
                )
              ),
            importerEoriNumber => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteImporterEoriNumberAnswer(
                    importerEoriNumber
                  ),
                complete => complete.copy(importerEoriNumber = importerEoriNumber)
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(importerEoriNumberAnswer = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                logAndDisplayError("could not get importer eori number"),
                _ => Redirect(routes.EnterDeclarantEoriNumberController.enterDeclarantEoriNumber())
              )
            }
          )
      }
  }

}

object EnterImporterEoriNumberController {

  final case class ImporterEoriNumber(value: Eori)

  object ImporterEoriNumber {
    implicit val format: OFormat[ImporterEoriNumber] = Json.format[ImporterEoriNumber]
  }

  val eoriNumberMapping: Mapping[Eori] =
    text
      .verifying("invalid.number", str => Eori.isValid(str))
      .transform[Eori](str => Eori(str), eori => eori.value)

  val eoriNumberForm: Form[ImporterEoriNumber] = Form(
    mapping(
      "enter-importer-eori-number" -> eoriNumberMapping
    )(ImporterEoriNumber.apply)(ImporterEoriNumber.unapply)
  )

}
