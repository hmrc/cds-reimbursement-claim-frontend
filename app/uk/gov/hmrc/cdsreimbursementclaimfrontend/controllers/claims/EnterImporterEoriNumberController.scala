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

import cats.instances.future.catsStdInstancesForFuture
import play.api.data.Form
import cats.data.EitherT
import play.api.data.Forms.{mapping, text}
import play.api.data._
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.withAnswersAndRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

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

  implicit val dataExtractor: DraftClaim => Option[ImporterEoriNumber] = _.importerEoriNumberAnswer

  def enterImporterEoriNumber(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[ImporterEoriNumber] { (_, answers, router) =>
        val emptyForm                            = EnterImporterEoriNumberController.eoriNumberForm
        val filledForm: Form[ImporterEoriNumber] = answers.fold(emptyForm)(emptyForm.fill)
        Ok(enterImporterEoriNumberPage(filledForm, router))
      }
    }

  def enterImporterEoriNumberSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[ImporterEoriNumber] { (fillingOutClaim, _, router) =>
        EnterImporterEoriNumberController.eoriNumberForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterImporterEoriNumberPage(
                  requestFormWithErrors,
                  router
                )
              ),
            importerEoriNumber => {

              val newDraftClaim =
                fillingOutClaim.draftClaim.copy(importerEoriNumberAnswer = Some(importerEoriNumber))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                logAndDisplayError("could not get importer eori number"),
                _ => Redirect(routes.EnterDeclarantEoriNumberController.enterDeclarantEoriNumber(journey))
              )

            }
          )
      }
    }

}

object EnterImporterEoriNumberController {

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
