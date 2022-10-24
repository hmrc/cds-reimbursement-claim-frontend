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

import cats.data.EitherT
import cats.implicits.catsSyntaxEq
import cats.instances.future.catsStdInstancesForFuture
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.withAnswers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ImporterEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{common => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterImporterEoriNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  claimService: ClaimService,
  val controllerComponents: MessagesControllerComponents,
  enterImporterEoriNumberPage: pages.enter_importer_eori_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendBaseController
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  val eoriNumberFormKey: String = "enter-importer-eori-number"

  implicit val journey: JourneyBindable = JourneyBindable.Scheduled

  implicit val dataExtractor: DraftClaim => Option[ImporterEoriNumberAnswer] = _.importerEoriNumberAnswer

  val enterImporterEoriNumber: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ImporterEoriNumberAnswer] { (_, answers) =>
        val emptyForm              = Forms.eoriNumberForm(eoriNumberFormKey)
        val filledForm: Form[Eori] =
          answers.fold(emptyForm)(importerEoriNumberAnswer => emptyForm.fill(importerEoriNumberAnswer.value))
        Ok(
          enterImporterEoriNumberPage(
            filledForm,
            routes.EnterImporterEoriNumberController.enterImporterEoriNumberSubmit
          )
        )
      }
    }

  val enterImporterEoriNumberSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ImporterEoriNumberAnswer] { (fillingOutClaim, _) =>
        Forms
          .eoriNumberForm(eoriNumberFormKey)
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterImporterEoriNumberPage(
                  requestFormWithErrors.fill(Eori("")),
                  routes.EnterImporterEoriNumberController.enterImporterEoriNumberSubmit
                )
              ),
            importerEoriNumber => {

              def updateJourney() = EitherT {
                val updatedJourney =
                  FillingOutClaim.from(fillingOutClaim)(
                    _.copy(importerEoriNumberAnswer = Some(ImporterEoriNumberAnswer(importerEoriNumber)))
                  )
                updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney)))
              }

              def checkWhetherConsigneeEORIsMatch = for {
                mrn         <-
                  EitherT
                    .fromOption[Future](fillingOutClaim.draftClaim.movementReferenceNumber, Error("could not get MRN"))
                declaration <- claimService.getDisplayDeclaration(mrn)
              } yield declaration
                .flatMap(_.displayResponseDetail.consigneeDetails)
                .exists(_.consigneeEORI === importerEoriNumber.value)

              val updateAndRedirect = for {
                eorisMatch <- checkWhetherConsigneeEORIsMatch
                                .leftMap(_ => Redirect(controllers.routes.IneligibleController.ineligible()))
                status     <-
                  if (eorisMatch)
                    updateJourney()
                      .map(_ =>
                        Redirect(
                          routes.EnterDeclarantEoriNumberController.enterDeclarantEoriNumber
                        )
                      )
                      .leftMap(logAndDisplayError("could not get importer eori number"))
                  else
                    EitherT.rightT[Future, Result](Redirect(controllers.routes.IneligibleController.ineligible()))
              } yield status

              updateAndRedirect.merge
            }
          )
      }
    }
}
