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
import cats.implicits.catsSyntaxEq
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.withAnswersAndRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{JourneyBindable, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{answers, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ImporterEoriNumberAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterImporterEoriNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  claimService: ClaimService,
  cc: MessagesControllerComponents,
  enterImporterEoriNumberPage: pages.enter_importer_eori_number
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftClaim => Option[ImporterEoriNumberAnswer] = _.importerEoriNumberAnswer

  def enterImporterEoriNumber(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[ImporterEoriNumberAnswer] { (_, answers, router) =>
        val emptyForm              = EnterImporterEoriNumberController.eoriNumberForm
        val filledForm: Form[Eori] =
          answers.fold(emptyForm)(importerEoriNumberAnswer => emptyForm.fill(importerEoriNumberAnswer.value))
        Ok(enterImporterEoriNumberPage(filledForm, router.submitUrlForEnterImporterEoriNumber()))
      }
    }

  def enterImporterEoriNumberSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[ImporterEoriNumberAnswer] { (fillingOutClaim, _, router) =>
        EnterImporterEoriNumberController.eoriNumberForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterImporterEoriNumberPage(
                  requestFormWithErrors.fill(Eori("")),
                  router.submitUrlForEnterImporterEoriNumber()
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
                      .map(_ => Redirect(routes.EnterDeclarantEoriNumberController.enterDeclarantEoriNumber(journey)))
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

object EnterImporterEoriNumberController {

  val eoriNumberFormKey: String = "enter-importer-eori-number"

  val eoriNumberForm: Form[Eori] = Form(
    mapping(
      eoriNumberFormKey -> nonEmptyText
        .verifying("invalid.number", str => str.isEmpty || Eori(str).isValid)
    )(Eori.apply)(Eori.unapply)
  )

}
