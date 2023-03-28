/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

import cats.data.EitherT
import cats.syntax.all._
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.movementReferenceNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpayments.EnterMovementReferenceNumberMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.MRNMultipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  claimService: ClaimService,
  enterMovementReferenceNumberPage: pages.enter_movement_reference_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val controllerComponents: MessagesControllerComponents)
    extends FrontendBaseController
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging
    with EnterMovementReferenceNumberMixin {

  import cats.data.EitherT._
  implicit val dataExtractor: DraftClaim => Option[MRN] = _.movementReferenceNumber

  def showMRNPage(status: Status, formWithErrors: Form[MRN])(implicit
    request: RequestWithSessionData[AnyContent]
  ): Result =
    status(
      enterMovementReferenceNumberPage(
        formWithErrors,
        MRNMultipleRoutes.subKey,
        routes.EnterMovementReferenceNumberController.enterMrnSubmit
      )
    )

  val enterJourneyMrn: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[MRN] { (_, previousAnswer) =>
        val emptyForm = movementReferenceNumberForm
        val form      = previousAnswer.fold(emptyForm)(emptyForm.fill)
        showMRNPage(Ok, form)
      }
    }

  val enterMrnSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[MRN] { (fillingOutClaim, previousAnswer) =>
        val draftClaim = fillingOutClaim.draftClaim
        movementReferenceNumberForm
          .bindFromRequest()
          .fold(
            formWithErrors => showMRNPage(BadRequest, formWithErrors),
            mrnNumber => {

              def getDeclaration(mrn: MRN): EitherT[Future, Error, Option[DisplayDeclaration]] =
                claimService
                  .getDisplayDeclaration(mrn)
                  .leftMap(_ => Error("Could not get declaration"))

              def nextPage(MRNJourneyFlow: EitherT[Future, Error, MrnJourney]): Future[Result] =
                MRNJourneyFlow.fold(
                  e => {
                    logger.warn(s"Mrn submission failed: ${e.message}")
                    Redirect(baseRoutes.IneligibleController.ineligible())
                  },
                  mrnJourney =>
                    Redirect(
                      getRoutes(getTypeOfClaim(draftClaim), JourneyBindable.Multiple).nextPageForEnterMRN(mrnJourney)
                    )
                )

              val isSameAsPrevious: Boolean =
                previousAnswer.exists(_.value === mrnNumber.value)

              if (draftClaim.associatedMRNsAnswer.list.exists(_ === mrnNumber)) {
                val (key, message) = ("enter-movement-reference-number", "multiple.error.existingMRN")
                showMRNPage(BadRequest, movementReferenceNumberForm.withError(key, message))
              } else if (isSameAsPrevious && draftClaim.isComplete)
                Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers)
              else if (isSameAsPrevious) {
                val MRNJourneyFlow: EitherT[Future, Error, MrnJourney] =
                  for {
                    acc14          <- getDeclaration(mrnNumber)
                    mrnJourneyFlow <-
                      fromEither[Future](evaluateMrnJourneyFlow(fillingOutClaim.signedInUserDetails, acc14))
                        .leftMap(_ => Error("could not evaluate MRN flow"))
                  } yield mrnJourneyFlow

                nextPage(MRNJourneyFlow)
              } else {
                val result: EitherT[Future, Error, MrnJourney] =
                  for {
                    maybeAcc14     <- if (isSameAsPrevious)
                                        EitherT
                                          .fromOption[Future](
                                            draftClaim.displayDeclaration,
                                            mrnNumber
                                          )
                                          .map(Some.apply)
                                          .leftFlatMap(getDeclaration)
                                      else
                                        getDeclaration(mrnNumber)
                    mrnJourneyFlow <-
                      fromEither[Future](evaluateMrnJourneyFlow(fillingOutClaim.signedInUserDetails, maybeAcc14))
                        .leftMap(_ => Error("could not evaluate MRN flow"))

                    declaration   <-
                      fromOption[Future](maybeAcc14, Error("could not unbox display declaration"))
                    contactDetails = extractContactDetails(
                                       declaration,
                                       mrnJourneyFlow,
                                       fillingOutClaim.signedInUserDetails.verifiedEmail
                                     )

                    contactAddress = extractContactAddress(declaration, mrnJourneyFlow)
                    _             <-
                      EitherT(
                        updateSession(sessionStore, request)(
                          if (draftClaim.isComplete)
                            renewMrnAndAcc14(fillingOutClaim, mrnNumber, declaration, contactDetails, contactAddress)
                          else
                            updateMrnAndAcc14(fillingOutClaim, mrnNumber, declaration, contactDetails, contactAddress)
                        )
                      ).leftMap(_ => Error("Could not save Display Declaration"))
                  } yield mrnJourneyFlow
                nextPage(result)
              }
            }
          )
      }
    }

}
