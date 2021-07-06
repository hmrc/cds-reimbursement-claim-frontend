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

import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.{CompletedFillingOutClaim, FillingOutClaim, PreFillingOutClaim}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.UserJourney.SelectBasisForClaimControllerTemplateMeta.DummyReferenceNumberControllerTemplateMeta
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.{JourneyParameters, UserJourney}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class DummyReferenceNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  dummyEnterMovementReferenceNumberPage: pages.dummy_enter_movement_reference_number
)(implicit
  viewConfig: ViewConfig,
  errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  ec: ExecutionContext
) extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  lazy val journey: UserJourney[DummyReferenceNumberController, JourneyParameters] =
    implicitly[UserJourney[DummyReferenceNumberController, JourneyParameters]]

  def show(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      request.extract(
        {
          case (_, claim: PreFillingOutClaim) =>
            claim.claimType.fold(Redirect(routes.SelectNumberOfClaimsController.show())) { claimType =>
              val form = EnterMovementReferenceNumberController
                .movementReferenceNumberForm(journey.getKey(claimType), isEntryNumberEnabled = true)
              Ok(dummyEnterMovementReferenceNumberPage(journey.getKey(claimType), journey.submitUrl, form))
            }
          case (_, claim: FillingOutClaim)    =>
            claim.draftClaim.movementReferenceNumber.fold(errorHandler.errorResult()) { mrn =>
              val form = EnterMovementReferenceNumberController
                .movementReferenceNumberForm(journey.getKey(claim.claimType), isEntryNumberEnabled = true)
                .fill(MovementReferenceNumber(mrn))
              Ok(dummyEnterMovementReferenceNumberPage(journey.getKey(claim.claimType), journey.submitUrl, form))
            }
        },
        request.startNewJourney
      )
    }

  def submit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.extract(
        {
          case (_, claim: PreFillingOutClaim)       =>
            claim.claimType.fold(Future.successful(Redirect(routes.SelectNumberOfClaimsController.show()))) {
              claimType =>
                EnterMovementReferenceNumberController
                  .movementReferenceNumberForm(journey.getKey(claimType), isEntryNumberEnabled = true)
                  .bindFromRequest()
                  .fold(
                    formErrors =>
                      Future.successful(
                        BadRequest(
                          dummyEnterMovementReferenceNumberPage(
                            journey.getKey(claimType),
                            journey.submitUrl,
                            formErrors
                          )
                        )
                      ),
                    mrn =>
                      updateSession(sessionStore, request)(
                        _.copy(journeyStatus =
                          FillingOutClaim
                            .of(claim.toFillingOutClaim(claimType))(_.copy(movementReferenceNumber = mrn.some))
                            .some
                        )
                      ).map(
                        _.fold(
                          _ => errorHandler.errorResult(),
                          _ => Redirect(journey.nextUrl(claimType, mrn))
                        )
                      )
                  )
            }
          case (_, claim: FillingOutClaim)          =>
            EnterMovementReferenceNumberController
              .movementReferenceNumberForm(journey.getKey(claim.claimType), isEntryNumberEnabled = true)
              .bindFromRequest()
              .fold(
                formErrors =>
                  Future.successful(
                    BadRequest(
                      dummyEnterMovementReferenceNumberPage(
                        journey.getKey(claim.claimType),
                        journey.submitUrl,
                        formErrors
                      )
                    )
                  ),
                mrn =>
                  updateSession(sessionStore, request)(
                    _.copy(journeyStatus = FillingOutClaim.of(claim)(_.copy(movementReferenceNumber = mrn.some)).some)
                  ).map(
                    _.fold(
                      _ => errorHandler.errorResult(),
                      _ => Redirect(journey.nextUrl(claim.claimType, mrn))
                    )
                  )
              )
          case (_, claim: CompletedFillingOutClaim) =>
            EnterMovementReferenceNumberController
              .movementReferenceNumberForm(journey.getKey(claim.claimType), isEntryNumberEnabled = true)
              .bindFromRequest()
              .fold(
                formErrors =>
                  Future.successful(
                    BadRequest(
                      dummyEnterMovementReferenceNumberPage(
                        journey.getKey(claim.claimType),
                        journey.submitUrl,
                        formErrors
                      )
                    )
                  ),
                mrn =>
                  updateSession(sessionStore, request)(
                    _.copy(journeyStatus =
                      CompletedFillingOutClaim.of(claim)(_.copy(movementReferenceNumber = mrn)).some
                    )
                  ).map(
                    _.fold(
                      _ => errorHandler.errorResult(),
                      _ => Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
                    )
                  )
              )
        },
        Future.successful(request.startNewJourney)
      )
    }
}
