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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.TemplateMeta
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

  lazy val meta: TemplateMeta[DummyReferenceNumberController] = implicitly[TemplateMeta[DummyReferenceNumberController]]

  def show(): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      request.extract(
        {
          case (_, claim: PreFillingOutClaim) =>
            claim.claimType.fold(Redirect(routes.SelectNumberOfClaimsController.show())) { claimType =>
              val form = EnterMovementReferenceNumberController
                .movementReferenceNumberForm(meta.getKey(claimType), isEntryNumberEnabled = true)
              Ok(dummyEnterMovementReferenceNumberPage(meta.getKey(claimType), meta.submitUrl, form))
            }
          case (_, claim: FillingOutClaim)    =>
            claim.draftClaim.movementReferenceNumber.fold(errorHandler.errorResult()) { mrn =>
              val form = EnterMovementReferenceNumberController
                .movementReferenceNumberForm(meta.getKey(claim.claimType), isEntryNumberEnabled = true)
                .fill(MovementReferenceNumber(mrn))
              Ok(dummyEnterMovementReferenceNumberPage(meta.getKey(claim.claimType), meta.submitUrl, form))
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
                  .movementReferenceNumberForm(meta.getKey(claimType), isEntryNumberEnabled = true)
                  .bindFromRequest()
                  .fold(
                    formErrors =>
                      Future.successful(
                        BadRequest(
                          dummyEnterMovementReferenceNumberPage(meta.getKey(claimType), meta.submitUrl, formErrors)
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
                          _ => Redirect(meta.nextUrl(claimType, mrn))
                        )
                      )
                  )
            }
          case (_, claim: FillingOutClaim)          =>
            EnterMovementReferenceNumberController
              .movementReferenceNumberForm(meta.getKey(claim.claimType), isEntryNumberEnabled = true)
              .bindFromRequest()
              .fold(
                formErrors =>
                  Future.successful(
                    BadRequest(
                      dummyEnterMovementReferenceNumberPage(meta.getKey(claim.claimType), meta.submitUrl, formErrors)
                    )
                  ),
                mrn =>
                  updateSession(sessionStore, request)(
                    _.copy(journeyStatus = FillingOutClaim.of(claim)(_.copy(movementReferenceNumber = mrn.some)).some)
                  ).map(
                    _.fold(
                      _ => errorHandler.errorResult(),
                      _ => Redirect(meta.nextUrl(claim.claimType, mrn))
                    )
                  )
              )
          case (_, claim: CompletedFillingOutClaim) =>
            EnterMovementReferenceNumberController
              .movementReferenceNumberForm(meta.getKey(claim.claimType), isEntryNumberEnabled = true)
              .bindFromRequest()
              .fold(
                formErrors =>
                  Future.successful(
                    BadRequest(
                      dummyEnterMovementReferenceNumberPage(meta.getKey(claim.claimType), meta.submitUrl, formErrors)
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

    //TODO: test implementation for components: TC, withAnswers, controllers
    //TODO: things to consider:
    //TODO: 1. what if the user is in a schedule journey and the user tries to hit a single journey endpoint? resolved by action filters
    //TODO: 2. how do we introduce feature flags? that is, block users from certain journeys (can the feature flag be applied at the TC level, instead of controller classes??)
    //TODO: 3. how do we handle wiring that is based on user submitted answers as well as claim-type, and movement reference number? (bypass the TC) ie value based journey evaluation (Not type driven)

    }
}
