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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.movementReferenceNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.PreFillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.SessionService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.TemplateMeta
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.TemplateMeta.syntax._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class DummyReferenceNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  sessionService: SessionService,
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

  def show(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      //TODO: we need a method like `withValidJourney` or withAnswers` but somehow pass case PreFillingOutClaim => preFillingOutClaim
      //TODO: simplify this by getting the session data from the request object
      sessionService
        .getAnswers({ case claim: PreFillingOutClaim => claim.claimType })
        .fold(
          error => {
            logger.warn(error.message)
            errorHandler.errorResult()
          },
          _.map { claimType =>
            claimType.showPage[DummyReferenceNumberController]((key, submitUrl) =>
              Ok(
                dummyEnterMovementReferenceNumberPage(
                  key,
                  submitUrl,
                  movementReferenceNumberForm(key, isEntryNumberEnabled = true)
                )
              )
            )
          } getOrElse errorHandler.errorResult
        )
    }

  def submit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      val meta = implicitly[TemplateMeta[DummyReferenceNumberController]]

      //TODO: for every http request, we already have a handle on the session data - there is no need to go to the database to grab the journey status and answers it's already available in request
      //TODO: we need a method like `withValidJourney` or withAnswers` but somehow pass case PreFillingOutClaim => preFillingOutClaim
      //TODO: this method will also handle the situation where a valid journey status is not present so we do not need to do the error redirect in every controller submit method
      //TODO: after this refactoring, all we need to do then is get the submitted answer and update the session data cache
      //TODO: updating the session data cache should ebe done via a simple method interface: something like def updateSessionData(currentSessionData, newAnswer): EitherT[Future, Error, Unit]
      //TODO: we fold on the updateSessionData result, handle any errors if any or redirect to next page in the journey
      //TODO: how to handle code re-use within a journey
      //TODO: what about CYA action: nextUrl must be have the type of the object in the session cache passed in so that it can match the correct url
      //TODO: test implementation for components: TC, withAnswers, controllers
      //TODO: things to consider:
      //TODO: 1. what if the user is in a schedule journey and the user tries to hit a single journey endpoint?
      //TODO: 2. how do we introduce feature flags? that is, block users from certain journeys (can the feature flag be applied at the TC level, instead of controller classes??)
      //TODO: 3. how do we handle wiring that is based on user submitted answers as well as claim-type, and movement reference number? (bypass the TC) ie value based journey evaluation (Not type driven)
      sessionService
        .getAnswers({ case preFillingOutClaim: PreFillingOutClaim => preFillingOutClaim })
        .leftMap(e => Error(e.message))
        .foldF(
          _ => Future.successful(errorHandler.errorResult()),
          preFillingOutClaim =>
            preFillingOutClaim.claimType match {
              case Some(claimType) =>
                EnterMovementReferenceNumberController
                  .movementReferenceNumberForm(meta.getKey(claimType), isEntryNumberEnabled = true)
                  .bindFromRequest()
                  .fold(
                    _ => Future.successful(errorHandler.errorResult()),
                    mrn =>
                      request.sessionData.fold(Future.successful(errorHandler.errorResult()))(session =>
                        sessionService
                          .persist(
                            session.copy(journeyStatus =
                              PreFillingOutClaim
                                .toFillingOutClaim(
                                  preFillingOutClaim.ggCredId,
                                  preFillingOutClaim.signedInUserDetails,
                                  claimType
                                )
                                .some
                            )
                          )
                          .foldF(
                            _ => Future.successful(errorHandler.errorResult()),
                            _ => Future.successful(Redirect(meta.nextUrl(claimType, mrn)))
                          )
                      )
                  )
              case None            => Future.successful(errorHandler.errorResult())
            }
        )

    }
}
