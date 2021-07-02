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
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import play.api.data.Form
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.movementReferenceNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.InitialClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MovementReferenceNumber
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
      sessionService
        .getAnswers({ case claim: InitialClaim => claim.claimType })
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

      def logError(message: String) = {
        logger.warn(message)
        errorHandler.errorResult()
      }

      val errorOrForm = for {
        claim     <- sessionService.getAnswers({ case claim: InitialClaim => claim })
                          .leftMap(e =>  logError(e.message))
        claimType <- EitherT.fromOption[Future](claim.claimType, logError("No claim type"))
        mrn      =  movementReferenceNumberForm(meta.getKey(claimType), isEntryNumberEnabled = true)
                          .bindFromRequest()
                          .fold(
                            formWithErrors => BadRequest(dummyEnterMovementReferenceNumberPage(meta.getKey(claimType), meta.submitUrl, formWithErrors)),
                            mrn => mrn
                          )
      } yield ()


      errorOrForm.flatMap(_.fold(

      ))

      errorOrClaim.fold(
          error => {
            logger.warn(error.message)
            errorHandler.errorResult()
          },
        data =>
              movementReferenceNumberForm(meta.getKey(data._2), isEntryNumberEnabled = true)
                .bindFromRequest()
                .fold(
                  formWithErrors => BadRequest(dummyEnterMovementReferenceNumberPage(meta.getKey(data._2), meta.submitUrl, formWithErrors)),
                  mrn => {
                    request.sessionData.fold(errorHandler.errorResult())(session =>
                      sessionService.persist(session.copy(journeyStatus = data._1.toFillingOutClaim(data._2).some))
                        .fold(error => {
                          logger.warn(error.message)
                          errorHandler.errorResult()
                        },
                          _ => Redirect(meta.nextUrl(data._2, mrn))
                        )
                    )
                  }
                )
        )
  }
}
