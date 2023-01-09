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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaimsList
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.hints.DropdownHints
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo

@Singleton
class SelectBasisForClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  val controllerComponents: MessagesControllerComponents,
  selectReasonForClaimPage: pages.select_basis_for_claim
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendBaseController
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with SessionDataExtractor
    with Logging {

  private val basisOfClaimsHints: DropdownHints =
    DropdownHints.range(elementIndex = 1, maxHints = 14)

  private def getPossibleClaimTypes(draftClaim: DraftClaim): BasisOfOverpaymentClaimsList =
    BasisOfOverpaymentClaimsList
      .withoutDuplicateEntry()
      .excludeNorthernIrelandClaims(
        draftClaim.whetherNorthernIrelandAnswer.getOrElse(YesNo.No).asBoolean,
        draftClaim.displayDeclaration
      )

  implicit val dataExtractor: DraftClaim => Option[BasisOfOverpaymentClaim] = _.basisOfClaimAnswer

  val selectBasisForClaim: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[BasisOfOverpaymentClaim] { (fillingOutClaim, answer) =>
        val emptyForm  = Forms.basisOfOverpaymentClaimForm
        val filledForm = answer.fold(emptyForm)(basisOfClaim => emptyForm.fill(basisOfClaim))
        Ok(
          selectReasonForClaimPage(
            filledForm,
            getPossibleClaimTypes(fillingOutClaim.draftClaim),
            basisOfClaimsHints,
            Some("scheduled"),
            routes.SelectBasisForClaimController.selectBasisForClaimSubmit
          )
        )
      }
    }

  val selectBasisForClaimSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[BasisOfOverpaymentClaim] { (fillingOutClaim, _) =>
        Forms.basisOfOverpaymentClaimForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                selectReasonForClaimPage(
                  formWithErrors,
                  getPossibleClaimTypes(fillingOutClaim.draftClaim),
                  basisOfClaimsHints,
                  Some("scheduled"),
                  routes.SelectBasisForClaimController.selectBasisForClaimSubmit
                )
              ),
            answer => {
              val updatedJourney = FillingOutClaim.from(fillingOutClaim)(_.copy(basisOfClaimAnswer = answer.some))

              EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = updatedJourney.some)))
                .leftMap(_ => Error("could not update session"))
                .fold(
                  logAndDisplayError("could not store reason for claim answer"),
                  _ =>
                    Redirect(
                      CheckAnswers.when(updatedJourney.draftClaim.isComplete)(alternatively =
                        routes.EnterAdditionalDetailsController.show
                      )
                    )
                )
            }
          )
      }
    }

}
