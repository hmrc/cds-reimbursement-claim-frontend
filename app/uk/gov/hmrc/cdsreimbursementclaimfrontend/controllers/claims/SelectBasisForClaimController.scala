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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.data.EitherT
import cats.implicits.catsSyntaxEq
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Forms.mapping
import play.api.data.Forms.number
import play.api.data._
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectBasisForClaimController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.BasisOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.BasisOfClaims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.hints.DropdownHints
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class SelectBasisForClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  cc: MessagesControllerComponents,
  selectReasonForClaimPage: pages.select_basis_for_claim
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with SessionDataExtractor
    with Logging {

  implicit val dataExtractor: DraftClaim => Option[BasisOfClaimAnswer] = _.basisOfClaimAnswer

  def selectBasisForClaim(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[BasisOfClaimAnswer] { (fillingOutClaim, answer) =>
        val emptyForm  = SelectBasisForClaimController.reasonForClaimForm
        val filledForm = answer.fold(emptyForm)(basisOfClaim => emptyForm.fill(basisOfClaim))
        Ok(
          selectReasonForClaimPage(
            filledForm,
            getPossibleClaimTypes(fillingOutClaim.draftClaim, journey),
            getBasisOfClaimsHints(journey)
          )
        )
      }
    }

  def selectBasisForClaimSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[BasisOfClaimAnswer] { (fillingOutClaim, _, routes) =>
        import routes._

        SelectBasisForClaimController.reasonForClaimForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                selectReasonForClaimPage(
                  formWithErrors,
                  getPossibleClaimTypes(fillingOutClaim.draftClaim, journey),
                  getBasisOfClaimsHints(journey)
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
                      answer match {
                        case BasisOfClaimAnswer.DuplicateEntry =>
                          OverpaymentsRoutes.EnterDuplicateMovementReferenceNumberController
                            .enterDuplicateMrn(journeyBindable)
                        case _                                 =>
                          CheckAnswers.when(updatedJourney.draftClaim.isComplete)(alternatively =
                            OverpaymentsRoutes.EnterAdditionalDetailsController.show(journeyBindable)
                          )
                      }
                    )
                )
            }
          )
      }
    }
}

object SelectBasisForClaimController {

  val selectBasisForClaimKey: String = "select-basis-for-claim"

  val reasonForClaimForm: Form[BasisOfClaimAnswer] =
    Form(
      mapping(
        selectBasisForClaimKey -> number
          .verifying("invalid reason for claim", idx => BasisOfClaims.contains(idx))
          .transform[BasisOfClaimAnswer](BasisOfClaims.all(_), BasisOfClaims.indexOf)
      )(identity)(Some(_))
    )

  def getPossibleClaimTypes(draftClaim: DraftClaim, journey: JourneyBindable): BasisOfClaims =
    BasisOfClaims
      .excludeNonJourneyClaims(journey)
      .excludeNorthernIrelandClaims(draftClaim)

  def getBasisOfClaimsHints(journey: JourneyBindable): DropdownHints =
    DropdownHints.range(
      if (journey === JourneyBindable.Scheduled || journey === JourneyBindable.Multiple) 1 else 0,
      maxHints = 14
    )
}
