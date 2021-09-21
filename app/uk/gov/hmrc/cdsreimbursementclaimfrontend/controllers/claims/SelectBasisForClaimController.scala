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
import cats.implicits.{catsSyntaxEq, catsSyntaxOptionId}
import com.google.inject.{Inject, Singleton}
import play.api.data.Forms.{mapping, number}
import play.api.data._
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectBasisForClaimController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaim._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BasisOfClaims, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.html.BasisOfClaimsHints
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

  implicit val dataExtractor: DraftC285Claim => Option[BasisOfClaim] = _.basisOfClaimAnswer

  def selectBasisForClaim(journey: JourneyBindable): Action[AnyContent] = show(isAmend = false)(journey)
  def changeBasisForClaim(journey: JourneyBindable): Action[AnyContent] = show(isAmend = true)(journey)

  def show(isAmend: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[BasisOfClaim] { (fillingOutClaim, answer, router) =>
        val emptyForm  = SelectBasisForClaimController.reasonForClaimForm
        val filledForm = answer.fold(emptyForm)(basisOfClaim => emptyForm.fill(SelectReasonForClaim(basisOfClaim)))
        Ok(
          selectReasonForClaimPage(
            filledForm,
            getPossibleClaimTypes(fillingOutClaim.draftClaim, journey),
            getBasisOfClaimsHints(journey),
            isAmend,
            router
          )
        )
      }
    }

  def selectBasisForClaimSubmit(journey: JourneyBindable): Action[AnyContent] = submit(isAmend = false)(journey)
  def changeBasisForClaimSubmit(journey: JourneyBindable): Action[AnyContent] = submit(isAmend = true)(journey)

  def submit(isAmend: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[BasisOfClaim] { (fillingOutClaim, _, router) =>
        SelectBasisForClaimController.reasonForClaimForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                selectReasonForClaimPage(
                  formWithErrors,
                  getPossibleClaimTypes(fillingOutClaim.draftClaim, journey),
                  getBasisOfClaimsHints(journey),
                  isAmend,
                  router
                )
              ),
            formOk => {
              val updatedJourney = FillingOutClaim.of(fillingOutClaim)(
                _.copy(
                  basisOfClaimAnswer = formOk.reasonForClaim.some
                )
              )

              EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = updatedJourney.some)))
                .leftMap(_ => Error("could not update session"))
                .fold(
                  logAndDisplayError("could not store reason for claim answer"),
                  _ => Redirect(router.nextPageForBasisForClaim(formOk.reasonForClaim, isAmend))
                )
            }
          )
      }
    }
}

object SelectBasisForClaimController {

  val selectBasisForClaimKey: String = "select-basis-for-claim"

  final case class SelectReasonForClaim(reasonForClaim: BasisOfClaim)

  val reasonForClaimForm: Form[SelectReasonForClaim] =
    Form(
      mapping(
        selectBasisForClaimKey -> number
          .verifying("invalid reason for claim", a => allClaimsTypes.map(_.value).contains(a))
          .transform[BasisOfClaim](allClaimsIntToType, allClaimsTypeToInt)
      )(SelectReasonForClaim.apply)(SelectReasonForClaim.unapply)
    )

  def getPossibleClaimTypes(draftClaim: DraftClaim, journey: JourneyBindable): BasisOfClaims =
    BasisOfClaims
      .withoutJourneyClaimsIfApplies(journey)
      .withoutNorthernIrelandClaimsIfApplies(draftClaim)

  def getBasisOfClaimsHints(journeyBindable: JourneyBindable): BasisOfClaimsHints =
    BasisOfClaimsHints.beginWith(
      if (journeyBindable === JourneyBindable.Scheduled || journeyBindable === JourneyBindable.Multiple) 2 else 0
    )
}
