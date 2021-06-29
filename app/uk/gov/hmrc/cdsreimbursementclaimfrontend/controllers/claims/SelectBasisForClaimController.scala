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
import cats.syntax.option._
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaimAnswer.CompleteBasisOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class SelectBasisForClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  val featureSwitch: FeatureSwitchService,
  cc: MessagesControllerComponents,
  selectReasonForClaimPage: pages.select_basis_for_claim
)(implicit ec: ExecutionContext, viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with SessionDataExtractor
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[BasisOfClaimAnswer] = _.basisOfClaimAnswer

  def selectBasisForClaim(journey: JourneyBindable): Action[AnyContent] = show(false)(journey)
  def changeBasisForClaim(journey: JourneyBindable): Action[AnyContent] = show(true)(journey)

  def show(isAmend: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[BasisOfClaimAnswer] { (fillingOutClaim, answers, router) =>
        val radioOptions = getPossibleClaimTypes(fillingOutClaim.draftClaim)
        val emptyForm    = SelectBasisForClaimController.reasonForClaimForm
        val filledForm   = answers
          .flatMap(_.fold(_.maybeBasisOfClaim, _.basisOfClaim.some))
          .fold(emptyForm)(basisOfClaim => emptyForm.fill(SelectReasonForClaim(basisOfClaim)))
        Ok(selectReasonForClaimPage(filledForm, radioOptions, isAmend, router))
      }
    }

  def selectBasisForClaimSubmit(journey: JourneyBindable): Action[AnyContent] = submit(false)(journey)
  def changeBasisForClaimSubmit(journey: JourneyBindable): Action[AnyContent] = submit(true)(journey)

  def submit(isAmend: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[BasisOfClaimAnswer] { (fillingOutClaim, _, router) =>
        SelectBasisForClaimController.reasonForClaimForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(selectReasonForClaimPage(formWithErrors, allClaimsTypes, isAmend, router)),
            formOk => {
              val updatedBasisClaim = CompleteBasisOfClaimAnswer(formOk.reasonForClaim)
              val newDraftClaim     =
                fillingOutClaim.draftClaim
                  .fold(_.copy(basisOfClaimAnswer = Option(updatedBasisClaim), reasonForBasisAndClaimAnswer = None))
              val updatedJourney    = fillingOutClaim.copy(draftClaim = newDraftClaim)

              EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))
                .fold(
                  e => {
                    logger.warn("could not store reason for claim answer", e)
                    errorHandler.errorResult()
                  },
                  _ =>
                    isAmend match {
                      case true  =>
                        Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
                      case false =>
                        Redirect(router.nextPageForBasisForClaim(formOk.reasonForClaim))
                    }
                )
            }
          )
      }

    }

}

object SelectBasisForClaimController {

  final case class SelectReasonForClaim(reasonForClaim: BasisOfClaim)

  val reasonForClaimForm: Form[SelectReasonForClaim] =
    Form(
      mapping(
        "select-basis-for-claim" -> number
          .verifying("invalid reason for claim", a => allClaimsTypes.map(_.value).contains(a))
          .transform[BasisOfClaim](allClaimsIntToType, allClaimsTypeToInt)
      )(SelectReasonForClaim.apply)(SelectReasonForClaim.unapply)
    )

  def getPossibleClaimTypes(darftClaim: DraftClaim): List[BasisOfClaim] = {
    val isNorthernIrelandJourney      =
      darftClaim.fold(_.claimNorthernIrelandAnswer).getOrElse(ClaimNorthernIrelandAnswer.No)
    val receivedExciseCodes           = darftClaim
      .fold(_.displayDeclaration)
      .flatMap(_.displayResponseDetail.ndrcDetails.map(_.map(_.taxType)))
      .getOrElse(Nil)
    val hasNorthernIrelandExciseCodes =
      receivedExciseCodes.toSet.intersect(TaxCode.listOfUKExciseCodeStrings).size > 0

    isNorthernIrelandJourney match {
      case ClaimNorthernIrelandAnswer.No  =>
        allClaimsTypes.diff(
          List(EvidenceThatGoodsHaveNotEnteredTheEU, IncorrectExciseValue, CorrectionToRiskClassification)
        )
      case ClaimNorthernIrelandAnswer.Yes =>
        hasNorthernIrelandExciseCodes match {
          case true  => allClaimsTypes
          case false => allClaimsTypes.diff(List(IncorrectExciseValue))
        }
    }

  }

}
