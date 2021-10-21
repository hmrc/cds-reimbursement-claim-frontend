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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement

import cats.data.EitherT
import cats.implicits._
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement.{ReimbursementClaim, ReimbursementClaimAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, DutyType, Error, TaxCode, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.FormUtils.moneyMapping
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{reimbursement => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class EnterReimbursementClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionCache: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration,
  enterReimbursementClaimPage: pages.enter_reimbursement_claim
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftClaim => Option[ReimbursementClaimAnswer] = _.reimbursementClaimAnswer

  def start(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[ReimbursementClaimAnswer] { (fillingOutClaim, answer) =>
      fillingOutClaim.draftClaim.dutyCodesSelectedAnswer
        .fold(
          Future.successful(Redirect(reimbursementRoutes.SelectDutyCodesController.start()))
        ) { dutyCodesAnswer =>
          val reimbursementClaimAnswer: ReimbursementClaimAnswer =
            answer.getOrElse(ReimbursementClaimAnswer.initialise(dutyCodesAnswer))

          val updatedJourneyStatus = FillingOutClaim.from(fillingOutClaim)(
            _.copy(
              reimbursementClaimAnswer = Some(reimbursementClaimAnswer.updateAnswer(dutyCodesAnswer))
            )
          )

          EitherT(updateSession(sessionCache, request)(_.copy(journeyStatus = Some(updatedJourneyStatus))))
            .leftMap(_ => Error("could not update session"))
            .fold(
              logAndDisplayError("could not update reimbursement claims"),
              _ =>
                reimbursementClaimAnswer.isIncompleteReimbursementClaim match {
                  case Some(dutyTypeAndTaxCode) =>
                    Redirect(
                      reimbursementRoutes.EnterReimbursementClaimController
                        .showReimbursementClaim(dutyTypeAndTaxCode._1, dutyTypeAndTaxCode._2)
                    )
                  case None                     =>
                    Redirect(reimbursementRoutes.CheckReimbursementClaimController.showReimbursementClaim())
                }
            )
        }
    }
  }

  def showReimbursementClaim(dutyType: DutyType, dutyCode: TaxCode): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[ReimbursementClaimAnswer] { (_, answer) =>
        val reimbursementInputForm =
          answer
            .map(_.reimbursementClaims(dutyType)(dutyCode))
            .filter(!_.isBlank)
            .foldLeft(EnterReimbursementClaimController.enterReimbursementClaimForm)((form, answer) =>
              form.fill(answer)
            )
        Ok(
          enterReimbursementClaimPage(
            reimbursementInputForm,
            dutyType,
            dutyCode
          )
        )
      }
    }

  def submitReimbursementClaim(dutyType: DutyType, dutyCode: TaxCode): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request: RequestWithSessionData[AnyContent] =>
      withAnswers[ReimbursementClaimAnswer] { (fillingOutClaim, maybeReimbursementClaimAnswer) =>
        EnterReimbursementClaimController.enterReimbursementClaimForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(enterReimbursementClaimPage(formWithErrors, dutyType, dutyCode)),
            reimbursementClaim =>
              maybeReimbursementClaimAnswer
                .fold(
                  Future.successful(
                    Redirect(baseRoutes.IneligibleController.ineligible())
                  )
                )(reimbursementClaimAnswer =>
                  updateSessionCache(
                    dutyType,
                    dutyCode,
                    reimbursementClaim,
                    reimbursementClaimAnswer,
                    fillingOutClaim
                  )
                )
          )
      }
    }

  private def updateSessionCache(
    dutyType: DutyType,
    dutyCode: TaxCode,
    reimbursementClaim: ReimbursementClaim,
    reimbursementClaimAnswer: ReimbursementClaimAnswer,
    fillingOutClaim: FillingOutClaim
  )(implicit hc: HeaderCarrier, request: RequestWithSessionData[AnyContent]): Future[Result] = {

    val updatedReimbursementClaimAnswer: ReimbursementClaimAnswer =
      reimbursementClaimAnswer.updateReimbursementClaim(dutyType, dutyCode, reimbursementClaim)

    val updatedJourney =
      FillingOutClaim.from(fillingOutClaim)(
        _.copy(reimbursementClaimAnswer = Some(updatedReimbursementClaimAnswer))
      )

    EitherT(updateSession(sessionCache, request)(_.copy(journeyStatus = Some(updatedJourney))))
      .leftMap(_ => Error("could not update session"))
      .fold(
        logAndDisplayError("could not get duty types selected"),
        _ =>
          updatedReimbursementClaimAnswer.isIncompleteReimbursementClaim match {
            case Some(dutyTypeAndTaxCode) =>
              Redirect(
                reimbursementRoutes.EnterReimbursementClaimController
                  .showReimbursementClaim(dutyTypeAndTaxCode._1, dutyTypeAndTaxCode._2)
              )
            case None                     =>
              Redirect(reimbursementRoutes.CheckReimbursementClaimController.showReimbursementClaim())
          }
      )
  }
}

object EnterReimbursementClaimController {

  val enterReimbursementClaimKey = "enter-reimbursement-claim"

  def enterReimbursementClaimForm: Form[ReimbursementClaim] = Form(
    enterReimbursementClaimKey ->
      mapping(
        s"amount-paid"           -> moneyMapping(13, 2, "error.invalid"),
        s"amount-should-of-paid" -> moneyMapping(13, 2, "error.invalid")
      )(ReimbursementClaim.apply)(ReimbursementClaim.unapply)
        .verifying(
          "invalid.reimbursement-claim",
          _.isValid
        )
  )
}
