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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import cats.implicits.catsSyntaxEq
import play.api.data.Form
import play.api.data.FormError
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterScheduledClaimForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_scheduled_claim

import scala.concurrent.Future

object EnterScheduledClaimMixin {
  final case class RoutesPack(
    showAction: (DutyType, TaxCode) => Call,
    postAction: (DutyType, TaxCode) => Call,
    showSelectDutyTypes: Call,
    showSelectDuties: DutyType => Call,
    showCheckClaimDetails: Call
  )
}

trait EnterScheduledClaimMixin extends JourneyBaseController {

  type Journey <: journeys.Journey with journeys.JourneyBase with journeys.ScheduledVariantProperties

  val enterClaimPage: enter_scheduled_claim
  val routesPack: EnterScheduledClaimMixin.RoutesPack

  def modifyJourney(
    journey: Journey,
    dutyType: DutyType,
    taxCode: TaxCode,
    paidAmount: BigDecimal,
    correctAmount: BigDecimal
  ): Either[String, Journey]

  final def showFirst(): Action[AnyContent] = actionReadJourney { _ => journey =>
    journey.findNextDutyToSelectDuties match {
      case None =>
        (journey.getFirstDutyToClaim match {
          case Some((dutyType, taxCode)) =>
            Redirect(routesPack.showAction(dutyType, taxCode))

          case None =>
            Redirect(routesPack.showSelectDutyTypes)
        }).asFuture

      case Some(emptyDuty) =>
        Redirect(routesPack.showSelectDuties(emptyDuty)).asFuture
    }
  }

  final def show(dutyType: DutyType, taxCode: TaxCode): Action[AnyContent] = actionReadJourney {
    implicit request => journey =>
      val maybeReimbursement: Option[AmountPaidWithCorrect] = journey.getReimbursementFor(dutyType, taxCode)
      val form                                              = enterScheduledClaimForm.withDefault(maybeReimbursement)

      Ok(
        enterClaimPage(dutyType, taxCode, form, routesPack.postAction(dutyType, taxCode), journey.isSubsidyOnlyJourney)
      ).asFuture
  }

  final def submit(currentDuty: DutyType, currentTaxCode: TaxCode): Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      Future.successful(
        enterScheduledClaimForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              (
                journey,
                BadRequest(
                  enterClaimPage(
                    currentDuty,
                    currentTaxCode,
                    redirectVerificationMessage(formWithErrors),
                    routesPack.postAction(currentDuty, currentTaxCode),
                    journey.isSubsidyOnlyJourney
                  )
                )
              ),
            reimbursement =>
              modifyJourney(journey, currentDuty, currentTaxCode, reimbursement.paidAmount, reimbursement.correctAmount)
                .fold(
                  errors => {
                    logger.error(s"Error updating reimbursement selection - $errors")
                    (
                      journey,
                      BadRequest(
                        enterClaimPage(
                          currentDuty,
                          currentTaxCode,
                          enterScheduledClaimForm,
                          routesPack.postAction(currentDuty, currentTaxCode),
                          journey.isSubsidyOnlyJourney
                        )
                      )
                    )
                  },
                  updatedJourney =>
                    (
                      updatedJourney, {
                        updatedJourney.findNextSelectedTaxCodeAfter(currentDuty, currentTaxCode) match {
                          case Some((nextDutyType, nextTaxCode)) =>
                            if (journey.hasCompleteReimbursementClaims)
                              Redirect(routesPack.showCheckClaimDetails)
                            else if (currentDuty.repr === nextDutyType.repr)
                              Redirect(routesPack.showAction(nextDutyType, nextTaxCode))
                            else
                              Redirect(routesPack.showSelectDuties(nextDutyType))
                          case None                              =>
                            updatedJourney.findNextSelectedDutyAfter(currentDuty) match {
                              case Some(nextDutyType) =>
                                Redirect(routesPack.showSelectDuties(nextDutyType))
                              case None               => Redirect(routesPack.showCheckClaimDetails)
                            }
                        }
                      }
                    )
                )
          )
      )
    },
    fastForwardToCYAEnabled = false
  )

  def redirectVerificationMessage(
    formWithErrors: Form[AmountPaidWithCorrect]
  ): Form[AmountPaidWithCorrect] = {
    val errors: Seq[FormError] = formWithErrors.errors.map {
      case formError if formError.messages.contains("invalid.claim") =>
        formError.copy(key = s"${formError.key}.actual-amount")

      case formError => formError
    }
    formWithErrors.copy(errors = errors)
  }
}
