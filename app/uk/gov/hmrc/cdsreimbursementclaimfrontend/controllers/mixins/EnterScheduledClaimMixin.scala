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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_scheduled_claim

import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory

object EnterScheduledClaimMixin {
  final case class RoutesPack(
    showAction: (DutyType, TaxCode) => Call,
    postAction: (DutyType, TaxCode) => Call,
    showSelectDutyTypes: Call,
    showSelectDuties: DutyType => Call,
    showSelectExciseCategoryDuties: ExciseCategory => Call,
    showCheckClaimDetails: Call
  )
}

trait EnterScheduledClaimMixin extends ClaimBaseController {

  type Claim <: claims.Claim & claims.ClaimBase & claims.ScheduledVariantProperties

  val enterClaimPage: enter_scheduled_claim
  val routesPack: EnterScheduledClaimMixin.RoutesPack

  def modifyClaim(
    claim: Claim,
    dutyType: DutyType,
    taxCode: TaxCode,
    paidAmount: BigDecimal,
    correctAmount: BigDecimal
  ): Either[String, Claim]

  final val showFirst: Action[AnyContent] = actionReadClaim { claim =>
    claim.findNextDutyToSelectDuties match {
      case None =>
        claim.getFirstDutyToClaim match {
          case Some((dutyType, taxCode)) =>
            Redirect(routesPack.showAction(dutyType, taxCode))

          case None =>
            Redirect(routesPack.showSelectDutyTypes)
        }

      case Some(emptyDuty) =>
        Redirect(routesPack.showSelectDuties(emptyDuty))
    }
  }

  final def show(dutyType: DutyType, taxCode: TaxCode): Action[AnyContent] =
    actionReadClaim { claim =>
      val maybeReimbursement: Option[AmountPaidWithCorrect] = claim.getReimbursementFor(dutyType, taxCode)
      val form                                              = enterScheduledClaimForm.withDefault(maybeReimbursement)

      Ok(
        enterClaimPage(dutyType, taxCode, form, routesPack.postAction(dutyType, taxCode))
      )
    }

  final def submit(currentDuty: DutyType, currentTaxCode: TaxCode): Action[AnyContent] = actionReadWriteClaim(
    implicit request =>
      claim =>
        Future.successful(
          enterScheduledClaimForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                (
                  claim,
                  BadRequest(
                    enterClaimPage(
                      currentDuty,
                      currentTaxCode,
                      redirectVerificationMessage(formWithErrors),
                      routesPack.postAction(currentDuty, currentTaxCode)
                    )
                  )
                ),
              reimbursement =>
                modifyClaim(claim, currentDuty, currentTaxCode, reimbursement.paidAmount, reimbursement.claimAmount)
                  .fold(
                    errors => {
                      logger.error(s"Error updating reimbursement selection - $errors")
                      (
                        claim,
                        BadRequest(
                          enterClaimPage(
                            currentDuty,
                            currentTaxCode,
                            enterScheduledClaimForm,
                            routesPack.postAction(currentDuty, currentTaxCode)
                          )
                        )
                      )
                    },
                    updatedClaim =>
                      (
                        updatedClaim, {
                          updatedClaim.findNextSelectedTaxCodeAfter(currentDuty, currentTaxCode) match {
                            case Some((nextDutyType, nextTaxCode: TaxCode)) =>
                              if claim.hasCompleteReimbursementClaims then Redirect(routesPack.showCheckClaimDetails)
                              else if currentDuty.repr === nextDutyType.repr then
                                Redirect(routesPack.showAction(nextDutyType, nextTaxCode))
                              else Redirect(routesPack.showSelectDuties(nextDutyType))

                            case Some((nextDutyType, nextExciseCategory: ExciseCategory)) =>
                              if claim.hasCompleteReimbursementClaims then Redirect(routesPack.showCheckClaimDetails)
                              else Redirect(routesPack.showSelectExciseCategoryDuties(nextExciseCategory))

                            case None =>
                              updatedClaim.findNextSelectedDutyAfter(currentDuty).match {
                                case Some(nextDutyType) => Redirect(routesPack.showSelectDuties(nextDutyType))
                                case None               => Redirect(routesPack.showCheckClaimDetails)
                              }
                          }
                        }
                      )
                  )
            )
        ),
    fastForwardToCYAEnabled = false
  )

  def redirectVerificationMessage(
    formWithErrors: Form[AmountPaidWithCorrect]
  ): Form[AmountPaidWithCorrect] = {
    val errors: Seq[FormError] = formWithErrors.errors.map {
      case formError if formError.messages.contains("invalid.claim") =>
        formError.copy(key = s"${formError.key}.claim-amount")

      case formError => formError
    }
    formWithErrors.copy(errors = errors)
  }
}
