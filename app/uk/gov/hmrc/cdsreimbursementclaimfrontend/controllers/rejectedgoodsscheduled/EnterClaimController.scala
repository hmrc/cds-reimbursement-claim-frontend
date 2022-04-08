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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import play.api.data.Form
import play.api.data.FormError
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterScheduledClaimForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterClaimController @Inject() (
  val jcc: JourneyControllerComponents,
  enterClaimPage: pages.enter_scheduled_claim
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  def showFirst(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    journey.findNextDutyToSelectTaxCodes match {
      case None =>
        (journey.getSelectedDuties.headOption
          .flatMap { case (dt, tcs) => tcs.headOption.map(tc => (dt, tc)) } match {
          case Some((dutyType, taxCode)) =>
            val postAction: Call                          = routes.EnterClaimController.submit(dutyType, taxCode)
            val maybeReimbursement: Option[Reimbursement] = journey.getReimbursementFor(dutyType, taxCode)
            val form                                      = enterScheduledClaimForm.withDefault(maybeReimbursement)

            Ok(enterClaimPage(dutyType, taxCode, form, postAction))

          case None => Redirect(routes.SelectDutyTypesController.show())
        }).asFuture

      case Some(emptyDuty) =>
        Redirect(routes.SelectTaxCodesController.show(emptyDuty)).asFuture
    }
  }

  def show(dutyType: DutyType, taxCode: TaxCode): Action[AnyContent] = actionReadJourney {
    implicit request => journey =>
      journey.findNextDutyToSelectTaxCodes match {
        case None =>
          val postAction: Call                          = routes.EnterClaimController.submit(dutyType, taxCode)
          val maybeReimbursement: Option[Reimbursement] = journey.getReimbursementFor(dutyType, taxCode)
          val form                                      = enterScheduledClaimForm.withDefault(maybeReimbursement)

          Ok(enterClaimPage(dutyType, taxCode, form, postAction)).asFuture

        case Some(emptyDuty) =>
          Redirect(routes.SelectTaxCodesController.show(emptyDuty)).asFuture
      }

  }

  def submit(currentDuty: DutyType, currentTaxCode: TaxCode): Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      journey.findNextDutyToSelectTaxCodes match {
        case None =>
          val postAction: Call = routes.EnterClaimController.submit(currentDuty, currentTaxCode)

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
                        postAction
                      )
                    )
                  ),
                reimbursement =>
                  journey
                    .submitAmountForReimbursement(
                      currentDuty,
                      currentTaxCode,
                      reimbursement.shouldOfPaid,
                      reimbursement.paidAmount
                    )
                    .fold(
                      errors => {
                        logger.error(s"Error updating reimbursement selection - $errors")
                        (
                          journey,
                          BadRequest(enterClaimPage(currentDuty, currentTaxCode, enterScheduledClaimForm, postAction))
                        )
                      },
                      updatedJourney =>
                        (
                          updatedJourney,
                          updatedJourney.findNextSelectedTaxCodeAfter(currentDuty, currentTaxCode) match {
                            case Some((nextDutyType, nextTaxCode)) =>
                              Redirect(routes.EnterClaimController.show(nextDutyType, nextTaxCode))
                            case None                              =>
                              Redirect(
                                "/rejected-goods/scheduled/check-claim"
                              ) //FIXME: routes.CheckClaimController.show()
                          }
                        )
                    )
              )
          )

        case Some(emptyDuty) =>
          (journey, Redirect(routes.SelectTaxCodesController.show(emptyDuty))).asFuture
      }
    },
    fastForwardToCYAEnabled = false
  )

  def redirectVerificationMessage(formWithErrors: Form[Reimbursement]): Form[Reimbursement] = {
    val errors: Seq[FormError] = formWithErrors.errors.map {
      case formError if formError.messages.contains("invalid.claim") =>
        formError.copy(key = s"${formError.key}.actual-amount")
      case formError                                                 => formError
    }
    formWithErrors.copy(errors = errors)
  }
}