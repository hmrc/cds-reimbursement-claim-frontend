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

import javax.inject.Inject
import javax.inject.Singleton
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterScheduledClaimController.enterScheduledClaimForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.EnterClaimController.findDutyTypeAndTaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}

import scala.collection.SortedMap
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterClaimController @Inject() (
  val jcc: JourneyControllerComponents,
  enterClaimPage: pages.enter_scheduled_claim
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  //TODO: Add form to Forms object
  def iterate(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    def redirectToSummaryPage: Future[Result] =
      Redirect("/scheduled/check-claim").asFuture //FIXME: routes.CheckClaimController.show()

    def start(dutyAndTaxCode: (DutyType, TaxCode)): Future[Result] =
      Redirect(routes.EnterClaimController.show(dutyAndTaxCode._1, dutyAndTaxCode._2)).asFuture

    if (journey.hasCompleteReimbursementClaims) redirectToSummaryPage
    else
      findDutyTypeAndTaxCode(journey.getSelectedDuties).fold(redirectToSummaryPage) { dutyAndTaxCode =>
        start(dutyAndTaxCode)
      }

  }

  def show(dutyType: DutyType, taxCode: TaxCode): Action[AnyContent] = actionReadJourney {
    implicit request => journey =>
      val postAction: Call                          = routes.EnterClaimController.submit(dutyType, taxCode)
      val maybeReimbursement: Option[Reimbursement] = journey.getReimbursementFor(dutyType, taxCode)
      val form                                      = enterScheduledClaimForm.withDefault(maybeReimbursement)

      Ok(enterClaimPage(dutyType, taxCode, form, postAction)).asFuture

  }

  def submit(currentDuty: DutyType, currentTaxCode: TaxCode): Action[AnyContent] = actionReadWriteJourney {
    implicit request => journey =>
      val postAction: Call = routes.EnterClaimController.submit(currentDuty, currentTaxCode)

      Future.successful(
        enterScheduledClaimForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              (
                journey,
                BadRequest(enterClaimPage(currentDuty, currentTaxCode, formWithErrors, postAction))
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
                    logger.error(s"Error updating tax codes selection - $errors")
                    (
                      journey,
                      BadRequest(enterClaimPage(currentDuty, currentTaxCode, enterScheduledClaimForm, postAction))
                    )
                  },
                  updatedJourney =>
                    (
                      updatedJourney,
                      updatedJourney.findNextSelectedTaxCodeAfter(currentDuty, currentTaxCode) match {
                        case Some(nextTaxCode) => Redirect(routes.EnterClaimController.show(currentDuty, nextTaxCode))
                        case None              =>
                          Redirect(
                            "/rejected-goods/scheduled/check-claim"
                          ) //FIXME: routes.CheckClaimController.show()
                      }
                    )
                )
          )
      )
  }

}

object EnterClaimController {

  def findDutyTypeAndTaxCode(
    value: Map[DutyType, Seq[TaxCode]]
  ): Option[(DutyType, TaxCode)] =
    for {
      dutyTypeAndTaxCodes   <- value.find(_._2.nonEmpty)
      firstAvailableTaxCode <- dutyTypeAndTaxCodes._2.find(_.value.nonEmpty)
    } yield (dutyTypeAndTaxCodes._1, firstAvailableTaxCode)

  def findUnclaimedReimbursements1(
    value: SortedMap[DutyType, SortedMap[TaxCode, Reimbursement]]
  ): Option[(DutyType, TaxCode)] =
    for {
      unclaimedReimbursements <- value.find(_._2.exists(_._2.isUnclaimed))
      firstAvailable          <- unclaimedReimbursements._2.find(_._2.isUnclaimed)
    } yield (unclaimedReimbursements._1, firstAvailable._1)

  def findReimbursement(value: SortedMap[DutyType, SortedMap[TaxCode, Reimbursement]]): Option[Reimbursement] =
    for {
      claimedReimbursements <- value.find(_._2.exists(_._2.isValid))
      firstAvailable        <- claimedReimbursements._2.find(_._2.isValid)
    } yield (firstAvailable._2)
}
