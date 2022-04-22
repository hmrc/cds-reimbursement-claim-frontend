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
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckClaimDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkClaimDetailsPage: pages.check_claim_details_scheduled
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  val checkClaimDetailsForm: Form[YesNo] = YesOrNoQuestionForm(CheckClaimDetailsController.checkClaimDetailsKey)

  private val postAction: Call         = routes.CheckClaimDetailsController.submit()
  private val selectDutiesAction: Call = routes.SelectDutyTypesController.show()

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val answers                         = journey.getReimbursementClaims
    val reimbursementTotal              = journey.getTotalReimbursementAmount
    implicit val subKey: Option[String] = Some("scheduled")
    if (journey.hasCompleteReimbursementClaims)
      Ok(checkClaimDetailsPage(answers, reimbursementTotal, checkClaimDetailsForm, postAction)).asFuture
    else Redirect(selectDutiesAction).asFuture
  }

  val submit: Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
      val answers                         = journey.getReimbursementClaims
      val reimbursementTotal              = journey.getTotalReimbursementAmount
      implicit val subKey: Option[String] = Some("scheduled")
      if (!journey.hasCompleteReimbursementClaims) (journey, Redirect(selectDutiesAction)).asFuture
      else {
        checkClaimDetailsForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              (
                journey,
                BadRequest(
                  checkClaimDetailsPage(
                    answers,
                    reimbursementTotal,
                    formWithErrors,
                    postAction
                  )
                )
              ),
            {
              case Yes => (journey, Redirect(routes.EnterInspectionDateController.show()))
              case No  => (journey, Redirect(selectDutiesAction))
            }
          )
          .asFuture
      }
    },
    fastForwardToCYAEnabled = false
  )

}

object CheckClaimDetailsController {
  val checkClaimDetailsKey: String = "check-claim-summary"
}
