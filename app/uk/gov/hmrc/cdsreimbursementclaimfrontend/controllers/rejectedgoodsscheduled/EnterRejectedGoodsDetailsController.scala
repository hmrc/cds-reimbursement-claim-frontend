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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import com.github.arturopala.validator.Validator.Validate
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterRejectedGoodsDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.enter_rejected_goods_details

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterRejectedGoodsDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  enterRejectedGoodsDetailsPage: enter_rejected_goods_details
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  private val postAction: Call = routes.EnterRejectedGoodsDetailsController.submit

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsScheduledJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  def show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Ok(
      enterRejectedGoodsDetailsPage(
        enterRejectedGoodsDetailsForm.withDefault(journey.answers.detailsOfRejectedGoods),
        postAction
      )
    ).asFuture
  }

  def submit: Action[AnyContent] = actionReadWriteJourney {
    implicit request => (journey: RejectedGoodsScheduledJourney) =>
      enterRejectedGoodsDetailsForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(
                enterRejectedGoodsDetailsPage(
                  formWithErrors,
                  postAction
                )
              )
            ),
          details => {
            val updatedJourney = journey
              .submitDetailsOfRejectedGoods(details)
            if journey.isSubsidyOnlyJourney then {
              updatedJourney
                .selectAndReplaceDutyTypeSetForReimbursement(Seq(DutyType.EuDuty))
                .fold(
                  errors => {
                    logger.error(s"Error updating duty types  - $errors")
                    (journey, BadRequest(enterRejectedGoodsDetailsPage(enterRejectedGoodsDetailsForm, postAction)))
                  },
                  subsidyJourney => (subsidyJourney, Redirect(routes.SelectDutiesController.show(DutyType.EuDuty)))
                )

            } else {
              (updatedJourney, Redirect(routes.SelectDutyTypesController.show))
            }

          }
        )
        .asFuture
  }
}
