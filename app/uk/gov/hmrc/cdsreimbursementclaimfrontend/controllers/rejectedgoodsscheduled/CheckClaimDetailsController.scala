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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.CheckClaimDetailsController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.CheckClaimDetailsController.checkClaimDetailsKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}

import javax.inject.Inject
import javax.inject.Singleton
import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class CheckClaimDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkClaimDetailsPage: pages.check - claim - details - scheduled
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController {

  val checkClaimDetailsForm: Form[YesNo] = YesOrNoQuestionForm(CheckClaimDetailsController.checkClaimDetailsKey)

  private val postAction: Call =
    routes.SelectDutyTypesController.submit() //FIXME: routes.CheckClaimDetailsController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val answers: SortedMap[DutyType, SortedMap[TaxCode, Reimbursement]] = journey.getReimbursementClaims
    Ok(checkClaimDetailsPage(answers, checkClaimDetailsForm, postAction)).asFuture

  }

  //val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
//  Future.successful(
//    checkClaimDetailsForm
//  .bindFromRequest()
//  .fold(
//  formWithErrors =>
//  (
//  journey,
//  BadRequest(
//    checkClaimDetailsPage(
//  formWithErrors,
//  postAction
//  )
//  )
//  ),
//    yesNo =>
//  (
//  journey..[SUBMIT METHOD](yesNo),
//Redirect(routes.[NEW CONTROLLER].show()) //FIXME
//  )
//  )
//  )
//}

}

object CheckClaimDetailsController {
  val checkClaimDetailsKey: String = "check-claim-summary"
}
