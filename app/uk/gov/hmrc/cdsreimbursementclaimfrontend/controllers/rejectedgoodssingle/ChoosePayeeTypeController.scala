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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import com.github.arturopala.validator.Validator.Validate
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.ChoosePayeeTypeMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.choose_payee_type_page

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class ChoosePayeeTypeController @Inject() (
  val jcc: JourneyControllerComponents,
  val choosePayeeTypePage: choose_payee_type_page
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController
    with ChoosePayeeTypeMixin {

  final override def modifyJourney(journey: Journey, payeeType: PayeeType): Either[String, Journey] =
    journey.submitPayeeType(payeeType)

  final val postAction: Call                 = routes.ChoosePayeeTypeController.submit
  final def nextPage(journey: Journey): Call =
    if (journey.isSubsidyOnlyJourney)
      routes.UploadFilesController.show()
    else if (journey.isAllSelectedDutiesAreCMAEligible)
      routes.ChooseRepaymentMethodController.show()
    else if (journey.needsBanksAccountDetailsSubmission)
      routes.CheckBankDetailsController.show()
    else
      routes.UploadFilesController.show()

}
