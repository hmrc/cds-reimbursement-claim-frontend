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
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails

import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{common => pages}

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.CheckBankDetailsMixin

@Singleton
class CheckBankDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  val checkBankAccountDetailsPage: pages.check_bank_account_details
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController
    with CheckBankDetailsMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsSingleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final override def continueRoute(journey: Journey): Call =
    if (journey.userHasSeenCYAPage) checkYourAnswers
    else routes.ChooseFileTypeController.show()

  final override val chooseBankAccountTypeRoute: Call =
    routes.ChooseBankAccountTypeController.show()

  final override val changeBankAccountDetailsRoute: Call =
    chooseBankAccountTypeRoute

  final override def modifyJourney(journey: Journey, bankAccountDetails: BankAccountDetails): Either[String, Journey] =
    journey.submitBankAccountDetails(bankAccountDetails)

}
