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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import com.github.arturopala.validator.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.CheckBankDetailsMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_bank_details_are_correct

import scala.concurrent.ExecutionContext

@Singleton
class CheckBankDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  val checkBankDetailsAreCorrectPage: check_bank_details_are_correct
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController
    with CheckBankDetailsMixin {

  import SecuritiesJourney.Checks._

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  final override val postAction: Call =
    routes.CheckBankDetailsController.submitWarning

  final override def continueRoute(journey: Journey): Call =
    if journey.userHasSeenCYAPage | journey.needsDocumentTypeSelection then {
      routes.ChooseFileTypeController.show
    } else {
      routes.UploadFilesController.show
    }

  final override val enterBankAccountDetailsRoute: Call =
    routes.EnterBankAccountDetailsController.show

  final override val changeBankAccountDetailsRoute: Call =
    enterBankAccountDetailsRoute

  final override def modifyJourney(journey: Journey, bankAccountDetails: BankAccountDetails): Either[String, Journey] =
    journey.submitBankAccountDetails(bankAccountDetails)

  final override def modifyJourneyRemoveBankDetails(journey: Journey): Journey =
    journey.removeBankAccountDetails()

}
