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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import com.github.arturopala.validator.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.check_bank_account_details

import scala.concurrent.ExecutionContext

@Singleton
class CheckBankDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  checkBankAccountDetailsPage: check_bank_account_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  import SecuritiesJourney.Checks._

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  def show(): Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      val continueRoute =
        if (journey.userHasSeenCYAPage | journey.needsDocumentTypeSelection) {
          routes.ChooseFileTypeController.show()
        } else {
          routes.UploadFilesController.show()
        }

      journey.computeBankAccountDetails
        .map { bankAccountDetails: BankAccountDetails =>
          journey
            .submitBankAccountDetails(bankAccountDetails)
            .fold(
              _ => (journey, Redirect(continueRoute)),
              journeyWithBankDetails =>
                (
                  journeyWithBankDetails,
                  Ok(
                    checkBankAccountDetailsPage(
                      bankAccountDetails.masked,
                      continueRoute,
                      routes.BankDetailsChangeLetterOfAuthorityController.show()
                    )
                  )
                )
            )
        }
        .getOrElse {
          (
            journey,
            Redirect(
              if (journey.needsBanksAccountDetailsSubmission && !journey.haveBankDetailsOnAcc14)
                routes.ChooseBankAccountTypeController.show()
              else
                continueRoute
            )
          )
        }
        .asFuture
    }
}
