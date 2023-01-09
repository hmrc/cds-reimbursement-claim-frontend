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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import com.github.arturopala.validator.Validator.Validate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.declarantOrImporterEoriMatchesUserOrHasBeenVerified
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney.Checks.hasMRNAndDisplayDeclarationAndRfS

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.bankAccountLetterOfAuthorityForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.securities.bank_account_letter_of_authority

import scala.concurrent.ExecutionContext

@Singleton
class BankDetailsChangeLetterOfAuthorityController @Inject() (
  val jcc: JourneyControllerComponents,
  bankAccountLetterOfAuthority: bank_account_letter_of_authority
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController {

  private val submitRoute: Call = routes.BankDetailsChangeLetterOfAuthorityController.submit()

  final override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(
      hasMRNAndDisplayDeclarationAndRfS &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified
    )

  val show: Action[AnyContent] = actionReadJourney { implicit request => _ =>
    Ok(bankAccountLetterOfAuthority(bankAccountLetterOfAuthorityForm, submitRoute)).asFuture
  }

  def submit(): Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      bankAccountLetterOfAuthorityForm
        .bindFromRequest()
        .fold(
          formWithError => BadRequest(bankAccountLetterOfAuthority(formWithError, submitRoute)),
          {
            case Yes => Redirect(routes.ChooseBankAccountTypeController.show())
            case No  =>
              if (journey.userHasSeenCYAPage) {
                Redirect(routes.CheckYourAnswersController.show())
              } else {
                Redirect(routes.CheckBankDetailsController.show())
              }
          }
        )
        .asFuture
    }
}
