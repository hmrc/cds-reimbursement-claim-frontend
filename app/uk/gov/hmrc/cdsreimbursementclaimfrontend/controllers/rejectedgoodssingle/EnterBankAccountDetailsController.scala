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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterBankAccountDetailsController @Inject() (
  val jcc: JourneyControllerComponents,
  enterBankAccountDetailsPage: pages.enter_bank_account_details
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  val formKey: String          = "enter-bank-details"
  private val postAction: Call = routes.EnterBankAccountDetailsController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful {
      val form = enterBankDetailsForm.withDefault(journey.answers.bankAccountDetails)

      Ok(enterBankAccountDetailsPage(form, postAction))
    }
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    Future.successful(
      enterBankDetailsForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(
                enterBankAccountDetailsPage(
                  formWithErrors,
                  postAction
                )
              )
            ),
          bankAccountDetails =>
            journey
              .submitBankAccountDetails(bankAccountDetails)
              .fold(
                errors => {
                  logger.error(s"unable to get bank account details - $errors")
                  (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                },
                updatedJourney =>
                  (
                    updatedJourney,
                    Redirect(
                      "/claim-for-reimbursement-of-import-duties/rejected-goods/single/check-these-bank-details-are-correct"
                    )
                  )
              )
        )
    )
  }

}
