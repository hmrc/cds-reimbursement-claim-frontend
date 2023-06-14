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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v2

import com.github.arturopala.validator.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReimbursementMethod
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.select_reimbursement_method

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ReimbursementMethodController @Inject() (
  val jcc: JourneyControllerComponents,
  selectReimbursementMethodPage: select_reimbursement_method
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsSingleJourneyBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsSingleJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val postAction: Call = routes.ReimbursementMethodController.submit

  val form: Form[ReimbursementMethod] =
    Forms.reimbursementMethodForm("reimbursement-method")

  final val show: Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      (
        if (journey.isAllSelectedDutiesAreCMAEligible) {
          Ok(
            selectReimbursementMethodPage(
              form.withDefault(journey.answers.reimbursementMethod),
              postAction
            )
          )
        } else
          Redirect(routes.ChooseFileTypeController.show)
      ).asFuture
    }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      form
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(
                selectReimbursementMethodPage(
                  formWithErrors,
                  postAction
                )
              )
            ).asFuture,
          method =>
            journey.submitReimbursementMethod(method) match {
              case Right(modifiedJourney) =>
                (
                  modifiedJourney,
                  Redirect(method match {
                    case ReimbursementMethod.BankAccountTransfer =>
                      routes.CheckBankDetailsController.show
                    case _                                       =>
                      routes.ChooseFileTypeController.show
                  })
                ).asFuture

              case Left("submitReimbursementMethod.notCMAEligible") =>
                (
                  journey,
                  Redirect(routes.ChooseFileTypeController.show)
                ).asFuture

              case Left(error) =>
                Future.failed(new Exception(error))
            }
        )
    }
}
