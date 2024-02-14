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
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.enter_or_change_method_of_disposal

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class DisposalMethodController @Inject() (
  val jcc: JourneyControllerComponents,
  enterOrChangeMethodOfDisposal: enter_or_change_method_of_disposal
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsScheduledJourneyBaseController
    with Logging {

  private def postAction: Call = routes.DisposalMethodController.submit

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[RejectedGoodsScheduledJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Ok(
      enterOrChangeMethodOfDisposal(
        Forms.methodOfDisposalForm.withDefault(journey.answers.methodOfDisposal),
        postAction
      )
    ).asFuture
  }

  val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      Forms.methodOfDisposalForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                journey,
                BadRequest(enterOrChangeMethodOfDisposal(formWithErrors, postAction))
              )
            ),
          methodOfDisposal => {
            val updatedJourney = journey.submitMethodOfDisposal(methodOfDisposal)
            Future.successful(
              (
                updatedJourney,
                Redirect(routes.EnterRejectedGoodsDetailsController.show)
              )
            )
          }
        )
    }
}
