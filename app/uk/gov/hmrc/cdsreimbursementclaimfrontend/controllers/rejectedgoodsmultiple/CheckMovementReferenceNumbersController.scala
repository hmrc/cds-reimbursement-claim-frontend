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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.check_movement_reference_numbers

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckMovementReferenceNumbersController @Inject() (
  val jcc: JourneyControllerComponents,
  checkMovementReferenceNumbers: check_movement_reference_numbers
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController {

  private val checkMovementReferenceNumbersKey: String       = "check-movement-reference-numbers.rejected-goods"
  private val checkMovementReferenceNumbersForm: Form[YesNo] = YesOrNoQuestionForm(checkMovementReferenceNumbersKey)
  private val postAction: Call                               = routes.CheckMovementReferenceNumbersController.submit()

  final val show: Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      journey.getMovementReferenceNumbers
        .map { mrns =>
          if (journey.hasCompleteMovementReferenceNumbers)
            if (journey.needsDeclarantAndConsigneeEoriSubmission && !journey.hasSubmittedDeclarantAndConsigneeEori) {
              Redirect(routes.EnterImporterEoriNumberController.show())
            } else
              Ok(
                checkMovementReferenceNumbers(
                  mrns,
                  checkMovementReferenceNumbersForm,
                  postAction,
                  routes.EnterMovementReferenceNumberController.show,
                  routes.CheckMovementReferenceNumbersController.delete
                )
              )
          else
            Redirect(routes.EnterMovementReferenceNumberController.show(journey.countOfMovementReferenceNumbers + 1))
        }
        .getOrElse(Redirect(routes.EnterMovementReferenceNumberController.show(0)))
        .asFuture
    }

  final val submit: Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      journey.getMovementReferenceNumbers
        .map { mrns =>
          checkMovementReferenceNumbersForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  checkMovementReferenceNumbers(
                    mrns,
                    formWithErrors,
                    postAction,
                    routes.EnterMovementReferenceNumberController.show,
                    routes.CheckMovementReferenceNumbersController.delete
                  )
                ),
              answer =>
                Redirect(
                  answer match {
                    case Yes =>
                      routes.EnterMovementReferenceNumberController.show(journey.countOfMovementReferenceNumbers + 1)
                    case No  =>
                      if (shouldForwardToCYA(journey)) checkYourAnswers
                      else routes.CheckClaimantDetailsController.show()
                  }
                )
            )
        }
        .getOrElse(Redirect(routes.EnterMovementReferenceNumberController.show(0)))
        .asFuture
    }

  final def delete(mrn: MRN): Action[AnyContent] =
    actionReadWriteJourney(
      _ =>
        journey =>
          journey
            .removeMovementReferenceNumberAndDisplayDeclaration(mrn)
            .fold(
              error => {
                logger.warn(s"Error occurred trying to remove MRN $mrn - `$error`")
                (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
              },
              updatedJourney => (nextPageOnDelete(updatedJourney))
            )
            .asFuture,
      fastForwardToCYAEnabled = false
    )

  private def nextPageOnDelete(journey: RejectedGoodsMultipleJourney): (RejectedGoodsMultipleJourney, Result) = (
    journey,
    if (journey.hasCompleteAnswers) Redirect(routes.CheckClaimDetailsController.show())
    else Redirect(routes.CheckMovementReferenceNumbersController.show())
  )
}
