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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.check_movement_reference_numbers

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class CheckMovementReferenceNumbersController @Inject() (
  val jcc: ClaimControllerComponents,
  checkMovementReferenceNumbers: check_movement_reference_numbers
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleClaimBaseController {

  private val checkMovementReferenceNumbersKey: String       = "check-movement-reference-numbers.rejected-goods"
  private val checkMovementReferenceNumbersForm: Form[YesNo] = YesOrNoQuestionForm(checkMovementReferenceNumbersKey)
  private val postAction: Call                               = routes.CheckMovementReferenceNumbersController.submit

  final val show: Action[AnyContent] =
    actionReadClaim { claim =>
      claim.getMovementReferenceNumbers
        .map { mrns =>
          if claim.hasCompleteMovementReferenceNumbers then
            if claim.needsDeclarantAndConsigneeEoriSubmission && !claim.hasSubmittedDeclarantAndConsigneeEori then {
              Redirect(routes.EnterImporterEoriNumberController.show)
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
          else Redirect(routes.EnterMovementReferenceNumberController.show(claim.countOfMovementReferenceNumbers + 1))
        }
        .getOrElse(Redirect(routes.EnterMovementReferenceNumberController.show(0)))

    }

  final val submit: Action[AnyContent] = simpleActionReadWriteClaim(
    claim =>
      claim.getMovementReferenceNumbers
        .map { mrns =>
          checkMovementReferenceNumbersForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                (
                  claim,
                  BadRequest(
                    checkMovementReferenceNumbers(
                      mrns,
                      formWithErrors,
                      postAction,
                      routes.EnterMovementReferenceNumberController.show,
                      routes.CheckMovementReferenceNumbersController.delete
                    )
                  )
                ),
              answer =>
                answer match {
                  case Yes =>
                    (
                      claim,
                      Redirect(
                        routes.EnterMovementReferenceNumberController.show(claim.countOfMovementReferenceNumbers + 1)
                      )
                    )
                  case No  =>
                    if shouldForwardToCYA(claim) then (claim, Redirect(checkYourAnswers))
                    else (claim.withEnterContactDetailsMode(true), Redirect(routes.BasisForClaimController.show))
                }
            )
        }
        .getOrElse((claim, Redirect(routes.EnterMovementReferenceNumberController.show(0)))),
    fastForwardToCYAEnabled = false
  )

  final def delete(mrn: MRN): Action[AnyContent] =
    actionReadWriteClaim(
      claim =>
        claim
          .removeMovementReferenceNumberAndImportDeclaration(mrn)
          .fold(
            error => {
              logger.warn(s"Error occurred trying to remove MRN $mrn - `$error`")
              (claim, Redirect(baseRoutes.IneligibleController.ineligible))
            },
            updatedClaim => nextPageOnDelete(updatedClaim)
          ),
      fastForwardToCYAEnabled = false
    )

  private def nextPageOnDelete(claim: RejectedGoodsMultipleClaim): (RejectedGoodsMultipleClaim, Result) = (
    claim,
    if claim.hasCompleteAnswers then Redirect(routes.CheckClaimDetailsController.show)
    else Redirect(routes.CheckMovementReferenceNumbersController.show)
  )
}
