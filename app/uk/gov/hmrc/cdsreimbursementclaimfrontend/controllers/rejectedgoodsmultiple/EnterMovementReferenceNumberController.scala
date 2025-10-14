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

import cats.data.EitherT
import cats.implicits.catsSyntaxEq
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.XiEoriConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.movementReferenceNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterMovementReferenceNumberUtil
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.GetXiEoriMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ImportDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserXiEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_movement_reference_number

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val jcc: ClaimControllerComponents,
  claimService: ClaimService,
  val xiEoriConnector: XiEoriConnector,
  enterMovementReferenceNumberPage: enter_movement_reference_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleClaimBaseController
    with GetXiEoriMixin {

  final val showFirst: Action[AnyContent] = show(1)

  final def show(pageIndex: Int): Action[AnyContent] = actionReadClaim { claim =>
    if pageIndex <= 0 || pageIndex > claim.countOfMovementReferenceNumbers + 1 then
      Redirect(routes.CheckMovementReferenceNumbersController.show)
    else {
      val mrnIndex: Int = pageIndex - 1

      Ok(
        enterMovementReferenceNumberPage(
          movementReferenceNumberForm
            .withDefault(claim.getNthMovementReferenceNumber(mrnIndex)),
          "multiple",
          Some(pageIndex),
          routes.EnterMovementReferenceNumberController.submit(pageIndex)
        )
      )
    }
  }

  final def submit(pageIndex: Int): Action[AnyContent] = actionReadWriteClaim { claim =>
    if pageIndex <= 0 || pageIndex > claim.countOfMovementReferenceNumbers + 1 then
      (
        claim,
        Redirect(routes.CheckMovementReferenceNumbersController.show)
      )
    else {
      val mrnIndex: Int = pageIndex - 1
      val filledForm    = movementReferenceNumberForm.bindFromRequest()
      movementReferenceNumberForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              claim,
              BadRequest(
                enterMovementReferenceNumberPage(
                  formWithErrors,
                  "multiple",
                  Some(pageIndex),
                  routes.EnterMovementReferenceNumberController.submit(pageIndex)
                )
              )
            ),
          mrn =>
            {
              for
                maybeAcc14    <- claimService.getImportDeclaration(mrn)
                _             <- EnterMovementReferenceNumberUtil.validateDeclarationCandidate(
                                   claim,
                                   maybeAcc14
                                 )
                updatedClaim  <- updateClaim(
                                   claim,
                                   mrnIndex,
                                   mrn,
                                   maybeAcc14
                                 )
                updatedClaim2 <- getUserXiEoriIfNeeded(updatedClaim, mrnIndex === 0)
              yield updatedClaim2
            }.fold(
              error =>
                if error.message.startsWith("error.") then {
                  (
                    claim,
                    BadRequest(
                      enterMovementReferenceNumberPage(
                        filledForm
                          .withError("enter-movement-reference-number", error.message),
                        "multiple",
                        Some(pageIndex),
                        routes.EnterMovementReferenceNumberController.submit(pageIndex)
                      )
                    )
                  )
                } else if error.message === "submitMovementReferenceNumber.wrongImportDeclarationEori" then {
                  (
                    claim,
                    BadRequest(customError(mrn, pageIndex, "error.wrongMRN"))
                  )
                } else if error.message === "submitMovementReferenceNumber.needsSubsidy" then {
                  (
                    claim,
                    BadRequest(customError(mrn, pageIndex, "error.needsSubsidy"))
                  )
                } else if error.message === "submitMovementReferenceNumber.needsNonSubsidy" then {
                  (
                    claim,
                    BadRequest(
                      customError(mrn, pageIndex, "error.needsNonSubsidy")
                    )
                  )
                } else if error.message === "submitMovementReferenceNumber.movementReferenceNumberAlreadyExists" then {
                  (
                    claim,
                    BadRequest(customError(mrn, pageIndex, "multiple.error.existingMRN"))
                  )
                } else {
                  logger.error(s"Unable to record $mrn", error.toException)
                  (claim, Redirect(routes.ProblemWithMrnController.show(pageIndex, mrn)))
                },
              updatedClaim => (updatedClaim, redirectLocation(claim, updatedClaim, mrn, pageIndex))
            )
        )
    }
  }

  private def updateClaim(
    claim: Claim,
    mrnIndex: Int,
    mrn: MRN,
    maybeAcc14: Option[ImportDeclaration]
  ): EitherT[Future, Error, Claim] =
    maybeAcc14 match {
      case Some(acc14) =>
        EitherT.fromEither[Future](
          claim
            .submitMovementReferenceNumberAndDeclaration(
              mrnIndex,
              mrn,
              acc14
            )
            .left
            .map(Error.apply)
        )
      case _           =>
        EitherT.leftT(Error("could not unbox display declaration"))
    }

  private def customError(mrn: MRN, pageIndex: Int, errorSuffix: String)(implicit
    request: Request[?]
  ) =
    enterMovementReferenceNumberPage(
      movementReferenceNumberForm
        .fill(mrn)
        .withError("enter-movement-reference-number", errorSuffix),
      "multiple",
      Some(pageIndex),
      routes.EnterMovementReferenceNumberController.submit(pageIndex)
    )

  private def redirectLocation(
    claim: RejectedGoodsMultipleClaim,
    updatedClaim: RejectedGoodsMultipleClaim,
    mrn: MRN,
    pageIndex: Int
  ): Result =
    Redirect(
      if updatedClaim.containsUnsupportedTaxCodeFor(mrn) then {
        if pageIndex === 1 then routes.ProblemWithDeclarationController.show
        else routes.ProblemWithDeclarationController.showNth(pageIndex)
      } else if updatedClaim.needsDeclarantAndConsigneeEoriMultipleSubmission(pageIndex) then {
        routes.EnterImporterEoriNumberController.show
      } else {
        if pageIndex === 1 then routes.CheckDeclarationDetailsController.show
        else if claim.userHasSeenCYAPage && claim.getCorrectAmountsFor(mrn).isEmpty then
          routes.SelectDutiesController.show(pageIndex)
        else routes.CheckMovementReferenceNumbersController.show
      }
    )

  override def modifyClaim(claim: Claim, userXiEori: UserXiEori): Claim =
    claim.submitUserXiEori(userXiEori)
}
