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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.movementReferenceNumberRejectedGoodsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterMovementReferenceNumberUtil
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.GetXiEoriMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserXiEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.enter_movement_reference_number
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  claimService: ClaimService,
  val xiEoriConnector: XiEoriConnector,
  featureSwitchService: FeatureSwitchService,
  enterMovementReferenceNumberPage: enter_movement_reference_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends RejectedGoodsMultipleJourneyBaseController
    with GetXiEoriMixin {

  override def isXiEoriSupported(implicit hc: HeaderCarrier): Boolean =
    featureSwitchService.isEnabled(Feature.XiEori)

  final val showFirst: Action[AnyContent] = show(1)

  final def show(pageIndex: Int): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    (if (pageIndex <= 0 || pageIndex > journey.countOfMovementReferenceNumbers + 1)
       Redirect(routes.CheckMovementReferenceNumbersController.show())
     else {
       val mrnIndex: Int = pageIndex - 1

       Ok(
         enterMovementReferenceNumberPage(
           movementReferenceNumberRejectedGoodsForm
             .withDefault(journey.getNthMovementReferenceNumber(mrnIndex)),
           Some("multiple"),
           pageIndex,
           routes.EnterMovementReferenceNumberController.submit(pageIndex),
           isSubsidy = journey.isSubsidyOnlyJourney
         )
       )
     }).asFuture
  }

  final def submit(pageIndex: Int): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    if (pageIndex <= 0 || pageIndex > journey.countOfMovementReferenceNumbers + 1)
      (
        journey,
        Redirect(routes.CheckMovementReferenceNumbersController.show())
      ).asFuture
    else {
      val mrnIndex: Int = pageIndex - 1
      val filledForm    = movementReferenceNumberRejectedGoodsForm.bindFromRequest()
      movementReferenceNumberRejectedGoodsForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              journey,
              BadRequest(
                enterMovementReferenceNumberPage(
                  formWithErrors,
                  Some("multiple"),
                  pageIndex,
                  routes.EnterMovementReferenceNumberController.submit(pageIndex),
                  isSubsidy = journey.isSubsidyOnlyJourney
                )
              )
            ).asFuture,
          mrn =>
            {
              for {
                maybeAcc14      <- claimService.getDisplayDeclaration(mrn)
                _               <- EnterMovementReferenceNumberUtil.validateDeclarationCandidate(
                                     journey,
                                     maybeAcc14
                                   )
                updatedJourney  <- updateJourney(
                                     journey,
                                     mrnIndex,
                                     mrn,
                                     maybeAcc14
                                   )
                updatedJourney2 <- getUserXiEoriIfNeeded(updatedJourney, mrnIndex === 0)
              } yield updatedJourney2
            }.fold(
              error =>
                if (error.message.startsWith("error.")) {
                  (
                    journey,
                    BadRequest(
                      enterMovementReferenceNumberPage(
                        filledForm
                          .withError("enter-movement-reference-number.rejected-goods", error.message),
                        Some("multiple"),
                        pageIndex,
                        routes.EnterMovementReferenceNumberController.submit(pageIndex),
                        isSubsidy = journey.isSubsidyOnlyJourney
                      )
                    )
                  )
                } else if (error.message === "submitMovementReferenceNumber.wrongDisplayDeclarationEori") {
                  (
                    journey,
                    BadRequest(customError(mrn, pageIndex, "error.wrongMRN", journey.isSubsidyOnlyJourney))
                  )
                } else if (error.message === "submitMovementReferenceNumber.needsSubsidy") {
                  (
                    journey,
                    BadRequest(customError(mrn, pageIndex, "error.needsSubsidy", journey.isSubsidyOnlyJourney))
                  )
                } else if (error.message === "submitMovementReferenceNumber.needsNonSubsidy") {
                  (
                    journey,
                    BadRequest(
                      customError(mrn, pageIndex, "error.needsNonSubsidy", journey.isSubsidyOnlyJourney)
                    )
                  )
                } else if (error.message === "submitMovementReferenceNumber.movementReferenceNumberAlreadyExists") {
                  (
                    journey,
                    BadRequest(customError(mrn, pageIndex, "error.existingMRN", journey.isSubsidyOnlyJourney))
                  )
                } else {
                  logger.error(s"Unable to record $mrn", error.toException)
                  (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
                },
              updatedJourney => (updatedJourney, redirectLocation(journey, updatedJourney, mrn, pageIndex))
            )
        )
    }
  }

  private def updateJourney(
    journey: Journey,
    mrnIndex: Int,
    mrn: MRN,
    maybeAcc14: Option[DisplayDeclaration]
  ): EitherT[Future, Error, Journey] =
    maybeAcc14 match {
      case Some(acc14) =>
        EitherT.fromEither[Future](
          journey
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

  private def customError(mrn: MRN, pageIndex: Int, errorSuffix: String, isSubsidy: Boolean)(implicit
    request: Request[_]
  ) =
    enterMovementReferenceNumberPage(
      movementReferenceNumberRejectedGoodsForm
        .fill(mrn)
        .withError("enter-movement-reference-number.rejected-goods", errorSuffix),
      Some("multiple"),
      pageIndex,
      routes.EnterMovementReferenceNumberController.submit(pageIndex)
    )

  private def redirectLocation(
    journey: RejectedGoodsMultipleJourney,
    updatedJourney: RejectedGoodsMultipleJourney,
    mrn: MRN,
    pageIndex: Int
  ): Result =
    Redirect(
      if (updatedJourney.needsDeclarantAndConsigneeEoriMultipleSubmission(pageIndex)) {
        routes.EnterImporterEoriNumberController.show()
      } else {
        if (pageIndex === 1) routes.CheckDeclarationDetailsController.show()
        else if (journey.userHasSeenCYAPage && journey.getReimbursementClaimsFor(mrn).isEmpty)
          routes.SelectDutiesController.show(pageIndex)
        else routes.CheckMovementReferenceNumbersController.show()
      }
    )

  override def modifyJourney(journey: Journey, userXiEori: UserXiEori): Journey =
    journey.submitUserXiEori(userXiEori)
}
