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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import cats.data.EitherT
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait EnterMovementReferenceNumberMixin extends JourneyBaseController with GetXiEoriMixin {

  def modifyJourney(journey: Journey, mrn: MRN, declaration: DisplayDeclaration): Either[String, Journey]

  def claimService: ClaimService
  def featureSwitchService: FeatureSwitchService
  def form(journey: Journey): Form[MRN]
  def getMovementReferenceNumber(journey: Journey): Option[MRN]
  def viewTemplate: Form[MRN] => Request[_] => HtmlFormat.Appendable
  def afterSuccessfullSubmit(journey: Journey): Result

  val formKey = "enter-movement-reference-number"

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful {
      Ok(
        viewTemplate(
          form(journey).withDefault(getMovementReferenceNumber(journey))
        )(request)
      )
    }
  }

  final val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    val filledForm = form(journey).bindFromRequest()
    filledForm.fold(
      formWithErrors =>
        Future.successful(
          (
            journey,
            BadRequest(
              viewTemplate(formWithErrors)(request)
            )
          )
        ),
      mrn =>
        {
          for {
            maybeAcc14      <- getDeclaration(mrn)
            _               <- EnterMovementReferenceNumberUtil.validateDeclarationHasSubsidyPayment(
                                 featureSwitchService.isEnabled(Feature.BlockSubsidies),
                                 maybeAcc14
                               )
            updatedJourney  <- updateJourney(journey, mrn, maybeAcc14)
            updatedJourney2 <- getUserXiEoriIfNeeded(updatedJourney, enabled = true)
          } yield updatedJourney2
        }.fold(
          errors =>
            if (errors.contains(EnterMovementReferenceNumberUtil.SUBSIDY_PAYMENT_FOUND_ERROR)) {
              (
                journey,
                BadRequest(
                  viewTemplate(filledForm.withError(formKey, "error.subsidy-payment-found"))(
                    request
                  )
                )
              )
            } else {
              logger.error(s"Unable to record $mrn", errors.toException)
              (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
            },
          updatedJourney => (updatedJourney, afterSuccessfullSubmit(updatedJourney))
        )
    )
  }

  private def updateJourney(
    journey: Journey,
    mrn: MRN,
    maybeAcc14: Option[DisplayDeclaration]
  ): EitherT[Future, Error, Journey] =
    maybeAcc14 match {
      case Some(acc14) =>
        EitherT.fromEither[Future](
          modifyJourney(journey, mrn, acc14).left.map(Error.apply)
        )
      case _           =>
        EitherT.leftT(Error("could not unbox display declaration"))
    }

  private def getDeclaration(mrn: MRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[DisplayDeclaration]] =
    claimService
      .getDisplayDeclaration(mrn)
}

object EnterMovementReferenceNumberUtil {
  val SUBSIDY_PAYMENT_FOUND_ERROR = "SUBSIDY_PAYMENT_FOUND_ERROR"

  def validateDeclarationHasSubsidyPayment(
    blockSubsidies: Boolean,
    maybeAcc14: Option[DisplayDeclaration]
  )(implicit ec: ExecutionContext): EitherT[Future, Error, Unit] =
    (blockSubsidies, maybeAcc14) match {
      case (false, _)             => EitherT.rightT(())
      case (_, None)              => EitherT.rightT(())
      case (_, Some(declaration)) =>
        if (declaration.hasSomeSubsidyPayment)
          EitherT.leftT(Error(SUBSIDY_PAYMENT_FOUND_ERROR))
        else
          EitherT.rightT(())
    }
}
