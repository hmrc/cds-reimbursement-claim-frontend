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
import play.api.mvc.Call
import play.api.mvc.Request
import play.api.mvc.Result
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.CommonJourneyProperties
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
  def viewTemplate: Form[MRN] => Request[?] => HtmlFormat.Appendable
  def subsidyWaiverErrorPage: (MRN, Boolean) => Request[?] => HtmlFormat.Appendable
  def afterSuccessfullSubmit(journey: Journey): Result
  val problemWithMrnCall: MRN => Call

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
      (mrn: MRN) =>
        {
          for
            maybeAcc14      <- claimService.getDisplayDeclaration(mrn)
            -               <- EnterMovementReferenceNumberUtil.validateEoriFormats(journey, maybeAcc14, featureSwitchService)
            _               <- EnterMovementReferenceNumberUtil.validateDeclarationCandidate(journey, maybeAcc14)
            updatedJourney  <- updateJourney(journey, mrn, maybeAcc14)
            updatedJourney2 <- getUserXiEoriIfNeeded(updatedJourney, enabled = true)
          yield updatedJourney2
        }.fold(
          error =>
            if error.message == "error.has-only-subsidy-items" then {
              (
                journey,
                Ok(
                  subsidyWaiverErrorPage(mrn, true)(request)
                )
              )
            } else if error.message == "error.has-some-subsidy-items" then {
              (
                journey,
                Ok(
                  subsidyWaiverErrorPage(mrn, false)(request)
                )
              )
            } else if error.message.startsWith("error.") then {
              (
                journey,
                BadRequest(
                  viewTemplate(filledForm.withError(formKey, error.message))(
                    request
                  )
                )
              )
            } else {
              (journey, Redirect(problemWithMrnCall(mrn)))
            },
          updatedJourney => (updatedJourney, afterSuccessfullSubmit(updatedJourney))
        )
    )
  }

  final val submitWithoutSubsidies: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
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
      (mrn: MRN) =>
        {
          for
            maybeAcc14      <- claimService.getDisplayDeclaration(mrn)
            -               <- EnterMovementReferenceNumberUtil.validateEoriFormats(journey, maybeAcc14, featureSwitchService)
            updatedJourney  <- updateJourney(journey, mrn, maybeAcc14.map(_.removeSubsidyItems))
            updatedJourney2 <- getUserXiEoriIfNeeded(updatedJourney, enabled = true)
          yield updatedJourney2
        }.fold(
          _ => (journey, Redirect(problemWithMrnCall(mrn))),
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
}

object EnterMovementReferenceNumberUtil {

  def validateDeclarationCandidate[Journey <: CommonJourneyProperties](
    journey: Journey,
    maybeAcc14: Option[DisplayDeclaration]
  )(implicit ec: ExecutionContext): EitherT[Future, Error, Unit] =
    maybeAcc14 match {
      case None              => EitherT.rightT(())
      case Some(declaration) =>
        journey.validateDeclarationCandidate(declaration) match {
          case None        => EitherT.rightT(())
          case Some(error) => EitherT.leftT(Error(error))
        }
    }

  def validateEoriFormats[Journey <: CommonJourneyProperties](
    journey: Journey,
    maybeAcc14: Option[DisplayDeclaration],
    featureSwitchService: FeatureSwitchService
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    if featureSwitchService.isDisabled(Feature.NewEoriFormat)
    then {
      if journey.answers.userEoriNumber.doesNotMatchOldFormat
      then
        EitherT.leftT(
          Error(
            s"user's eori got new format (${journey.answers.userEoriNumber}) but the new-eori-format feature is off"
          )
        )
      else
        maybeAcc14 match {
          case None              => EitherT.rightT(())
          case Some(declaration) =>
            if declaration.getDeclarantEori.doesNotMatchOldFormat
            then
              EitherT.leftT(
                Error(
                  s"declarant eori got new format (${declaration.getDeclarantEori.value}) but the new-eori-format feature is off"
                )
              )
            else if declaration.getConsigneeEori.exists(_.doesNotMatchOldFormat)
            then
              EitherT.leftT(
                Error(
                  s"consignee eori got new format (${declaration.getConsigneeEori.get.value}) format but the new-eori-format feature is off"
                )
              )
            else EitherT.rightT(())
        }
    } else EitherT.rightT(())
}
