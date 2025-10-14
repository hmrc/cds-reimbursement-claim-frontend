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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.CommonClaimProperties
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait EnterMovementReferenceNumberMixin extends ClaimBaseController with GetXiEoriMixin {

  def modifyClaim(claim: Claim, mrn: MRN, declaration: DisplayDeclaration): Either[String, Claim]

  def claimService: ClaimService
  def featureSwitchService: FeatureSwitchService

  val shouldValidateDeclaration: Boolean = true

  def form(claim: Claim): Form[MRN]
  def getMovementReferenceNumber(claim: Claim): Option[MRN]
  def viewTemplate: Form[MRN] => Request[?] ?=> HtmlFormat.Appendable
  def subsidyWaiverErrorPage: (MRN, Boolean) => Request[?] ?=> HtmlFormat.Appendable
  def afterSuccessfullSubmit(claim: Claim): Result
  val problemWithMrnCall: MRN => Call

  val formKey = "enter-movement-reference-number"

  final val show: Action[AnyContent] = actionReadClaim { claim =>
    Future.successful {
      Ok(
        viewTemplate(
          form(claim).withDefault(getMovementReferenceNumber(claim))
        )
      )
    }
  }

  final val submit: Action[AnyContent] = actionReadWriteClaim { claim =>
    val filledForm = form(claim).bindFromRequest()
    filledForm.fold(
      formWithErrors =>
        Future.successful(
          (
            claim,
            BadRequest(
              viewTemplate(formWithErrors)
            )
          )
        ),
      (mrn: MRN) =>
        {
          for
            maybeAcc14    <- claimService.getDisplayDeclaration(mrn)
            -             <- EnterMovementReferenceNumberUtil.validateEoriFormats(claim, maybeAcc14, featureSwitchService)
            _             <- if shouldValidateDeclaration
                             then EnterMovementReferenceNumberUtil.validateDeclarationCandidate(claim, maybeAcc14)
                             else EitherT.fromEither[Future](Right(()))
            updatedClaim  <- updateClaim(claim, mrn, maybeAcc14)
            updatedClaim2 <- getUserXiEoriIfNeeded(updatedClaim, enabled = true)
          yield updatedClaim2
        }.fold(
          error =>
            if error.message == "error.has-only-subsidy-items" then {
              (
                claim,
                Ok(
                  subsidyWaiverErrorPage(mrn, true)
                )
              )
            } else if error.message == "error.has-some-subsidy-items" then {
              (
                claim,
                Ok(
                  subsidyWaiverErrorPage(mrn, false)
                )
              )
            } else if error.message.startsWith("error.") then {
              (
                claim,
                BadRequest(
                  viewTemplate(filledForm.withError(formKey, error.message))
                )
              )
            } else {
              (claim, Redirect(problemWithMrnCall(mrn)))
            },
          updatedClaim => (updatedClaim, afterSuccessfullSubmit(updatedClaim))
        )
    )
  }

  final val submitWithoutSubsidies: Action[AnyContent] = actionReadWriteClaim { claim =>
    val filledForm = form(claim).bindFromRequest()
    filledForm.fold(
      formWithErrors =>
        Future.successful(
          (
            claim,
            BadRequest(
              viewTemplate(formWithErrors)
            )
          )
        ),
      (mrn: MRN) =>
        {
          for
            maybeAcc14    <- claimService.getDisplayDeclaration(mrn)
            -             <- EnterMovementReferenceNumberUtil.validateEoriFormats(claim, maybeAcc14, featureSwitchService)
            updatedClaim  <- updateClaim(claim, mrn, maybeAcc14.map(_.removeSubsidyItems))
            updatedClaim2 <- getUserXiEoriIfNeeded(updatedClaim, enabled = true)
          yield updatedClaim2
        }.fold(
          _ => (claim, Redirect(problemWithMrnCall(mrn))),
          updatedClaim => (updatedClaim, afterSuccessfullSubmit(updatedClaim))
        )
    )
  }

  private def updateClaim(
    claim: Claim,
    mrn: MRN,
    maybeAcc14: Option[DisplayDeclaration]
  ): EitherT[Future, Error, Claim] =
    maybeAcc14 match {
      case Some(acc14) =>
        EitherT.fromEither[Future](
          modifyClaim(claim, mrn, acc14).left.map(Error.apply)
        )
      case _           =>
        EitherT.leftT(Error("could not unbox display declaration"))
    }
}

object EnterMovementReferenceNumberUtil {

  def validateDeclarationCandidate[Claim <: CommonClaimProperties](
    claim: Claim,
    maybeAcc14: Option[DisplayDeclaration]
  )(implicit ec: ExecutionContext): EitherT[Future, Error, Unit] =
    maybeAcc14 match {
      case None              => EitherT.rightT(())
      case Some(declaration) =>
        claim.validateDeclarationCandidate(declaration) match {
          case None        => EitherT.rightT(())
          case Some(error) => EitherT.leftT(Error(error))
        }
    }

  def validateEoriFormats[Claim <: CommonClaimProperties](
    claim: Claim,
    maybeAcc14: Option[DisplayDeclaration],
    featureSwitchService: FeatureSwitchService
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    if featureSwitchService.isDisabled(Feature.NewEoriFormat)
    then {
      if claim.answers.userEoriNumber.doesNotMatchOldFormat
      then
        EitherT.leftT(
          Error(
            s"user's eori got new format (${claim.answers.userEoriNumber}) but the new-eori-format feature is off"
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
