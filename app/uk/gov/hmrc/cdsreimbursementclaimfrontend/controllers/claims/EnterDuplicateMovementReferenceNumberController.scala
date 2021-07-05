/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.data.EitherT
import cats.syntax.all._
import com.google.inject.{Inject, Singleton}
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.data.validation.{Constraint, Invalid, Valid}
import play.api.data.{Form, FormError, Mapping}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDuplicateMovementReferenceNumberController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.MrnImporter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterDuplicateMovementReferenceNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  claimService: ClaimService,
  enterDuplicateMovementReferenceNumberPage: pages.enter_duplicate_movement_reference_number
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[MovementReferenceNumber] =
    _.duplicateMovementReferenceNumberAnswer


}

object EnterDuplicateMovementReferenceNumberController {

  val enterDuplicateMovementReferenceNumberKey = "enter-duplicate-movement-reference-number"
  val invalidNumberError                       = "invalid.number"

  def getForm(): Form[MovementReferenceNumber] =
    Form(
      mapping(
        enterDuplicateMovementReferenceNumberKey -> nonEmptyText
          .transform[MRN](str => MRN.changeToUpperCaseWithoutSpaces(str), _.value)
      )(MovementReferenceNumber.apply)(_.value.toOption)
    )

  def getForm(maybeMrnAnswer: Option[MovementReferenceNumber]): Form[MovementReferenceNumber] =
    maybeMrnAnswer match {
      case None            => getForm()
      case Some(mrnAnswer) =>
        mrnAnswer.value match {
          case Left(entryNumber) => entryForm(entryNumber)
          case Right(mrn)        => mrnForm(mrn)
        }
    }

  def mrnForm(mainMrn: MRN): Form[MovementReferenceNumber] =
    Form(
      mapping(
        enterDuplicateMovementReferenceNumberKey -> mrnMapping(mainMrn)
      )(MovementReferenceNumber.apply)(_.value.toOption)
    )

  def mrnMapping(mainMrn: MRN): Mapping[MRN] =
    nonEmptyText
      .verifying(Constraint[String] { str: String =>
        if (str.length === 0) Invalid("error.required")
        else if (str === mainMrn.value) Invalid("invalid.enter-different-mrn")
        else if (EntryNumber.isValid(str)) Invalid("invalid.mrn-not-entry-number")
        else if (MRN.isValid(str)) Valid
        else Invalid(invalidNumberError)
      })
      .transform[MRN](str => MRN.changeToUpperCaseWithoutSpaces(str), _.value)

  def entryForm(mainEntryNumber: EntryNumber): Form[MovementReferenceNumber] =
    Form(
      mapping(
        enterDuplicateMovementReferenceNumberKey -> entryNumberMapping(mainEntryNumber)
      )(MovementReferenceNumber.apply)(_.value.swap.toOption)
    )

  def entryNumberMapping(mainEntryNumber: EntryNumber): Mapping[EntryNumber] =
    nonEmptyText
      .verifying(Constraint[String] { str: String =>
        if (str.length === 0) Invalid("error.required")
        else if (str === mainEntryNumber.value) Invalid("invalid.enter-different-entry-number")
        else if (MRN.isValid(str)) Invalid("invalid.entry-number-not-mrn")
        else if (EntryNumber.isValid(str)) Valid
        else Invalid(invalidNumberError)
      })
      .transform[EntryNumber](str => EntryNumber.changeToUpperCaseWithoutSpaces(str), _.value)

  def processFormErrors(errors: Seq[FormError]): FormError =
    errors.headOption
      .map(fe => FormError(enterDuplicateMovementReferenceNumberKey, fe.messages))
      .getOrElse(FormError(enterDuplicateMovementReferenceNumberKey, List("invalid")))

}
