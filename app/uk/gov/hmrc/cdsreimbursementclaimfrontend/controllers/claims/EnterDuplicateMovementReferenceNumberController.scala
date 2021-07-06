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
import cats.data.EitherT.{fromEither, fromOption}
import cats.syntax.all._
import com.google.inject.{Inject, Singleton}
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.data.validation.{Constraint, Invalid, Valid}
import play.api.data.{Form, FormError, Mapping}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDuplicateMovementReferenceNumberController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.evaluateMrnJourneyFlow
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.ErnImporter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
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
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[MovementReferenceNumber] =
    _.duplicateMovementReferenceNumberAnswer

  def enterDuplicateMrn(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MovementReferenceNumber] { (_, previousAnswer, router) =>
        val emptyForm = getForm()
        val form      = previousAnswer.fold(emptyForm)(emptyForm.fill(_))
        Ok(enterDuplicateMovementReferenceNumberPage(form, router))
      }
    }

  def enterDuplicateMrnSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MovementReferenceNumber] { (fillingOutClaim, _, router) =>
        getForm(fillingOutClaim.draftClaim.fold(identity).movementReferenceNumber)
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterDuplicateMovementReferenceNumberPage(
                  requestFormWithErrors
                    .copy(errors = Seq(processFormErrors(router.refNumberKey, requestFormWithErrors.errors))),
                  router
                )
              ),
            mrnOrEntryNumber => {

              val errorRedirect: Error => Result = e => {
                logger.warn("Mrn or Entry Number submission failed: ", e)
                Redirect(baseRoutes.IneligibleController.ineligible())
              }
              mrnOrEntryNumber match {
                case entryNumberAnswer @ MovementReferenceNumber(Left(_)) =>
                  EitherT(updateSession(sessionStore, request)(updateDuplicateMRN(fillingOutClaim, entryNumberAnswer)))
                    .leftMap(_ => Error("Could not save Entry Number"))
                    .fold(errorRedirect, _ => Redirect(router.nextPageForDuplicateMRN(ErnImporter)))
                case mrnAnswer @ MovementReferenceNumber(Right(mrn))      =>
                  val result = for {
                    maybeAcc14     <- claimService
                                        .getDisplayDeclaration(mrn)
                                        .leftMap(_ => Error("Could not get declaration"))
                    mrnJourneyFlow <-
                      fromEither[Future](evaluateMrnJourneyFlow(fillingOutClaim.signedInUserDetails, maybeAcc14))
                        .leftMap(_ => Error("could not evaluate MRN flow"))
                    declaration    <-
                      fromOption[Future](maybeAcc14, Error("could not unbox display declaration"))
                    _              <-
                      EitherT(
                        updateSession(sessionStore, request)(
                          updateDuplicateMrnAndAcc14(fillingOutClaim, mrnAnswer, declaration)
                        )
                      )
                        .leftMap(_ => Error("Could not save Display Declaration"))
                  } yield mrnJourneyFlow
                  result.fold(
                    errorRedirect,
                    mrnJourney =>
                      Redirect(
                        getRoutes(getNumberOfClaims(fillingOutClaim.draftClaim), Option(mrnOrEntryNumber), journey)
                          .nextPageForEnterMRN(mrnJourney)
                      )
                  )
              }
            }
          )
      }
    }

  def updateDuplicateMRN(
    fillingOutClaim: FillingOutClaim,
    mrnOrEntryNumber: MovementReferenceNumber
  ): SessionDataTransform = {
    val updatedDraftClaim =
      fillingOutClaim.draftClaim.fold(_.copy(duplicateMovementReferenceNumberAnswer = Option(mrnOrEntryNumber)))
    updateDraftClaim(fillingOutClaim, updatedDraftClaim)
  }

  def updateDuplicateMrnAndAcc14(
    fillingOutClaim: FillingOutClaim,
    mrnOrEntryNumber: MovementReferenceNumber,
    acc14: DisplayDeclaration
  ): SessionDataTransform = {
    val updatedDraftClaim = fillingOutClaim.draftClaim.fold(
      _.copy(movementReferenceNumber = Option(mrnOrEntryNumber), displayDeclaration = Option(acc14))
    )
    updateDraftClaim(fillingOutClaim, updatedDraftClaim)
  }

  def updateDraftClaim(fillingOutClaim: FillingOutClaim, newDraftClaim: DraftC285Claim): SessionDataTransform = {
    val updatedJourney               = fillingOutClaim.copy(draftClaim = newDraftClaim)
    val update: SessionDataTransform = _.copy(journeyStatus = Some(updatedJourney))
    update
  }

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

  def processFormErrors(refKey: Option[String], errors: Seq[FormError]): FormError = {
    val mainKey = enterDuplicateMovementReferenceNumberKey + refKey.map(a => s".$a").getOrElse("")
    errors.headOption
      .map(fe => FormError(mainKey, fe.messages))
      .getOrElse(FormError(mainKey, List("invalid")))
  }

}
