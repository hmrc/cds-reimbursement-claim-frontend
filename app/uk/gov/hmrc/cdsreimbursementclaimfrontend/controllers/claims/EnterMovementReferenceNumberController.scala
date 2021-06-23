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
import cats.implicits._
import com.google.inject.{Inject, Singleton}
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.data._
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{MRNBulkRoutes, MRNScheduledRoutes, MRNSingleRoutes, SessionDataExtractor, SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.{ErnImporter, MrnImporter, ThirdPartyImporter}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{MovementReferenceNumber, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{ClaimService, FeatureSwitchService}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  claimService: ClaimService,
  featureSwitch: FeatureSwitchService,
  enterMovementReferenceNumberPage: pages.enter_movement_reference_number,
  enterNoLegacyMrnPage: pages.enter_no_legacy_mrn
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  import cats.data.EitherT._
  implicit val dataExtractor: DraftC285Claim => Option[MovementReferenceNumber] = _.movementReferenceNumber

  private def resolveEnterMrnPageFor(
    feature: FeatureSwitchService
  )(form: Form[MovementReferenceNumber], isAmend: Boolean, router: ReimbursementRoutes)(implicit
    request: RequestWithSessionData[AnyContent]
  ): HtmlFormat.Appendable =
    if (feature.EntryNumber.isEnabled()) enterMovementReferenceNumberPage(form, isAmend, router)
    else enterNoLegacyMrnPage(form, isAmend, router)

  private def resolveMessagesKey(feature: FeatureSwitchService): String =
    if (feature.EntryNumber.isEnabled()) enterMovementReferenceNumberKey else enterNoLegacyMrnKey

  def enterJourneyMrn(journey: JourneyBindable): Action[AnyContent]  = changeOrEnterMrn(isAmend = false, journey)
  def changeJourneyMrn(journey: JourneyBindable): Action[AnyContent] = changeOrEnterMrn(isAmend = true, journey)

  protected def changeOrEnterMrn(isAmend: Boolean, journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[MovementReferenceNumber] { (_, previousAnswer) =>
        val router    = localRouter(journey)
        val emptyForm = EnterMovementReferenceNumberController.movementReferenceNumberForm(
          resolveMessagesKey(featureSwitch),
          featureSwitch.EntryNumber.isEnabled()
        )
        val form      = previousAnswer.fold(emptyForm)(emptyForm.fill)
        Ok(resolveEnterMrnPageFor(featureSwitch)(form, isAmend, router))
      }
    }

  def enterMrnSubmit(journey: JourneyBindable): Action[AnyContent]  = mrnSubmit(false, journey)
  def changeMrnSubmit(journey: JourneyBindable): Action[AnyContent] = mrnSubmit(true, journey)

  def mrnSubmit(isAmend: Boolean, journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[MovementReferenceNumber] { (fillingOutClaim, previousAnswer) =>
        EnterMovementReferenceNumberController
          .movementReferenceNumberForm(resolveMessagesKey(featureSwitch), featureSwitch.EntryNumber.isEnabled())
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                resolveEnterMrnPageFor(featureSwitch)(
                  formWithErrors
                    .copy(errors =
                      Seq(
                        EnterMovementReferenceNumberController.processFormErrors(
                          resolveMessagesKey(featureSwitch),
                          formWithErrors.errors
                        )
                      )
                    ),
                  isAmend,
                  getRoutes(journey, Some(MovementReferenceNumber(Right(MRN("")))))
                )
              ),
            mrnOrEntryNumber => {
              val errorRedirect: Error => Result = e => {
                logger.warn("Mrn or Entry Number submission failed: ", e)
                Redirect(baseRoutes.IneligibleController.ineligible())
              }
              val previousValue                  = previousAnswer.map(_.stringValue).getOrElse("")
              val currentValue                   = mrnOrEntryNumber.value.map(_.value).leftMap(_.value).merge
              (previousValue === currentValue && isAmend) match {
                case true  =>
                  Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
                case false =>
                  mrnOrEntryNumber match {
                    case entryNumberAnswer @ MovementReferenceNumber(Left(_)) =>
                      EitherT(updateSession(sessionStore, request)(updateMRN(fillingOutClaim, entryNumberAnswer)))
                        .leftMap(_ => Error("Could not save Entry Number"))
                        .fold(
                          errorRedirect,
                          _ => Redirect(getRoutes(journey, Option(mrnOrEntryNumber)).nextPageForEnterMRN(ErnImporter))
                        )
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
                              updateMrnAndAcc14(fillingOutClaim, mrnAnswer, declaration)
                            )
                          )
                            .leftMap(_ => Error("Could not save Display Declaration"))
                      } yield mrnJourneyFlow
                      result.fold(
                        errorRedirect,
                        mrnJourney =>
                          Redirect(getRoutes(journey, Option(mrnOrEntryNumber)).nextPageForEnterMRN(mrnJourney))
                      )
                  }
              }
            }
          )
      }
    }

  def updateMRN(fillingOutClaim: FillingOutClaim, mrnOrEntryNumber: MovementReferenceNumber): SessionDataTransform = {
    val updatedDraftClaim = fillingOutClaim.draftClaim.fold(_.copy(movementReferenceNumber = Option(mrnOrEntryNumber)))
    updateDraftClaim(fillingOutClaim, updatedDraftClaim)
  }

  def updateMrnAndAcc14(
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

  def localRouter(journey: JourneyBindable): ReimbursementRoutes =
    journey match {
      case JourneyBindable.Single   => MRNSingleRoutes
      case JourneyBindable.Bulk     => MRNBulkRoutes
      case JourneyBindable.Schedule => MRNScheduledRoutes
    }

}

object EnterMovementReferenceNumberController {

  val enterMovementReferenceNumberKey = "enter-movement-reference-number"
  val enterNoLegacyMrnKey             = "enter-no-legacy-mrn"

  def movementReferenceNumberMapping(isEntryNumberEnabled: Boolean): Mapping[Either[EntryNumber, MRN]] =
    nonEmptyText
      .verifying("invalid.number", str => EntryNumber.isValid(str) && isEntryNumberEnabled || MRN.isValid(str))
      .transform[Either[EntryNumber, MRN]](
        str =>
          if (MRN.isValid(str)) Right(MRN.changeToUpperCaseWithoutSpaces(str))
          else Left(EntryNumber.changeToUpperCaseWithoutSpaces(str)),
        {
          case Left(entryNumber) => entryNumber.value
          case Right(mrn)        => mrn.value
        }
      )

  def movementReferenceNumberForm(key: String, isEntryNumberEnabled: Boolean): Form[MovementReferenceNumber] =
    Form(
      mapping(
        key -> movementReferenceNumberMapping(isEntryNumberEnabled)
      )(MovementReferenceNumber.apply)(MovementReferenceNumber.unapply)
    )

  def processFormErrors(key: String, errors: Seq[FormError]): FormError =
    if (errors.exists(fe => fe.message === "error.required")) {
      FormError(key, List("error.required"))
    } else if (errors.exists(fe => fe.message === "invalid.reference"))
      FormError(key, List("invalid.reference"))
    else
      FormError(key, List("invalid"))

  def evaluateMrnJourneyFlow(
    signedInUserDetails: SignedInUserDetails,
    maybeDisplayDeclaration: Option[DisplayDeclaration]
  ): Either[Error, MrnJourney] =
    maybeDisplayDeclaration match {
      case Some(displayDeclaration) =>
        (
          displayDeclaration.displayResponseDetail.consigneeDetails,
          Some(displayDeclaration.displayResponseDetail.declarantDetails.declarantEORI)
        ) match {
          case (None, _)                                        => Right(ThirdPartyImporter(displayDeclaration))
          case (Some(consigneeDetails), Some(declarantDetails)) =>
            if (consigneeDetails.consigneeEORI === signedInUserDetails.eori.value)
              Right(MrnImporter(displayDeclaration))
            else if (
              consigneeDetails.consigneeEORI =!= signedInUserDetails.eori.value || declarantDetails =!= signedInUserDetails.eori.value
            )
              Right(ThirdPartyImporter(displayDeclaration))
            else Right(ThirdPartyImporter(displayDeclaration))
          case _                                                =>
            Left(Error("could not determine if signed in user's Eori matches any on the declaration"))
        }

      case None => Left(Error("received no declaration information"))
    }

}
