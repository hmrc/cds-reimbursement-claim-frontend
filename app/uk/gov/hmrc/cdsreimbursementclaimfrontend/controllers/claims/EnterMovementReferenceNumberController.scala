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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.evaluateMrnJourneyFlow
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.{MrnImporter, ThirdPartyImporter}
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
  enterMovementReferenceNumberPage: pages.enter_movement_reference_number
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  import cats.data.EitherT._
  implicit val dataExtractor: DraftC285Claim => Option[MovementReferenceNumber] = _.movementReferenceNumber

  def enterMrn(): Action[AnyContent]  = changeOrEnterMrn(false)
  def changeMrn(): Action[AnyContent] = changeOrEnterMrn(true)

  protected def changeOrEnterMrn(isCYA: Boolean): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[MovementReferenceNumber] { (_, previousAnswer) =>
        val emptyForm = EnterMovementReferenceNumberController.movementReferenceNumberForm(featureSwitch)
        val form      = previousAnswer.fold(emptyForm)(emptyForm.fill _)
        Ok(enterMovementReferenceNumberPage(form, isCYA))
      }
    }

  def enterMrnSubmit(): Action[AnyContent]  = mrnSubmit(false)
  def changeMrnSubmit(): Action[AnyContent] = mrnSubmit(true)

  def mrnSubmit(isCYA:Boolean): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[MovementReferenceNumber] { (fillingOutClaim, previousAnswer) =>
        EnterMovementReferenceNumberController
          .movementReferenceNumberForm(featureSwitch)
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                enterMovementReferenceNumberPage(
                  formWithErrors
                    .copy(errors = Seq(EnterMovementReferenceNumberController.processFormErrors(formWithErrors.errors)))
                )
              ),
            mrnOrEntryNumber => {
              val errorRedirect: Error => Result = e => {
                logger.warn("Mrn or Entry Number submission failed: ", e)
                Redirect(baseRoutes.IneligibleController.ineligible())
              }
              val previousValue                  = previousAnswer.map(_.stringValue).getOrElse("")
              val currentValue                   = mrnOrEntryNumber.value.map(_.value).leftMap(_.value).merge
              (previousValue === currentValue && isCYA) match {
                case true  =>
                  Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
                case false =>
                  mrnOrEntryNumber match {
                    case entryNumberAnswer @ MovementReferenceNumber(Left(_)) =>
                      EitherT(updateSession(sessionStore, request)(updateMRN(fillingOutClaim, entryNumberAnswer)))
                        .leftMap(_ => Error("Could not save Entry Number"))
                        .fold(
                          errorRedirect,
                          _ => Redirect(routes.EnterDeclarationDetailsController.enterDeclarationDetails())
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
                        {
                          case Left(_)  => Redirect(routes.CheckDeclarationDetailsController.checkDetails())
                          case Right(_) => Redirect(routes.EnterImporterEoriNumberController.enterImporterEoriNumber())
                        }
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

}

object EnterMovementReferenceNumberController {

  def movementReferenceNumberMapping(featureSwitch: FeatureSwitchService): Mapping[Either[EntryNumber, MRN]] =
    nonEmptyText
      .verifying(
        "invalid.number",
        str => EntryNumber.isValid(str) && featureSwitch.EntryNumber.isEnabled || MRN.isValid(str)
      )
      .transform[Either[EntryNumber, MRN]](
        str =>
          if (MRN.isValid(str)) Right(MRN.changeToUpperCaseWithoutSpaces(str))
          else Left(EntryNumber.changeToUpperCaseWithoutSpaces(str)),
        {
          case Left(entryNumber) => entryNumber.value
          case Right(mrn)        => mrn.value
        }
      )

  val movementReferenceNumberForm: FeatureSwitchService => Form[MovementReferenceNumber] = featureSwitch =>
    Form(
      mapping(
        "enter-movement-reference-number" -> movementReferenceNumberMapping(featureSwitch)
      )(MovementReferenceNumber.apply)(MovementReferenceNumber.unapply)
    )

  def processFormErrors(errors: Seq[FormError]): FormError =
    if (errors.exists(fe => fe.message === "error.required")) {
      FormError("enter-movement-reference-number", List("error.required"))
    } else if (errors.exists(fe => fe.message === "invalid.reference"))
      FormError("enter-movement-reference-number", List("invalid.reference"))
    else
      FormError("enter-movement-reference-number", List("invalid"))

  def evaluateMrnJourneyFlow(
    signedInUserDetails: SignedInUserDetails,
    maybeDisplayDeclaration: Option[DisplayDeclaration]
  ): Either[Error, Either[MrnImporter, ThirdPartyImporter]] =
    maybeDisplayDeclaration match {
      case Some(displayDeclaration) =>
        (
          displayDeclaration.displayResponseDetail.consigneeDetails,
          Some(displayDeclaration.displayResponseDetail.declarantDetails.declarantEORI)
        ) match {
          case (None, _)                                        => Right(Right(ThirdPartyImporter(displayDeclaration)))
          case (Some(consigneeDetails), Some(declarantDetails)) =>
            if (consigneeDetails.consigneeEORI === signedInUserDetails.eori.value)
              Right(Left(MrnImporter(displayDeclaration)))
            else if (
              consigneeDetails.consigneeEORI =!= signedInUserDetails.eori.value || declarantDetails =!= signedInUserDetails.eori.value
            )
              Right(Right(ThirdPartyImporter(displayDeclaration)))
            else Right(Right(ThirdPartyImporter(displayDeclaration)))
          case _                                                => Left(Error("could not determine if signed in user's Eori matches any on the declaration"))
        }

      case None => Left(Error("received no declaration information"))
    }

}
