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
import com.google.inject.{Inject, Singleton}
import play.api.data.Forms.{mapping, nonEmptyText, of}
import play.api.data.{Form, Mapping}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.TemporaryJourneyExtractor.withAnswers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, TemporaryJourneyExtractor}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.{Logging, TimeUtils}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.time.LocalDate
import scala.concurrent.{ExecutionContext}

@Singleton
class EnterDeclarationDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  featureSwitch: FeatureSwitchService,
  enterDuplicateDeclarationDetailsPage: pages.enter_duplicate_declaration_details,
  enterDeclarationDetailsPage: pages.enter_declaration_details
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[EntryNumberDeclarationDetails] = _.declarationDetailsAnswer

  def enterDeclarationDetails(): Action[AnyContent]  = show(isAmend = false)
  def changeDeclarationDetails(): Action[AnyContent] = show(isAmend = true)

  def show(isAmend: Boolean): Action[AnyContent] =
    (featureSwitch.EntryNumber.hideIfNotEnabled andThen authenticatedActionWithSessionData).async { implicit request =>
      withAnswers[EntryNumberDeclarationDetails] { (fillingOutClaim, answers) =>
        val ernDeclarationDetailsForm =
          answers.toList.foldLeft(EnterDeclarationDetailsController.entryNumberDeclarationDetailsForm)((form, answer) =>
            form.fill(answer)
          )

        fillingOutClaim.draftClaim.movementReferenceNumber
          .fold(Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))) {
            case Left(entryNumber) =>
              Ok(
                enterDeclarationDetailsPage(ernDeclarationDetailsForm, entryNumber, isAmend)
              )
            case Right(_)          =>
              Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
          }

      }
    }

  def enterDeclarationDetailsSubmit(): Action[AnyContent]  = submit(isAmend = false)
  def changeDeclarationDetailsSubmit(): Action[AnyContent] = submit(isAmend = true)

  def submit(isAmend: Boolean): Action[AnyContent] =
    (featureSwitch.EntryNumber.hideIfNotEnabled andThen authenticatedActionWithSessionData).async { implicit request =>
      withAnswers[EntryNumberDeclarationDetails] { (fillingOutClaim, _) =>
        EnterDeclarationDetailsController.entryNumberDeclarationDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              fillingOutClaim.draftClaim.movementReferenceNumber
                .fold(Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))) {
                  case Left(entryNumber) =>
                    BadRequest(
                      enterDeclarationDetailsPage(
                        requestFormWithErrors,
                        entryNumber,
                        isAmend
                      )
                    )
                  case Right(_)          =>
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                },
            declarantDetailAnswers => {

              val newDraftClaim =
                fillingOutClaim.draftClaim.fold(_.copy(declarationDetailsAnswer = Some(declarantDetailAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not capture declaration details", e)
                  errorHandler.errorResult()
                },
                _ =>
                  isAmend match {
                    case true  => Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
                    case false => Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType())

                  }
              )
            }
          )
      }
    }

  def enterDuplicateDeclarationDetails(): Action[AnyContent] =
    (featureSwitch.EntryNumber.hideIfNotEnabled andThen authenticatedActionWithSessionData).async { implicit request =>
      withAnswers[EntryNumberDeclarationDetails] { (fillingOutClaim, answers) =>
        val ernDeclarationDetailsForm =
          answers.toList.foldLeft(EnterDeclarationDetailsController.entryNumberDeclarationDetailsForm)((form, answer) =>
            form.fill(answer)
          )

        fillingOutClaim.draftClaim.movementReferenceNumber
          .fold(Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))) {
            case Left(entryNumber) =>
              Ok(
                enterDeclarationDetailsPage(ernDeclarationDetailsForm, entryNumber)
              )
            case Right(_)          =>
              Redirect(
                routes.EnterDuplicateMovementReferenceNumberController.enterDuplicateMrn(
                  TemporaryJourneyExtractor.extractJourney
                )
              )
          }
      }
    }

  def enterDuplicateDeclarationDetailsSubmit(): Action[AnyContent] =
    (featureSwitch.EntryNumber.hideIfNotEnabled andThen authenticatedActionWithSessionData).async { implicit request =>
      withAnswers[EntryNumberDeclarationDetails] { (fillingOutClaim, _) =>
        EnterDeclarationDetailsController.entryNumberDeclarationDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              fillingOutClaim.draftClaim.movementReferenceNumber
                .fold(Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))) {
                  case Left(entryNumber) =>
                    BadRequest(
                      enterDuplicateDeclarationDetailsPage(
                        requestFormWithErrors,
                        entryNumber
                      )
                    )
                  case Right(_)          =>
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                },
            declarantDetailAnswers => {

              val newDraftClaim =
                fillingOutClaim.draftClaim
                  .fold(_.copy(duplicateDeclarationDetailsAnswer = Some(declarantDetailAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not capture duplicate declaration details", e)
                  errorHandler.errorResult()
                },
                _ =>
                  Redirect(
                    routes.EnterCommoditiesDetailsController
                      .enterCommoditiesDetails(TemporaryJourneyExtractor.extractJourney)
                  )
              )
            }
          )
      }
    }
}

object EnterDeclarationDetailsController {

  val entryNumberDeclarationDetailsForm: Form[EntryNumberDeclarationDetails] = Form(
    mapping(
      "enter-declaration-details.date-of-import"          -> dateOfImportMapping(LocalDate.now),
      "enter-declaration-details.place-of-import"         -> nonEmptyText(maxLength = 70),
      "enter-declaration-details.importer-name"           -> nonEmptyText(maxLength = 70),
      "enter-declaration-details.importer-email-address"  -> Email.mappingMaxLength,
      "enter-declaration-details.importer-phone-number"   -> PhoneNumber.mapping,
      "enter-declaration-details.declarant-name"          -> nonEmptyText(maxLength = 70),
      "enter-declaration-details.declarant-email-address" -> Email.mappingMaxLength,
      "enter-declaration-details.declarant-phone-number"  -> PhoneNumber.mapping
    )(EntryNumberDeclarationDetails.apply)(EntryNumberDeclarationDetails.unapply)
  )

  def dateOfImportMapping(today: LocalDate): Mapping[DateOfImport] =
    mapping(
      "" -> of(
        TimeUtils.dateFormatter(
          Some(today),
          None,
          "enter-declaration-details.day",
          "enter-declaration-details.month",
          "enter-declaration-details.year",
          "enter-declaration-details"
        )
      )
    )(DateOfImport(_))(d => Some(d.value))

}
