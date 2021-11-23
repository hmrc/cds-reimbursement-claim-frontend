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
import play.api.data._
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{JourneyBindable, ReimbursementRoutes, SessionDataExtractor, SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.{MrnImporter, ThirdPartyImporter}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.{ContactAddress, Country}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.{Email, PhoneNumber}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterMovementReferenceNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  claimService: ClaimService,
  enterMovementReferenceNumberPage: pages.enter_movement_reference_number
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  import cats.data.EitherT._
  implicit val dataExtractor: DraftClaim => Option[MRN] = _.movementReferenceNumber

  def enterJourneyMrn(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[MRN] { (_, previousAnswer) =>
        val emptyForm = movementReferenceNumberForm()
        val form      = previousAnswer.fold(emptyForm)(emptyForm.fill)
        Ok(enterMovementReferenceNumberPage(form, ReimbursementRoutes(journey)))
      }
    }

  def enterMrnSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[MRN] { (fillingOutClaim, previousAnswer) =>
        EnterMovementReferenceNumberController
          .movementReferenceNumberForm()
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                enterMovementReferenceNumberPage(
                  formWithErrors,
                  ReimbursementRoutes(journey)
                )
              ),
            mrnNumber => {

              def getDeclaration(mrn: MRN): EitherT[Future, Error, Option[DisplayDeclaration]] =
                claimService
                  .getDisplayDeclaration(mrn)
                  .leftMap(_ => Error("Could not get declaration"))

              val isSameAsPrevious: Boolean =
                previousAnswer.exists(_.value === mrnNumber.value)

              if (isSameAsPrevious && fillingOutClaim.draftClaim.isComplete)
                Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers(journey))
              else if (isSameAsPrevious && journey === JourneyBindable.Multiple)
                Redirect(routes.CheckMovementReferenceNumbersController.showMrns())
              else {
                val result: EitherT[Future, Error, MrnJourney] =
                  for {
                    maybeAcc14     <- if (isSameAsPrevious)
                                        EitherT
                                          .fromOption[Future](
                                            fillingOutClaim.draftClaim.displayDeclaration,
                                            mrnNumber
                                          )
                                          .map(Some.apply)
                                          .leftFlatMap(getDeclaration)
                                      else
                                        getDeclaration(mrnNumber)
                    mrnJourneyFlow <-
                      fromEither[Future](evaluateMrnJourneyFlow(fillingOutClaim.signedInUserDetails, maybeAcc14))
                        .leftMap(_ => Error("could not evaluate MRN flow"))

                    declaration   <-
                      fromOption[Future](maybeAcc14, Error("could not unbox display declaration"))
                    contactDetails = extractContactDetails(
                                       declaration,
                                       mrnJourneyFlow,
                                       fillingOutClaim.signedInUserDetails.verifiedEmail
                                     )

                    contactAddress = extractContactAddress(declaration, mrnJourneyFlow)
                    _             <-
                      EitherT(
                        updateSession(sessionStore, request)(
                          if (fillingOutClaim.draftClaim.isComplete)
                            renewMrnAndAcc14(fillingOutClaim, mrnNumber, declaration, contactDetails, contactAddress)
                          else
                            updateMrnAndAcc14(fillingOutClaim, mrnNumber, declaration, contactDetails, contactAddress)
                        )
                      ).leftMap(_ => Error("Could not save Display Declaration"))
                  } yield mrnJourneyFlow
                result.fold(
                  e => {
                    logger.warn(s"Mrn submission failed: ${e.message}")
                    Redirect(baseRoutes.IneligibleController.ineligible())
                  },
                  mrnJourney =>
                    Redirect(
                      getRoutes(getTypeOfClaim(fillingOutClaim.draftClaim), journey)
                        .nextPageForEnterMRN(mrnJourney)
                    )
                )
              }
            }
          )
      }
    }

  def updateMRN(fillingOutClaim: FillingOutClaim, mrn: MRN): SessionDataTransform = {
    val updatedDraftClaim = fillingOutClaim.draftClaim.copy(movementReferenceNumber = Some(mrn))
    updateDraftClaim(fillingOutClaim, updatedDraftClaim)
  }

  def updateMrnAndAcc14(
    fillingOutClaim: FillingOutClaim,
    mrn: MRN,
    acc14: DisplayDeclaration,
    maybeContactDetails: Option[MrnContactDetails],
    maybeContactAddress: Option[ContactAddress]
  ): SessionDataTransform = {

    val updatedDraftClaim = fillingOutClaim.draftClaim.copy(
      movementReferenceNumber = Some(mrn),
      displayDeclaration = Some(acc14),
      mrnContactDetailsAnswer = maybeContactDetails,
      mrnContactAddressAnswer = maybeContactAddress,
      associatedMRNsAnswer = None,
      associatedMRNsDeclarationAnswer = None
    )

    updateDraftClaim(fillingOutClaim, updatedDraftClaim)
  }

  def renewMrnAndAcc14(
    fillingOutClaim: FillingOutClaim,
    mrn: MRN,
    acc14: DisplayDeclaration,
    maybeContactDetails: Option[MrnContactDetails],
    maybeContactAddress: Option[ContactAddress]
  ): SessionDataTransform = {

    val typeOfClaimAnswer: TypeOfClaimAnswer = getTypeOfClaim(fillingOutClaim.draftClaim)
    val blankDraftClaim                      = DraftClaim.blank
    val updatedDraftClaim                    = DraftClaim.blank.copy(
      typeOfClaim = Some(typeOfClaimAnswer),
      movementReferenceNumber = Some(mrn),
      displayDeclaration = Some(acc14),
      mrnContactDetailsAnswer = maybeContactDetails,
      mrnContactAddressAnswer = maybeContactAddress,
      associatedMRNsAnswer = None,
      associatedMRNsDeclarationAnswer = None
    )
    val journey                              = FillingOutClaim(fillingOutClaim.ggCredId, fillingOutClaim.signedInUserDetails, blankDraftClaim)

    updateDraftClaim(journey, updatedDraftClaim)
  }

  def updateDraftClaim(fillingOutClaim: FillingOutClaim, newDraftClaim: DraftClaim): SessionDataTransform = {
    val update: SessionDataTransform = _.copy(journeyStatus = Some(fillingOutClaim.copy(draftClaim = newDraftClaim)))
    update
  }
}

object EnterMovementReferenceNumberController {

  val enterMovementReferenceNumberKey: String = "enter-movement-reference-number"

  def movementReferenceNumberForm(): Form[MRN] =
    Form(
      mapping(
        enterMovementReferenceNumberKey ->
          nonEmptyText
            .verifying(
              "invalid.number",
              str => str.isEmpty || MRN(str).isValid
            )
            .transform[MRN](MRN(_), _.value)
      )(identity)(Some(_))
    )

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
            else
              Right(ThirdPartyImporter(displayDeclaration))
          case _                                                =>
            Left(Error("could not determine if signed in user's Eori matches any on the declaration"))
        }

      case None => Left(Error("received no declaration information"))
    }

  def extractContactDetails(
    displayDeclaration: DisplayDeclaration,
    mrnJourney: MrnJourney,
    email: Email
  ): Option[MrnContactDetails] =
    mrnJourney match {
      case MrnImporter(_) =>
        val consignee  = displayDeclaration.displayResponseDetail.consigneeDetails
        val maybeName  = consignee.flatMap(_.contactDetails).flatMap(_.contactName)
        val maybePhone = consignee.flatMap(_.contactDetails).flatMap(_.telephone)
        maybeName.map(name => MrnContactDetails(name, email, maybePhone.map(PhoneNumber(_))))
      case _              =>
        val declarant  = displayDeclaration.displayResponseDetail.declarantDetails
        val maybeName  = declarant.contactDetails.flatMap(_.contactName)
        val maybePhone = declarant.contactDetails.flatMap(_.telephone)
        maybeName.map(name => MrnContactDetails(name, email, maybePhone.map(PhoneNumber(_))))
    }

  def extractContactAddress(displayDeclaration: DisplayDeclaration, mrnJourney: MrnJourney): Option[ContactAddress] =
    (mrnJourney match {
      case MrnImporter(_) =>
        val consignee = displayDeclaration.displayResponseDetail.consigneeDetails
        consignee.flatMap(_.contactDetails)
      case _              =>
        val declarant = displayDeclaration.displayResponseDetail.declarantDetails
        declarant.contactDetails
    }).flatMap { contactDetails =>
      (contactDetails.addressLine1, contactDetails.postalCode).mapN { (addressLine1, postCode) =>
        ContactAddress(
          addressLine1,
          contactDetails.addressLine2,
          None,
          contactDetails.addressLine3.getOrElse(""),
          postCode,
          contactDetails.countryCode.map(Country(_)).getOrElse(Country.uk)
        )
      }
    }

}
