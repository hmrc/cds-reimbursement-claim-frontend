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

import cats.Applicative
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.{ContactAddress, Country}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
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
    router.journeyBindable match {
      case JourneyBindable.Single =>
        feature.EntryNumber.isEnabled() match {
          case true  => enterMovementReferenceNumberPage(form, isAmend, router)
          case false => enterNoLegacyMrnPage(form, isAmend, router)
        }
      case _                      => enterMovementReferenceNumberPage(form, isAmend, router)
    }

  def enterJourneyMrn(journey: JourneyBindable): Action[AnyContent]  = changeOrEnterMrn(isAmend = false, journey)
  def changeJourneyMrn(journey: JourneyBindable): Action[AnyContent] = changeOrEnterMrn(isAmend = true, journey)

  protected def changeOrEnterMrn(isAmend: Boolean, journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[MovementReferenceNumber] { (_, previousAnswer) =>
        val router    = localRouter(journey)
        val emptyForm = movementReferenceNumberForm(featureSwitch.EntryNumber.isEnabled())
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
          .movementReferenceNumberForm(featureSwitch.EntryNumber.isEnabled())
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                resolveEnterMrnPageFor(featureSwitch)(
                  formWithErrors.copy(errors =
                    transformFormErrors(featureSwitch.EntryNumber.isEnabled(), journey, formWithErrors.errors)
                  ),
                  isAmend,
                  localRouter(journey)
                )
              ),
            mrnOrEntryNumber => {
              val errorRedirect: Error => Result = e => {
                logger.warn("Mrn or Entry Number submission failed: ", e)
                Redirect(baseRoutes.IneligibleController.ineligible())
              }
              val previousValue                  = previousAnswer.map(_.stringValue).getOrElse("")
              val currentValue                   = mrnOrEntryNumber.stringValue
              (previousValue === currentValue && isAmend) match {
                case true  =>
                  Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers(journey))
                case false =>
                  mrnOrEntryNumber match {
                    case entryNumberAnswer @ MovementReferenceNumber(Left(_)) =>
                      EitherT(updateSession(sessionStore, request)(updateMRN(fillingOutClaim, entryNumberAnswer)))
                        .leftMap(_ => Error("Could not save Entry Number"))
                        .fold(
                          errorRedirect,
                          _ =>
                            Redirect(
                              getRoutes(
                                getNumberOfClaims(fillingOutClaim.draftClaim),
                                Option(mrnOrEntryNumber),
                                journey
                              ).nextPageForEnterMRN(ErnImporter)
                            )
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
                        contactDetails  = extractContactDetails(
                                            declaration,
                                            mrnJourneyFlow,
                                            fillingOutClaim.signedInUserDetails.verifiedEmail
                                          )
                        contactAddress  = extractContactAddress(declaration, mrnJourneyFlow)
                        _              <-
                          EitherT(
                            updateSession(sessionStore, request)(
                              updateMrnAndAcc14(fillingOutClaim, mrnAnswer, declaration, contactDetails, contactAddress)
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
    acc14: DisplayDeclaration,
    maybeContactDetails: Option[MrnContactDetails],
    maybeContactAddress: Option[ContactAddress]
  ): SessionDataTransform = {
    val updatedDraftClaim = fillingOutClaim.draftClaim.fold(
      _.copy(
        movementReferenceNumber = Option(mrnOrEntryNumber),
        displayDeclaration = Option(acc14),
        mrnContactDetailsAnswer = maybeContactDetails,
        mrnContactAddressAnswer = maybeContactAddress
      )
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
      case JourneyBindable.Single    => MRNSingleRoutes
      case JourneyBindable.Bulk      => MRNBulkRoutes
      case JourneyBindable.Scheduled => MRNScheduledRoutes
    }

}

object EnterMovementReferenceNumberController {

  val enterMovementReferenceNumberKey: String = "enter-movement-reference-number"
  val enterNoLegacyMrnKey: String             = "enter-no-legacy-mrn"

  def transformFormErrors(
    isEntryNumberEnabled: Boolean,
    journeyBindable: JourneyBindable,
    errors: Seq[FormError]
  ): Seq[FormError] = {
    val langKey =
      if (!isEntryNumberEnabled && journeyBindable === JourneyBindable.Single) enterNoLegacyMrnKey
      else enterMovementReferenceNumberKey
    errors.map(_.copy(key = langKey))
  }

  def movementReferenceNumberMapping(isEntryNumberEnabled: Boolean): Mapping[Either[EntryNumber, MRN]] =
    nonEmptyText
      .verifying(
        "invalid.number",
        str => EntryNumber.isValid(str) && isEntryNumberEnabled || MRN.isValid(str)
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

  def movementReferenceNumberForm(isEntryNumberEnabled: Boolean): Form[MovementReferenceNumber] =
    Form(
      mapping(
        enterMovementReferenceNumberKey -> movementReferenceNumberMapping(isEntryNumberEnabled)
      )(MovementReferenceNumber.apply)(MovementReferenceNumber.unapply)
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
            else Right(ThirdPartyImporter(displayDeclaration))
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
      Applicative[Option].map2(contactDetails.addressLine1, contactDetails.postalCode) { (addressLine1, postCode) =>
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
