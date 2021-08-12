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
import cats.implicits.catsSyntaxOptionId
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import julienrf.json.derived
import play.api.data.Forms.{mapping, number}
import play.api.data.{Form, FormError}
import play.api.i18n.Messages
import play.api.libs.json.OFormat
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{AddressLookupConfig, ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckClaimantDetailsController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupOptions.TimeoutConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DeclarantTypeAnswer, Error, MrnContactDetails, NamePhoneEmail}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckClaimantDetailsController @Inject() (
  addressLookupService: AddressLookupService,
  addressLookupConfig: AddressLookupConfig,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  claimantDetails: pages.check_claimant_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with SessionDataExtractor
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[MrnContactDetails] = _.mrnContactDetailsAnswer

  def show(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        Ok(renderTemplate(checkClaimantDetailsAnswerForm, fillingOutClaim, router))
      }
    }

  def add(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        val mandatoryDataAvailable = validateSessionOrAcc14(fillingOutClaim)
        checkClaimantDetailsAnswerForm
          .bindFromRequest()
          .fold(
            formWithErrors => {
              val updatedForm = updatedFormErrors(formWithErrors, mandatoryDataAvailable)
              BadRequest(renderTemplate(updatedForm, fillingOutClaim, router))
            },
            formOk => Redirect(router.nextPageForAddClaimantDetails(formOk))
          )
      }
    }

  def change(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        val mandatoryDataAvailable = validateSessionOrAcc14(fillingOutClaim)
        checkClaimantDetailsAnswerForm
          .bindFromRequest()
          .fold(
            formWithErrors => {
              val updatedForm = updatedFormErrors(formWithErrors, mandatoryDataAvailable)
              BadRequest(renderTemplate(updatedForm, fillingOutClaim, router))
            },
            formOk =>
              formOk match {
                case YesClaimantDetailsAnswer =>
                  Redirect(router.nextPageForChangeClaimantDetails(formOk))
                case NoClaimantDetailsAnswer  =>
                  val updatedClaim = removeContactDetails(fillingOutClaim)
                  EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedClaim))))
                    .leftMap(err => Error(s"Could not remove contact details: ${err.message}"))
                    .fold(
                      e => logAndDisplayError("Submit Declarant Type error: ").apply(e),
                      _ => Redirect(router.nextPageForChangeClaimantDetails(formOk))
                    )
              }
          )
      }
    }

  def changeAddress(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      implicit val timeoutConfig: TimeoutConfig = TimeoutConfig(
        timeoutAmount = viewConfig.timeout,
        timeoutUrl = viewConfig.buildCompleteSelfUrl(baseRoutes.StartController.timedOut()),
        timeoutKeepAliveUrl = viewConfig.buildCompleteSelfUrl(viewConfig.ggKeepAliveUrl).some
      )

      val addressSearchRequest =
        AddressLookupRequest
          .redirectBackTo(routes.CheckClaimantDetailsController.updateAddress(journey))
          .signOutUserVia(viewConfig.signOutUrl)
          .nameServiceAs("cds-reimbursement-claim")
          .maximumShow(addressLookupConfig.maxAddressesToShow)
          .makeAccessibilityFooterAvailableVia(viewConfig.accessibilityStatementUrl)
          .makePhaseFeedbackAvailableVia(viewConfig.contactHmrcUrl)
          .searchUkAddressOnly(true)
          .showConfirmChangeText(true)
          .showSearchAgainLink(true)
          .showChangeLink(true)
          .showBanner(true)

      val response = addressLookupService initiate addressSearchRequest

      response.fold(logAndDisplayError("Error occurred starting address lookup: "), url => Redirect(url.toString))
    }

  def updateAddress(journey: JourneyBindable, maybeId: Option[UUID] = None): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      println(journey)
      maybeId.fold(Future.successful(BadRequest("")))(
        addressLookupService
          .retrieveUserAddress(_)
          .fold(
            logAndDisplayError("Error retrieving address: "),
            address => Ok(address.toString)
          )
      )
    }

  def renderTemplate(
    form: Form[CheckClaimantDetailsAnswer],
    fillingOutClaim: FillingOutClaim,
    router: ReimbursementRoutes
  )(implicit
    request: RequestWithSessionData[_],
    messages: Messages,
    viewConfig: ViewConfig
  ): HtmlFormat.Appendable = {
    val mandatoryDataAvailable = validateSessionOrAcc14(fillingOutClaim)
    claimantDetails(
      form,
      mandatoryDataAvailable,
      extractDetailsRegisteredWithCDS(fillingOutClaim),
      extractEstablishmentAddress(fillingOutClaim),
      extractContactDetails(fillingOutClaim),
      extractContactAddress(fillingOutClaim),
      router
    )
  }

  def updatedFormErrors[T](formWithErrors: Form[T], mandatoryDataAvailable: Boolean): Form[T] =
    mandatoryDataAvailable match {
      case true  => replaceFormError("error.required", "error.required.change", formWithErrors)
      case false => replaceFormError("error.required", "error.required.add", formWithErrors)
    }

  def replaceFormError[T](originalError: String, replaceTo: String, formWithErrors: Form[T]): Form[T] =
    formWithErrors.copy(errors = formWithErrors.errors.map { fe =>
      val newMsgs = fe.messages.map(msg => if (msg === originalError) replaceTo else msg)
      FormError(fe.key, newMsgs)
    })

}

object CheckClaimantDetailsController {
  val languageKey: String = "claimant-details"

  sealed trait CheckClaimantDetailsAnswer extends Product with Serializable

  case object YesClaimantDetailsAnswer extends CheckClaimantDetailsAnswer
  case object NoClaimantDetailsAnswer extends CheckClaimantDetailsAnswer

  val checkClaimantDetailsAnswerForm: Form[CheckClaimantDetailsAnswer] =
    Form(
      mapping(
        languageKey -> number
          .verifying("invalid", a => a === 0 || a === 1)
          .transform[CheckClaimantDetailsAnswer](
            value =>
              if (value === 0) YesClaimantDetailsAnswer
              else NoClaimantDetailsAnswer,
            {
              case YesClaimantDetailsAnswer => 0
              case NoClaimantDetailsAnswer  => 1
            }
          )
      )(identity)(Some(_))
    )

  implicit val format: OFormat[CheckClaimantDetailsAnswer] = derived.oformat[CheckClaimantDetailsAnswer]()

  def extractDetailsRegisteredWithCDS(fillingOutClaim: FillingOutClaim): NamePhoneEmail = {
    val draftC285Claim = fillingOutClaim.draftClaim.fold(identity)
    val email          = fillingOutClaim.signedInUserDetails.verifiedEmail
    Applicative[Option]
      .map2(draftC285Claim.displayDeclaration, draftC285Claim.declarantTypeAnswer) { (declaration, declarantType) =>
        declarantType match {
          case DeclarantTypeAnswer.Importer | DeclarantTypeAnswer.AssociatedWithImporterCompany =>
            val consignee = declaration.displayResponseDetail.consigneeDetails
            val name      = consignee.map(_.legalName)
            val phone     = consignee.flatMap(_.contactDetails.flatMap(_.telephone))
            NamePhoneEmail(name, phone.map(PhoneNumber(_)), Some(email))
          case DeclarantTypeAnswer.AssociatedWithRepresentativeCompany                          =>
            val declarant = declaration.displayResponseDetail.declarantDetails
            val name      = declarant.legalName
            val phone     = declarant.contactDetails.flatMap(_.telephone)
            NamePhoneEmail(Some(name), phone.map(PhoneNumber(_)), Some(email))
        }
      }
      .getOrElse(NamePhoneEmail(None, None, None))
  }

  def extractEstablishmentAddress(fillingOutClaim: FillingOutClaim): Option[EstablishmentAddress] = {
    val draftC285Claim = fillingOutClaim.draftClaim.fold(identity)
    Applicative[Option]
      .map2(draftC285Claim.displayDeclaration, draftC285Claim.declarantTypeAnswer) { (declaration, declarantType) =>
        declarantType match {
          case DeclarantTypeAnswer.Importer | DeclarantTypeAnswer.AssociatedWithImporterCompany =>
            declaration.displayResponseDetail.consigneeDetails.map(_.establishmentAddress)
          case DeclarantTypeAnswer.AssociatedWithRepresentativeCompany                          =>
            Some(declaration.displayResponseDetail.declarantDetails.establishmentAddress)
        }
      }
      .getOrElse(None)
  }

  def extractContactDetails(fillingOutClaim: FillingOutClaim): NamePhoneEmail = {
    val draftC285Claim = fillingOutClaim.draftClaim.fold(identity)
    val email          = fillingOutClaim.signedInUserDetails.verifiedEmail
    draftC285Claim.mrnContactDetailsAnswer
      .map { contact =>
        NamePhoneEmail(Option(contact.fullName), Option(contact.phoneNumber), Option(contact.emailAddress))
      }
      .orElse(
        Applicative[Option]
          .map2(draftC285Claim.displayDeclaration, draftC285Claim.declarantTypeAnswer) { (declaration, declarantType) =>
            declarantType match {
              case DeclarantTypeAnswer.Importer | DeclarantTypeAnswer.AssociatedWithImporterCompany =>
                val consignee = declaration.displayResponseDetail.consigneeDetails
                val name      = consignee.flatMap(_.contactDetails).flatMap(_.contactName)
                val phone     = consignee.flatMap(_.contactDetails).flatMap(_.telephone)
                NamePhoneEmail(name, phone.map(PhoneNumber(_)), Some(email))
              case DeclarantTypeAnswer.AssociatedWithRepresentativeCompany                          =>
                val declarant = declaration.displayResponseDetail.declarantDetails
                val name      = declarant.contactDetails.flatMap(_.contactName)
                val phone     = declarant.contactDetails.flatMap(_.telephone)
                NamePhoneEmail(name, phone.map(PhoneNumber(_)), Some(email))
            }
          }
      )
      .getOrElse(NamePhoneEmail(None, None, None))
  }

  def extractContactAddress(fillingOutClaim: FillingOutClaim): Option[NonUkAddress] = {
    val draftC285Claim = fillingOutClaim.draftClaim.fold(identity)
    draftC285Claim.mrnContactAddressAnswer
      .orElse(
        Applicative[Option]
          .map2(draftC285Claim.displayDeclaration, draftC285Claim.declarantTypeAnswer) { (declaration, declarantType) =>
            (declarantType match {
              case DeclarantTypeAnswer.Importer | DeclarantTypeAnswer.AssociatedWithImporterCompany =>
                declaration.displayResponseDetail.consigneeDetails.flatMap(_.contactDetails)
              case DeclarantTypeAnswer.AssociatedWithRepresentativeCompany                          =>
                declaration.displayResponseDetail.declarantDetails.contactDetails
            }).flatMap { contactDetails =>
              Applicative[Option].map2(contactDetails.addressLine1, contactDetails.postalCode) {
                (addressLine1, postCode) =>
                  NonUkAddress(
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
          .flatten
      )
  }

}
