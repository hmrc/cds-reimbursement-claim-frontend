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
import cats.syntax.all._
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckContactDetailsMrnController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupOptions.TimeoutConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DeclarantTypeAnswer, Error, MrnContactDetails, NamePhoneEmail}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{AddressLookupService, FeatureSwitchService}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CheckContactDetailsMrnController @Inject() (
  addressLookupService: AddressLookupService,
  addressLookupConfig: AddressLookupConfig,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  cc: MessagesControllerComponents,
  claimantDetailsPage: pages.check_claimant_details
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

  def addDetails(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        val mandatoryDataAvailable = fillingOutClaim.draftClaim.isMandatoryDataAvailable
        checkClaimantDetailsAnswerForm
          .bindFromRequest()
          .fold(
            formWithErrors => {
              val updatedForm = updatedFormErrors(formWithErrors, mandatoryDataAvailable)
              BadRequest(renderTemplate(updatedForm, fillingOutClaim, router))
            },
            formOk => Redirect(router.nextPageForAddClaimantDetails(formOk, featureSwitch))
          )
      }
    }

  def submit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        val mandatoryDataAvailable = fillingOutClaim.draftClaim.isMandatoryDataAvailable
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
                  Redirect(router.nextPageForChangeClaimantDetails(formOk, featureSwitch))
                case NoClaimantDetailsAnswer  =>
                  val updatedClaim = FillingOutClaim
                    .of(fillingOutClaim)(_.copy(mrnContactDetailsAnswer = None, mrnContactAddressAnswer = None))
                  EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedClaim))))
                    .leftMap(err => Error(s"Could not remove contact details: ${err.message}"))
                    .fold(
                      e => logAndDisplayError("Submit Declarant Type error: ").apply(e),
                      _ => Redirect(router.nextPageForChangeClaimantDetails(formOk, featureSwitch))
                    )
              }
          )
      }
    }

  def changeAddress(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      implicit val timeoutConfig: TimeoutConfig = TimeoutConfig(
        timeoutAmount = viewConfig.timeout,
        timeoutUrl = baseRoutes.StartController.timedOut().url,
        timeoutKeepAliveUrl = viewConfig.buildCompleteSelfUrl(viewConfig.ggKeepAliveUrl).some
      )

      val addressSearchRequest =
        AddressLookupRequest
          .redirectBackTo(routes.CheckContactDetailsMrnController.updateAddress(journey))
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

  def updateAddress(journey: JourneyBindable, maybeAddressLookupId: Option[UUID] = None): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (claim, _, _) =>
        def updateLookupAddress(id: UUID) =
          for {
            newAddress <- addressLookupService.retrieveUserAddress(id)
            copyClaim   = FillingOutClaim.of(claim)(_.copy(mrnContactAddressAnswer = newAddress.some))
            result     <- EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = copyClaim.some)))
          } yield result

        maybeAddressLookupId
          .map(updateLookupAddress)
          .getOrElse(EitherT.rightT[Future, Error](()))
          .fold(
            logAndDisplayError("Error updating Address Lookup address: "),
            _ => Redirect(routes.CheckContactDetailsMrnController.show(journey))
          )
      }(dataExtractor, request, journey)
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
    val mandatoryDataAvailable = fillingOutClaim.draftClaim.isMandatoryDataAvailable
    val draftC285Claim         = fillingOutClaim.draftClaim.fold(identity)
    claimantDetailsPage(
      form,
      mandatoryDataAvailable,
      extractDetailsRegisteredWithCDS(fillingOutClaim),
      extractEstablishmentAddress(fillingOutClaim),
      draftC285Claim.mrnContactDetailsAnswer,
      draftC285Claim.mrnContactAddressAnswer,
      router
    )
  }

  def updatedFormErrors[T](formWithErrors: Form[T], mandatoryDataAvailable: Boolean): Form[T] =
    if (mandatoryDataAvailable)
      replaceFormError("error.required", "error.required.change", formWithErrors)
    else replaceFormError("error.required", "error.required.add", formWithErrors)

  def replaceFormError[T](originalError: String, replaceTo: String, formWithErrors: Form[T]): Form[T] =
    formWithErrors.copy(errors = formWithErrors.errors.map { fe =>
      val newMsgs = fe.messages.map(msg => if (msg === originalError) replaceTo else msg)
      FormError(fe.key, newMsgs)
    })

}

object CheckContactDetailsMrnController {
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
      .flatten
  }
}
