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
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import julienrf.json.derived
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.i18n.Messages
import play.api.libs.json.OFormat
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckClaimantDetailsController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DeclarantTypeAnswer, Error, NamePhoneEmail}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class CheckClaimantDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  claimantDetails: pages.check_claimant_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with SessionDataExtractor
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[CheckClaimantDetailsAnswer] = _.checkClaimantDetailsAnswer

  def show(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[CheckClaimantDetailsAnswer] { (fillingOutClaim, answers, router) =>
        val emptyForm  = checkClaimantDetailsAnswerForm
        val filledForm = answers.fold(emptyForm)(emptyForm.fill(_))
        Ok(renderTemplate(filledForm, fillingOutClaim, router))
      }
    }

  def submit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[CheckClaimantDetailsAnswer] { (fillingOutClaim, _, router) =>
        checkClaimantDetailsAnswerForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(renderTemplate(formWithErrors, fillingOutClaim, router)),
            formOk => {
              val newDraftClaim  = fillingOutClaim.draftClaim.fold(_.copy(checkClaimantDetailsAnswer = Option(formOk)))
              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap(err => Error(s"Could not save Declarant Type: ${err.message}"))
                .fold(
                  e => {
                    logger.warn("Submit Declarant Type error: ", e)
                    errorHandler.errorResult()
                  },
                  _ => Redirect(router.nextPageForClaimantDetails())
                )
            }
          )
      }

    }

  def renderTemplate(
    form: Form[CheckClaimantDetailsAnswer],
    fillingOutClaim: FillingOutClaim,
    router: ReimbursementRoutes
  )(implicit
    request: RequestWithSessionData[_],
    messages: Messages,
    viewConfig: ViewConfig
  ): HtmlFormat.Appendable =
    claimantDetails(
      form,
      extractDetailsRegisteredWithCDS(fillingOutClaim),
      extractEstablishmentAddress(fillingOutClaim),
      extractContactDetails(fillingOutClaim),
      extractContactAddress(fillingOutClaim),
      router
    )

}

object CheckClaimantDetailsController {
  val languageKey: String = "claimant-details"

  sealed trait CheckClaimantDetailsAnswer extends Product with Serializable

  case object UseContactDetails extends CheckClaimantDetailsAnswer
  case object UseDetailsRegisteredWithCDS extends CheckClaimantDetailsAnswer

  val checkClaimantDetailsAnswerForm: Form[CheckClaimantDetailsAnswer] =
    Form(
      mapping(
        languageKey -> number
          .verifying("invalid", a => a === 0 || a === 1)
          .transform[CheckClaimantDetailsAnswer](
            value =>
              if (value === 0) UseContactDetails
              else UseDetailsRegisteredWithCDS,
            {
              case UseContactDetails           => 0
              case UseDetailsRegisteredWithCDS => 1
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
      .getOrElse(NamePhoneEmail(None, None, None))
  }

  def extractContactAddress(fillingOutClaim: FillingOutClaim): Option[NonUkAddress] = {
    val draftC285Claim = fillingOutClaim.draftClaim.fold(identity)
    Applicative[Option]
      .map2(draftC285Claim.displayDeclaration, draftC285Claim.declarantTypeAnswer) { (declaration, declarantType) =>
        (declarantType match {
          case DeclarantTypeAnswer.Importer | DeclarantTypeAnswer.AssociatedWithImporterCompany =>
            declaration.displayResponseDetail.consigneeDetails.flatMap(_.contactDetails)
          case DeclarantTypeAnswer.AssociatedWithRepresentativeCompany                          =>
            declaration.displayResponseDetail.declarantDetails.contactDetails
        }).flatMap{ contactDetails =>
        Applicative[Option].map2(contactDetails.addressLine1, contactDetails.postalCode ) { (addressLine1,postCode) =>
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
      }.flatten
  }

  def validateSessionOrAcc14(fillingOutClaim: FillingOutClaim): Boolean = {
    val draftC285Claim = fillingOutClaim.draftClaim.fold(identity)
    if (
      draftC285Claim.mrnContactDetailsAnswer.isDefined &&
      draftC285Claim.mrnContactAddressAnswer.isDefined
    )
      true
    else if (
      extractContactDetails(fillingOutClaim).name.isDefined &&
      extractContactDetails(fillingOutClaim).email.isDefined &&
      extractContactAddress(fillingOutClaim).map(_.line1).isDefined &&
      extractContactAddress(fillingOutClaim).map(_.postcode).isDefined
    )
      true
    else
      false
  }

}
