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
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DeclarantTypeAnswer, DetailsRegisteredWithCdsAnswer, NamePhoneEmail}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckClaimantDetailsController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Country
@Singleton
class CheckClaimantDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  featureSwitch: FeatureSwitchService,
  claimantDetails: pages.check_claimant_details
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with SessionDataExtractor
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[DetailsRegisteredWithCdsAnswer] =
    _.detailsRegisteredWithCdsAnswer

  def show(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[DetailsRegisteredWithCdsAnswer] { (fillingOutClaim, _, router) =>
        val namePhoneEmail       = extractContactsRegisteredWithCDSA(fillingOutClaim)
        val establishmentAddress = extractEstablishmentAddress(fillingOutClaim)
        val contactDetails       = extractContactDetails(fillingOutClaim)
        val contactAddress       = extractContactAddress(fillingOutClaim)
        Ok(claimantDetails(namePhoneEmail, establishmentAddress, contactDetails, contactAddress, router, featureSwitch))
      }
    }

}

object CheckClaimantDetailsController {
  val languageKey: String = "claimant-details"

  def extractContactsRegisteredWithCDSA(fillingOutClaim: FillingOutClaim): NamePhoneEmail = {
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
        val contactDetails = declarantType match {
          case DeclarantTypeAnswer.Importer | DeclarantTypeAnswer.AssociatedWithImporterCompany =>
            declaration.displayResponseDetail.consigneeDetails.flatMap(_.contactDetails)
          case DeclarantTypeAnswer.AssociatedWithRepresentativeCompany                          =>
            declaration.displayResponseDetail.declarantDetails.contactDetails
        }
        NonUkAddress(
          contactDetails.flatMap(_.addressLine1).getOrElse(""),
          contactDetails.flatMap(_.addressLine2),
          None,
          contactDetails.flatMap(_.addressLine3).getOrElse(""),
          contactDetails.flatMap(_.postalCode).getOrElse(""),
          contactDetails.flatMap(_.countryCode).map(Country(_)).getOrElse(Country.uk)
        )
      }
  }

}
