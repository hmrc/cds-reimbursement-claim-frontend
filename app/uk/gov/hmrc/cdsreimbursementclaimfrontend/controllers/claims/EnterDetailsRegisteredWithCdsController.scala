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
import cats.implicits.catsSyntaxEq
import cats.syntax.either._
import cats.syntax.option._
import com.google.inject.{Inject, Singleton}
import julienrf.json.derived
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText, of}
import play.api.libs.json.OFormat
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.TemporaryJourneyExtractor._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDetailsRegisteredWithCdsController.{DetailsRegisteredWithCdsFormData, consigneeToClaimantDetails, declarantToClaimantDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, TemporaryJourneyExtractor}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DetailsRegisteredWithCdsAnswer.{CompleteDetailsRegisteredWithCdsAnswer, IncompleteDetailsRegisteredWithCdsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.{ContactAddress, Country}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterDetailsRegisteredWithCdsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  cc: MessagesControllerComponents,
  detailsRegisteredWithCdsPage: pages.enter_claimant_details_as_registered_with_cds
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {
}

object EnterDetailsRegisteredWithCdsController {

  final case class DetailsRegisteredWithCdsFormData(
    fullName: String,
    emailAddress: Email,
    contactAddress: ContactAddress,
    addCompanyDetails: Boolean
  )

  object DetailsRegisteredWithCdsFormData {
    implicit val format: OFormat[DetailsRegisteredWithCdsFormData] =
      derived.oformat[DetailsRegisteredWithCdsFormData]()
  }

  val detailsRegisteredWithCdsForm: Form[DetailsRegisteredWithCdsFormData] = Form(
    mapping(
      "enter-claimant-details-as-registered-with-cds.individual-full-name" -> nonEmptyText(maxLength = 512),
      "enter-claimant-details-as-registered-with-cds.individual-email"     -> Email.mappingMaxLength,
      ""                                                                   -> ContactAddress.addressFormMapping,
      "enter-claimant-details-as-registered-with-cds.add-company-details"  -> of(BooleanFormatter.formatter)
    )(DetailsRegisteredWithCdsFormData.apply)(DetailsRegisteredWithCdsFormData.unapply)
  )

  def consigneeToClaimantDetails(
    displayDeclaration: DisplayDeclaration,
    verifiedEmail: Email
  ): DetailsRegisteredWithCdsFormData = {
    val declaration          = displayDeclaration.displayResponseDetail
    val establishmentAddress = declaration.consigneeDetails.map(p => p.establishmentAddress)
    DetailsRegisteredWithCdsFormData(
      declaration.consigneeDetails.map(_.legalName).getOrElse(""),
      verifiedEmail,
      ContactAddress(
        establishmentAddress.map(_.addressLine1).getOrElse(""),
        establishmentAddress.flatMap(_.addressLine2),
        None,
        establishmentAddress.flatMap(_.addressLine3).getOrElse(""),
        establishmentAddress.flatMap(_.postalCode).getOrElse(""),
        establishmentAddress.map(cc => Country(cc.countryCode)).getOrElse(Country.uk)
      ),
      addCompanyDetails = false
    )
  }

  def declarantToClaimantDetails(
    displayDeclaration: DisplayDeclaration,
    verifiedEmail: Email
  ): DetailsRegisteredWithCdsFormData = {
    val declaration          = displayDeclaration.displayResponseDetail
    val establishmentAddress = declaration.declarantDetails.establishmentAddress
    DetailsRegisteredWithCdsFormData(
      declaration.declarantDetails.legalName,
      verifiedEmail,
      ContactAddress(
        establishmentAddress.addressLine1,
        establishmentAddress.addressLine2,
        None,
        establishmentAddress.addressLine3.getOrElse(""),
        establishmentAddress.postalCode.getOrElse(""),
        Country(establishmentAddress.countryCode)
      ),
      addCompanyDetails = false
    )
  }

}
