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
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.TemporaryJourneyExtractor._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.{ContactAddress, Country}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class EnterContactDetailsEntryNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  cc: MessagesControllerComponents,
  enterYourContactDetailsPage: pages.enter_your_contact_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[ContactDetailsAnswer] =
    _.entryNumberContactDetailsAnswer

  implicit protected val journeyBindable = JourneyBindable.Single

  def enterContactDetails(): Action[AnyContent]  = show(false)
  def changeContactDetails(): Action[AnyContent] = show(true)

  def show(isAmend: Boolean): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[ContactDetailsAnswer] { (_, answers, router) =>
        val emptyForm  = EnterContactDetailsEntryNumberController.contactDetailsForm
        val filledForm = answers.fold(emptyForm)(emptyForm.fill(_))
        Ok(enterYourContactDetailsPage(filledForm, isAmend, router))
      }
    }

  def enterContactDetailsSubmit(): Action[AnyContent]  = submit(false)
  def changeContactDetailsSubmit(): Action[AnyContent] = submit(true)

  def submit(isAmend: Boolean): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[ContactDetailsAnswer] { (fillingOutClaim, _, router) =>
        EnterContactDetailsEntryNumberController.contactDetailsForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(enterYourContactDetailsPage(formWithErrors, isAmend, router)),
            formOk => {
              val updatedJourney =
                FillingOutClaim.of(fillingOutClaim)(_.copy(entryNumberContactDetailsAnswer = Option(formOk)))
              EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap(_ => Error("Could not save Entry Number Contact Details"))
                .fold(
                  logAndDisplayError("Submit Entry Number Contact Details: "),
                  _ => Redirect(router.nextPageForEntryNumberContactDetails(fillingOutClaim.draftClaim.declarantType))
                )
            }
          )
      }

    }
}

object EnterContactDetailsEntryNumberController {

  val contactDetailsForm: Form[ContactDetailsAnswer] = Form(
    mapping(
      "enter-your-contact-details.contact-name"         -> nonEmptyText(maxLength = 512),
      "enter-your-contact-details.contact-email"        -> Email.mappingMaxLength,
      "enter-your-contact-details.contact-phone-number" -> PhoneNumber.mapping,
      ""                                                -> ContactAddress.addressFormMapping
    )(ContactDetailsAnswer.apply)(ContactDetailsAnswer.unapply)
  )

  def toContactDetailsFormData(
    contactDetails: Option[ContactDetails],
    verifiedEmail: Email
  ): ContactDetailsAnswer =
    ContactDetailsAnswer(
      contactDetails.flatMap(_.contactName).getOrElse(""),
      verifiedEmail,
      PhoneNumber(contactDetails.flatMap(_.telephone).getOrElse("")),
      ContactAddress(
        contactDetails.flatMap(_.addressLine1).getOrElse(""),
        contactDetails.flatMap(_.addressLine2),
        None,
        contactDetails.flatMap(_.addressLine3).getOrElse(""),
        contactDetails.flatMap(_.postalCode).getOrElse(""),
        contactDetails.flatMap(_.countryCode).map(Country(_)).getOrElse(Country.uk)
      )
    )

}
