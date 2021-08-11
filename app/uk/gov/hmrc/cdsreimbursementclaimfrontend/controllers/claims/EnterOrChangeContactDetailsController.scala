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
class EnterOrChangeContactDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  cc: MessagesControllerComponents,
  enterOrChangeContactDetailsPage: pages.enter_or_change_contact_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[MrnContactDetails] = _.mrnContactDetailsAnswer

  def enterMrnContactDetails(implicit journey: JourneyBindable): Action[AnyContent]  = show(isChange = false)
  def changeMrnContactDetails(implicit journey: JourneyBindable): Action[AnyContent] = show(isChange = true)

  def show(isChange: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (_, answers, router) =>
        val mrnContactDetailsForm =
          answers.toList.foldLeft(EnterOrChangeContactDetailsController.mrnContactDetailsForm)((form, answer) =>
            form.fill(answer)
          )
        Ok(enterOrChangeContactDetailsPage(mrnContactDetailsForm, router, isChange))
      }
    }

  def enterMrnContactDetailsSubmit(implicit journey: JourneyBindable): Action[AnyContent]  = submit(isChange = false)
  def changeMrnContactDetailsSubmit(implicit journey: JourneyBindable): Action[AnyContent] = submit(isChange = true)

  def submit(isChange: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        EnterOrChangeContactDetailsController.mrnContactDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterOrChangeContactDetailsPage(requestFormWithErrors, router, isChange)
              ),
            contactDetailsFormData => {

              val updatedJourney =
                FillingOutClaim.of(fillingOutClaim)(_.copy(mrnContactDetailsAnswer = Some(contactDetailsFormData)))

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                logAndDisplayError("could not capture contact details"),
                _ => Redirect(router.nextPageForEnterOrChangeMrnContactDetails(isChange))
              )
            }
          )
      }
    }
}

object EnterOrChangeContactDetailsController {

  val mrnContactDetailsForm: Form[MrnContactDetails] = Form(
    mapping(
      "enter-or-change-contact-details.contact-name"         -> nonEmptyText(maxLength = 512),
      "enter-or-change-contact-details.contact-email"        -> Email.mappingMaxLength,
      "enter-or-change-contact-details.contact-phone-number" -> PhoneNumber.mapping
    )(MrnContactDetails.apply)(MrnContactDetails.unapply)
  )

  def toContactFormData(
    mrnContactDetails: Option[ContactDetails],
    verifiedEmail: Email
  ): MrnContactDetails =
    MrnContactDetails(
      mrnContactDetails.flatMap(_.contactName).getOrElse(""),
      verifiedEmail,
      PhoneNumber(mrnContactDetails.flatMap(_.telephone).getOrElse(""))
    )

}
