/*
 * Copyright 2022 HM Revenue & Customs
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
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.nonEmptyText
import play.api.data.Forms.optional
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class EnterContactDetailsMrnController @Inject() (
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

  implicit val dataExtractor: DraftClaim => Option[MrnContactDetails] = _.mrnContactDetailsAnswer

  def enterMrnContactDetails(implicit journey: JourneyBindable): Action[AnyContent]  = show(isChange = false)
  def changeMrnContactDetails(implicit journey: JourneyBindable): Action[AnyContent] = show(isChange = true)

  def show(isChange: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        val answers               =
          (fillingOutClaim.draftClaim.mrnContactDetailsAnswer, fillingOutClaim.draftClaim.mrnContactAddressAnswer)
            .mapN { (contactDetails, _) =>
              contactDetails
            }
        val mrnContactDetailsForm =
          answers.foldLeft(EnterContactDetailsMrnController.mrnContactDetailsForm)((form, answer) => form.fill(answer))

        val postAction =
          if (isChange) router.submitUrlForChangeMrnContactDetails() else router.submitUrlForEnterMrnContactDetails()
        Ok(enterOrChangeContactDetailsPage(mrnContactDetailsForm, postAction, isChange))
      }
    }

  def enterMrnContactDetailsSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    submit(isChange = false)

  def changeMrnContactDetailsSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    submit(isChange = true)

  def submit(isChange: Boolean)(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        EnterContactDetailsMrnController.mrnContactDetailsForm
          .bindFromRequest()
          .fold(
            formWithErrors => {
              val postAction =
                if (isChange) router.submitUrlForChangeMrnContactDetails()
                else router.submitUrlForEnterMrnContactDetails()
              BadRequest(enterOrChangeContactDetailsPage(formWithErrors, postAction, isChange))
            },
            formOk => {

              val updatedClaim =
                FillingOutClaim.from(fillingOutClaim)(_.copy(mrnContactDetailsAnswer = Some(formOk)))

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedClaim))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                logAndDisplayError("could not capture contact details"),
                _ => Redirect(router.nextPageForMrnContactDetails(isChange))
              )
            }
          )
      }
    }
}

object EnterContactDetailsMrnController {

  val mrnContactDetailsForm: Form[MrnContactDetails] = Form(
    mapping(
      "enter-contact-details.contact-name"         -> nonEmptyText(maxLength = 512),
      "enter-contact-details.contact-email"        -> Email.mappingMaxLength,
      "enter-contact-details.contact-phone-number" -> optional(PhoneNumber.mapping)
    )(MrnContactDetails.apply)(MrnContactDetails.unapply)
  )

}
