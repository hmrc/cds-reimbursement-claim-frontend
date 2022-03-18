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
import cats.implicits.catsSyntaxOptionId
import cats.syntax.all._
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.data.FormError
import play.api.i18n.Messages
import play.api.mvc._
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckContactDetailsMrnController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class CheckContactDetailsMrnController @Inject() (
  addressLookupService: AddressLookupService,
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

  implicit val dataExtractor: DraftClaim => Option[MrnContactDetails] = _.mrnContactDetailsAnswer

  def show(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        if (fillingOutClaim.draftClaim.isMandatoryContactDataAvailable) {
          Ok(
            renderTemplate(
              whetherContinue,
              fillingOutClaim,
              router.submitPageForClaimantDetails(true),
              mandatoryDataAvailable = true,
              journey
            )
          )
        } else Redirect(routes.CheckContactDetailsMrnController.addDetailsShow(journey))
      }
    }

  def addDetailsShow(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        Ok(
          renderTemplate(
            whetherContinue,
            fillingOutClaim,
            router.submitPageForClaimantDetails(false),
            mandatoryDataAvailable = false,
            journey
          )
        )
      }
    }

  def addDetailsSubmit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        val mandatoryDataAvailable = fillingOutClaim.draftClaim.isMandatoryContactDataAvailable
        whetherContinue
          .bindFromRequest()
          .fold(
            formWithErrors => {
              val updatedForm = updatedFormErrors(formWithErrors, mandatoryDataAvailable)
              BadRequest(
                renderTemplate(
                  updatedForm,
                  fillingOutClaim,
                  router.submitPageForClaimantDetails(mandatoryDataAvailable),
                  mandatoryDataAvailable,
                  journey
                )
              )
            },
            {
              case Yes => Redirect(routes.EnterContactDetailsMrnController.enterMrnContactDetails(journey))
              case No  => nextPageForClaimantDetails(fillingOutClaim, router, featureSwitch)
            }
          )
      }
    }

  def submit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        val mandatoryDataAvailable = fillingOutClaim.draftClaim.isMandatoryContactDataAvailable
        whetherContinue
          .bindFromRequest()
          .fold(
            formWithErrors => {
              val updatedForm = updatedFormErrors(formWithErrors, mandatoryDataAvailable)
              BadRequest(
                renderTemplate(
                  updatedForm,
                  fillingOutClaim,
                  router.submitPageForClaimantDetails(mandatoryDataAvailable),
                  mandatoryDataAvailable,
                  journey
                )
              )
            },
            {
              case Yes => nextPageForClaimantDetails(fillingOutClaim, router, featureSwitch)
              case No  =>
                val updatedClaim = FillingOutClaim.from(fillingOutClaim)(
                  _.copy(mrnContactDetailsAnswer = None, mrnContactAddressAnswer = None)
                )

                EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedClaim))))
                  .leftMap(err => Error(s"Could not remove contact details: ${err.message}"))
                  .fold(
                    e => logAndDisplayError("Submit Declarant Type error: ").apply(e),
                    _ => Redirect(routes.CheckContactDetailsMrnController.addDetailsShow(journey))
                  )
            }
          )
      }
    }

  def redirectToALF(implicit journey: JourneyBindable): Action[AnyContent] =
    Action.andThen(authenticatedAction).async { implicit request =>
      addressLookupService
        .startLookupRedirectingBackTo(routes.CheckContactDetailsMrnController.retrieveAddressFromALF(journey))
        .fold(logAndDisplayError("Error occurred starting address lookup: "), url => Redirect(url.toString))
    }

  def retrieveAddressFromALF(journey: JourneyBindable, addressIdentity: Option[UUID] = None): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (claim, _, _) =>
        def updateLookupAddress(id: UUID) =
          for {
            newAddress <- addressLookupService.retrieveUserAddress(id)
            copyClaim   = FillingOutClaim.from(claim)(_.copy(mrnContactAddressAnswer = newAddress.some))
            result     <- EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = copyClaim.some)))
          } yield result

        addressIdentity
          .map(updateLookupAddress)
          .getOrElse(EitherT.rightT[Future, Error](()))
          .fold(
            {
              case e @ Error(message, _, _)
                  if message.contains("/address/postcode: error.path.missing") ||
                    message.contains("/address/lines: error.minLength") =>
                logger warn s"Error updating Address Lookup address: $e"
                Redirect(routes.ProblemWithAddressController.problem(journey))
              case e: Error =>
                logAndDisplayError("Error updating Address Lookup address: ")(errorHandler, request)(e)
            },
            _ => Redirect(routes.CheckContactDetailsMrnController.show(journey))
          )
      }(dataExtractor, request, journey)
    }

  def renderTemplate(
    form: Form[YesNo],
    fillingOutClaim: FillingOutClaim,
    postAction: Call,
    mandatoryDataAvailable: Boolean,
    journey: JourneyBindable
  )(implicit
    request: RequestWithSessionData[_],
    messages: Messages,
    viewConfig: ViewConfig
  ): HtmlFormat.Appendable =
    claimantDetailsPage(
      form,
      mandatoryDataAvailable,
      fillingOutClaim.draftClaim.extractDetailsRegisteredWithCDS(fillingOutClaim.signedInUserDetails.verifiedEmail),
      fillingOutClaim.draftClaim.extractEstablishmentAddress,
      fillingOutClaim.draftClaim.mrnContactDetailsAnswer,
      fillingOutClaim.draftClaim.mrnContactAddressAnswer,
      postAction,
      journey
    )

  def updatedFormErrors[T](formWithErrors: Form[T], mandatoryDataAvailable: Boolean): Form[T] =
    if (mandatoryDataAvailable)
      replaceFormError("error.required", "error.required.change", formWithErrors)
    else replaceFormError("error.required", "error.required.add", formWithErrors)

  def replaceFormError[T](originalError: String, replaceTo: String, formWithErrors: Form[T]): Form[T] =
    formWithErrors.copy(errors = formWithErrors.errors.map { fe =>
      val newMsgs = fe.messages.map(msg => if (msg === originalError) replaceTo else msg)
      FormError(fe.key, newMsgs)
    })

  private def nextPageForClaimantDetails(
    fillingOutClaim: FillingOutClaim,
    router: ReimbursementRoutes,
    featureSwitch: FeatureSwitchService
  )(implicit journey: JourneyBindable): Result =
    Redirect(
      router.CheckAnswers.when(fillingOutClaim.draftClaim.isComplete)(alternatively =
        if (featureSwitch.isEnabled(Feature.NorthernIreland))
          routes.ClaimNorthernIrelandController.selectWhetherNorthernIrelandClaim(journey)
        else routes.SelectBasisForClaimController.selectBasisForClaim(journey)
      )
    )
}

object CheckContactDetailsMrnController {

  val checkContactDetailsKey: String = "claimant-details"

  val whetherContinue: Form[YesNo] = YesOrNoQuestionForm(checkContactDetailsKey)
}
