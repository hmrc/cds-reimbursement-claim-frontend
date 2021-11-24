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
import play.api.data.{Form, FormError}
import play.api.i18n.Messages
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{AddressLookupConfig, ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckContactDetailsMrnController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{JourneyBindable, SessionDataExtractor, SessionUpdates, YesOrNoQuestionForm, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.{No, Yes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupOptions.TimeoutConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{DeclarantTypeAnswer, YesNo}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.{NamePhoneEmail, PhoneNumber}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, Error, MrnContactDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{AddressLookupService, FeatureSwitchService}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
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

  implicit val dataExtractor: DraftClaim => Option[MrnContactDetails] = _.mrnContactDetailsAnswer

  def show(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        if (fillingOutClaim.draftClaim.isMandatoryContactDataAvailable)
          Ok(renderTemplate(whetherContinue, fillingOutClaim, router, mandatoryDataAvailable = true))
        else Redirect(routes.CheckContactDetailsMrnController.addDetailsShow(journey))
      }
    }

  def addDetailsShow(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        Ok(renderTemplate(whetherContinue, fillingOutClaim, router, mandatoryDataAvailable = false))
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
              BadRequest(renderTemplate(updatedForm, fillingOutClaim, router, mandatoryDataAvailable))
            },
            answer => Redirect(router.nextPageForAddClaimantDetails(answer, featureSwitch))
          )
      }
    }

  def submit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[MrnContactDetails] { (fillingOutClaim, _, router) =>
        implicit val routes: ReimbursementRoutes = extractRoutes(fillingOutClaim.draftClaim, journey)
        import routes._

        val mandatoryDataAvailable = fillingOutClaim.draftClaim.isMandatoryContactDataAvailable
        whetherContinue
          .bindFromRequest()
          .fold(
            formWithErrors => {
              val updatedForm = updatedFormErrors(formWithErrors, mandatoryDataAvailable)
              BadRequest(renderTemplate(updatedForm, fillingOutClaim, router, mandatoryDataAvailable))
            },
            {
              case answer@Yes =>
                Redirect(
                  CheckAnswers.when(fillingOutClaim.draftClaim.isComplete)(alternatively =
                    router.nextPageForChangeClaimantDetails(answer, featureSwitch)
                  )
                )
              case answer@No =>
                val updatedClaim = FillingOutClaim.from(fillingOutClaim)(
                  _.copy(
                    mrnContactDetailsAnswer = None,
                    mrnContactAddressAnswer = None)
                )

                EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedClaim))))
                  .leftMap(err => Error(s"Could not remove contact details: ${err.message}"))
                  .fold(
                    e => logAndDisplayError("Submit Declarant Type error: ").apply(e),
                    _ => Redirect(router.nextPageForChangeClaimantDetails(answer, featureSwitch))
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
            copyClaim   = FillingOutClaim.from(claim)(_.copy(mrnContactAddressAnswer = newAddress.some))
            result     <- EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = copyClaim.some)))
          } yield result

        maybeAddressLookupId
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
    router: ReimbursementRoutes,
    mandatoryDataAvailable: Boolean
  )(implicit
    request: RequestWithSessionData[_],
    messages: Messages,
    viewConfig: ViewConfig
  ): HtmlFormat.Appendable =
    claimantDetailsPage(
      form,
      mandatoryDataAvailable,
      extractDetailsRegisteredWithCDS(fillingOutClaim),
      extractEstablishmentAddress(fillingOutClaim),
      fillingOutClaim.draftClaim.mrnContactDetailsAnswer,
      fillingOutClaim.draftClaim.mrnContactAddressAnswer,
      router
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

}

object CheckContactDetailsMrnController {

  val checkContactDetailsKey: String = "claimant-details"

  val whetherContinue: Form[YesNo] = YesOrNoQuestionForm(checkContactDetailsKey)

  def extractDetailsRegisteredWithCDS(fillingOutClaim: FillingOutClaim): NamePhoneEmail = {
    val email = fillingOutClaim.signedInUserDetails.verifiedEmail
    Applicative[Option]
      .map2(fillingOutClaim.draftClaim.displayDeclaration, fillingOutClaim.draftClaim.declarantTypeAnswer) {
        (declaration, declarantType) =>
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

  def extractEstablishmentAddress(fillingOutClaim: FillingOutClaim): Option[EstablishmentAddress] =
    Applicative[Option]
      .map2(fillingOutClaim.draftClaim.displayDeclaration, fillingOutClaim.draftClaim.declarantTypeAnswer) {
        (declaration, declarantType) =>
          declarantType match {
            case DeclarantTypeAnswer.Importer | DeclarantTypeAnswer.AssociatedWithImporterCompany =>
              declaration.displayResponseDetail.consigneeDetails.map(_.establishmentAddress)
            case DeclarantTypeAnswer.AssociatedWithRepresentativeCompany                          =>
              Some(declaration.displayResponseDetail.declarantDetails.establishmentAddress)
          }
      }
      .flatten
}
