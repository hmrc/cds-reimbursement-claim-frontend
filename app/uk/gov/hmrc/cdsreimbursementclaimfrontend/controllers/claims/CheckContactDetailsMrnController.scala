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
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
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

  def startAddressLookup(implicit journey: JourneyBindable): Call =
    routes.CheckContactDetailsMrnController.redirectToALF(journey)

  def show(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        val maybeContactDetails  = fillingOutClaim.draftClaim.computeContactDetails(fillingOutClaim.signedInUserDetails)
        val maybeAddressDetails  = fillingOutClaim.draftClaim.computeAddressDetails(fillingOutClaim.signedInUserDetails)
        val changeContactDetails = routes.EnterContactDetailsMrnController.changeMrnContactDetails(journey)
        val postAction           = routes.CheckContactDetailsMrnController.submit(journey)

        (maybeContactDetails, maybeAddressDetails) match {
          case (Some(contactDetails), Some(contactAddress)) =>
            val view =
              claimantDetailsPage(contactDetails, contactAddress, changeContactDetails, startAddressLookup, postAction)
            Ok(view)

          case _ =>
            Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(journey))
        }
      }
    }

  def submit(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        val sessionUpdateResult: Future[Either[Error, Unit]] =
          if (
            fillingOutClaim.draftClaim.mrnContactDetailsAnswer.isDefined &&
            fillingOutClaim.draftClaim.mrnContactAddressAnswer.isDefined
          )
            Future.successful(Right(()))
          else {
            val updatedClaim: FillingOutClaim = fillingOutClaim.copy(draftClaim =
              fillingOutClaim.draftClaim.copy(
                mrnContactDetailsAnswer = fillingOutClaim.draftClaim.mrnContactDetailsAnswer
                  .orElse(
                    fillingOutClaim.draftClaim.computeContactDetails(fillingOutClaim.signedInUserDetails)
                  ),
                mrnContactAddressAnswer = fillingOutClaim.draftClaim.mrnContactAddressAnswer
                  .orElse(
                    fillingOutClaim.draftClaim.computeAddressDetails(fillingOutClaim.signedInUserDetails)
                  )
              )
            )
            updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedClaim)))
          }

        EitherT(sessionUpdateResult)
          .leftMap(_ => Error("could not update session"))
          .fold(
            logAndDisplayError("could not store contact and address details"),
            _ => {
              val router = extractRoutes(fillingOutClaim.draftClaim, journey)
              Redirect(
                router.CheckAnswers.when(fillingOutClaim.draftClaim.isComplete)(alternatively =
                  if (featureSwitch.isEnabled(Feature.NorthernIreland))
                    routes.ClaimNorthernIrelandController.selectWhetherNorthernIrelandClaim(journey)
                  else routes.SelectBasisForClaimController.selectBasisForClaim(journey)
                )
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

}

object CheckContactDetailsMrnController {

  val checkContactDetailsKey: String = "claimant-details"

  val whetherContinue: Form[YesNo] = YesOrNoQuestionForm(checkContactDetailsKey)
}
