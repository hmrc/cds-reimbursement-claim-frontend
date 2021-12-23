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
import org.scalamock.handlers.CallHandler3
import org.scalatest.EitherValues
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckYourAnswersAndSubmitController.SubmitClaimResult.SubmitClaimError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.JustSubmittedClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.SubmitClaimFailed
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyBindableGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SubmissionResponseGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CheckYourAnswersAndSubmitControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with EitherValues {

  val mockClaimService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimService)
    )

  val controller: CheckYourAnswersAndSubmitController = instanceOf[CheckYourAnswersAndSubmitController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def mockSubmitClaim(submitClaimRequest: SubmitClaimRequest)(
    response: Either[Error, SubmitClaimResponse]
  ): CallHandler3[SubmitClaimRequest, Lang, HeaderCarrier, EitherT[Future, Error, SubmitClaimResponse]] =
    (mockClaimService
      .submitClaim(_: SubmitClaimRequest, _: Lang)(_: HeaderCarrier))
      .expects(submitClaimRequest, *, *)
      .returning(EitherT.fromEither[Future](response))

  "Check Your Answers And Submit Controller" when {

    "handling requests to check all answers" must {

      "redirect to the start of the journey" when {

        "there is no journey status in the session" in {
          val journey = sample[JourneyBindable]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty)
          }

          checkIsRedirect(
            controller.checkAllAnswers(journey)(FakeRequest()),
            baseRoutes.StartController.start()
          )
        }
      }

      "show the confirmation page" in {
        val justSubmittedClaim = sample[JustSubmittedClaim]

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(justSubmittedClaim))
        }

        checkPageIsDisplayed(
          controller.confirmationOfSubmission(justSubmittedClaim.journey)(FakeRequest()),
          messageFromMessageKey("confirmation-of-submission.title")
        )
      }
    }

    "handling requests to submit a claim" when {

      "the submission is a success" must {

        "show the confirmation page" in {
          val fillingOutClaim = sample[FillingOutClaim]

          val completeClaim = CompleteClaim
            .fromDraftClaim(fillingOutClaim.draftClaim, fillingOutClaim.signedInUserDetails.verifiedEmail)
            .value

          val journeyBindable = JourneyExtractor.extractJourney(fillingOutClaim)

          val submitClaimRequest = SubmitClaimRequest(
            fillingOutClaim.draftClaim.id,
            completeClaim,
            fillingOutClaim.signedInUserDetails
          )

          val submitClaimResponse = sample[SubmitClaimResponse]

          val session = SessionData(fillingOutClaim)

          val justSubmittedJourney = session.copy(journeyStatus =
            Some(
              JustSubmittedClaim(
                fillingOutClaim.ggCredId,
                fillingOutClaim.signedInUserDetails,
                completeClaim,
                submitClaimResponse,
                journeyBindable
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(fillingOutClaim))
            mockSubmitClaim(submitClaimRequest)(Right(submitClaimResponse))
            mockStoreSession(justSubmittedJourney)(Right(()))
          }

          checkIsRedirect(
            controller.checkAllAnswersSubmit(journeyBindable)(FakeRequest()),
            routes.CheckYourAnswersAndSubmitController.confirmationOfSubmission(journeyBindable)
          )
        }
      }

      "the submission is a failure" must {

        "show the submission error page" in {
          val fillingOutClaim = sample[FillingOutClaim]

          val completeClaim = CompleteClaim
            .fromDraftClaim(fillingOutClaim.draftClaim, fillingOutClaim.signedInUserDetails.verifiedEmail)
            .value

          val journeyBindable = JourneyExtractor.extractJourney(fillingOutClaim)

          val submitClaimRequest = SubmitClaimRequest(
            fillingOutClaim.draftClaim.id,
            completeClaim,
            fillingOutClaim.signedInUserDetails
          )

          val submitClaimError = sample[SubmitClaimError]

          val session = SessionData(fillingOutClaim)

          val submissionFailed = session.copy(journeyStatus =
            Some(
              SubmitClaimFailed(
                fillingOutClaim.ggCredId,
                fillingOutClaim.signedInUserDetails,
                journeyBindable
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSubmitClaim(submitClaimRequest)(Left(submitClaimError.error))
            mockStoreSession(submissionFailed)(Right(()))
          }

          checkIsRedirect(
            controller.checkAllAnswersSubmit(journeyBindable)(FakeRequest()),
            routes.CheckYourAnswersAndSubmitController.submissionError(journeyBindable)
          )
        }
      }
    }

    "handling requests with a submission error session" must {

      "redirect to the start of the journey" when {

        "the journey is other than a failed submission" in {
          val fillingOutClaim = sample[FillingOutClaim]
          val journeyBindable = JourneyExtractor.extractJourney(fillingOutClaim)
          val session         = SessionData(fillingOutClaim)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            controller.submissionError(journeyBindable)(FakeRequest()),
            baseRoutes.StartController.start()
          )
        }
      }

      "redirect to the confirmation page" when {

        "the claim has just been submitted" in {
          val fillingOutClaim = sample[FillingOutClaim]

          val completeClaim = CompleteClaim
            .fromDraftClaim(fillingOutClaim.draftClaim, fillingOutClaim.signedInUserDetails.verifiedEmail)
            .value

          val journeyBindable = JourneyExtractor.extractJourney(fillingOutClaim)

          val session = SessionData(fillingOutClaim)

          val submitClaimResponse = sample[SubmitClaimResponse]

          val justSubmittedJourney = session.copy(journeyStatus =
            Some(
              JustSubmittedClaim(
                fillingOutClaim.ggCredId,
                fillingOutClaim.signedInUserDetails,
                completeClaim,
                submitClaimResponse,
                journeyBindable
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(justSubmittedJourney)
          }

          checkIsRedirect(
            controller.submissionError(journeyBindable)(FakeRequest()),
            routes.CheckYourAnswersAndSubmitController.confirmationOfSubmission(journeyBindable)
          )
        }
      }
    }

    "show a technical error page" when {

      "the user has not completely filled in the claim" in {
        val fillingOutClaim = sample[FillingOutClaim]
        val journeyBindable = JourneyExtractor.extractJourney(fillingOutClaim)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(FillingOutClaim.from(fillingOutClaim)(_.copy(commoditiesDetailsAnswer = None))))
        }

        checkIsTechnicalErrorPage(controller.checkAllAnswersSubmit(journeyBindable)(FakeRequest()))

      }

      "the submission was a success but the session could not be updated" in {
        val fillingOutClaim = sample[FillingOutClaim]

        val completeClaim = CompleteClaim
          .fromDraftClaim(fillingOutClaim.draftClaim, fillingOutClaim.signedInUserDetails.verifiedEmail)
          .value

        val journeyBindable = JourneyExtractor.extractJourney(fillingOutClaim)

        val session = SessionData(fillingOutClaim)

        val submitClaimRequest = SubmitClaimRequest(
          fillingOutClaim.draftClaim.id,
          completeClaim,
          fillingOutClaim.signedInUserDetails
        )

        val submitClaimResponse = sample[SubmitClaimResponse]

        val justSubmittedJourney = session.copy(journeyStatus =
          Some(
            JustSubmittedClaim(
              fillingOutClaim.ggCredId,
              fillingOutClaim.signedInUserDetails,
              completeClaim,
              submitClaimResponse,
              journeyBindable
            )
          )
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockSubmitClaim(submitClaimRequest)(Right(submitClaimResponse))
          mockStoreSession(justSubmittedJourney)(Left(Error("BOOM!")))
        }

        checkIsTechnicalErrorPage(controller.checkAllAnswersSubmit(journeyBindable)(FakeRequest()))
      }

    }

    "SubmissionError" should {
      "render the error page when the submission fails" in {
        val submitClaimFailed = sample[SubmitClaimFailed]

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(submitClaimFailed))
        }

        val page = controller.submissionError(submitClaimFailed.journey)(FakeRequest())
        checkPageIsDisplayed(page, messages("submit-claim-error.title"))

      }
    }
  }
}
