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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.JustSubmittedClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.SubmitClaimFailed
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.C285ClaimRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SubmissionResponseGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimResult

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

  def mockSubmitClaim(submitClaimRequest: C285ClaimRequest)(
    response: Either[Error, SubmitClaimResponse]
  ): CallHandler3[C285ClaimRequest, Lang, HeaderCarrier, EitherT[Future, Error, SubmitClaimResponse]] =
    (mockClaimService
      .submitClaim(_: C285ClaimRequest, _: Lang)(_: HeaderCarrier))
      .expects(submitClaimRequest, *, *)
      .returning(EitherT.fromEither[Future](response))

  "Check Your Answers And Submit Controller" when {

    "handling requests to check all answers" must {

      "redirect to the start of the journey" when {

        "there is no journey status in the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty)
          }

          checkIsRedirect(
            controller.checkAllAnswers(FakeRequest()),
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
          controller.confirmationOfSubmission(FakeRequest()),
          messageFromMessageKey("confirmation-of-submission.title")
        )
      }
    }

    "handling requests to submit a claim" when {

      "the submission is a success" must {

        "show the confirmation page" in {
          val fillingOutClaim = sample[FillingOutClaim]

          val claim = C285Claim
            .fromDraftClaim(
              fillingOutClaim.draftClaim,
              fillingOutClaim.signedInUserDetails.verifiedEmail,
              fillingOutClaim.signedInUserDetails.eori
            )
            .value

          val submitClaimRequest = C285ClaimRequest(
            fillingOutClaim.draftClaim.id,
            claim,
            fillingOutClaim.signedInUserDetails
          )

          val submitClaimResponse = sample[SubmitClaimResponse]

          val session = SessionData(fillingOutClaim)

          val justSubmittedJourney = session.copy(journeyStatus =
            Some(
              JustSubmittedClaim(
                fillingOutClaim.ggCredId,
                fillingOutClaim.signedInUserDetails,
                claim,
                submitClaimResponse
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
            controller.checkAllAnswersSubmit(FakeRequest()),
            routes.CheckYourAnswersAndSubmitController.confirmationOfSubmission
          )
        }
      }

      "the submission is a failure" must {

        "show the submission error page" in {
          val fillingOutClaim = sample[FillingOutClaim]

          val claim = C285Claim
            .fromDraftClaim(
              fillingOutClaim.draftClaim,
              fillingOutClaim.signedInUserDetails.verifiedEmail,
              fillingOutClaim.signedInUserDetails.eori
            )
            .value

          val submitClaimRequest = C285ClaimRequest(
            fillingOutClaim.draftClaim.id,
            claim,
            fillingOutClaim.signedInUserDetails
          )

          val submitClaimError = sample[SubmitClaimResult.SubmitClaimError]

          val session = SessionData(fillingOutClaim)

          val submissionFailed = session.copy(journeyStatus =
            Some(
              SubmitClaimFailed(
                fillingOutClaim.ggCredId,
                fillingOutClaim.signedInUserDetails
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
            controller.checkAllAnswersSubmit(FakeRequest()),
            routes.CheckYourAnswersAndSubmitController.submissionError
          )
        }
      }
    }

    "handling requests with a submission error session" must {

      "redirect to the start of the journey" when {

        "the journey is other than a failed submission" in {
          val fillingOutClaim = sample[FillingOutClaim]
          val session         = SessionData(fillingOutClaim)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            controller.submissionError(FakeRequest()),
            baseRoutes.StartController.start()
          )
        }
      }

      "redirect to the confirmation page" when {

        "the claim has just been submitted" in {
          val fillingOutClaim = sample[FillingOutClaim]

          val claim = C285Claim
            .fromDraftClaim(
              fillingOutClaim.draftClaim,
              fillingOutClaim.signedInUserDetails.verifiedEmail,
              fillingOutClaim.signedInUserDetails.eori
            )
            .value

          val session = SessionData(fillingOutClaim)

          val submitClaimResponse = sample[SubmitClaimResponse]

          val justSubmittedJourney = session.copy(journeyStatus =
            Some(
              JustSubmittedClaim(
                fillingOutClaim.ggCredId,
                fillingOutClaim.signedInUserDetails,
                claim,
                submitClaimResponse
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(justSubmittedJourney)
          }

          checkIsRedirect(
            controller.submissionError(FakeRequest()),
            routes.CheckYourAnswersAndSubmitController.confirmationOfSubmission
          )
        }
      }
    }

    "show a technical error page" when {

      "the user has not completely filled in the claim" in {
        val fillingOutClaim = sample[FillingOutClaim]

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(FillingOutClaim.from(fillingOutClaim)(_.copy(additionalDetailsAnswer = None))))
        }

        checkIsTechnicalErrorPage(controller.checkAllAnswersSubmit(FakeRequest()))

      }

      "the submission was a success but the session could not be updated" in {
        val fillingOutClaim = sample[FillingOutClaim]

        val claim = C285Claim
          .fromDraftClaim(
            fillingOutClaim.draftClaim,
            fillingOutClaim.signedInUserDetails.verifiedEmail,
            fillingOutClaim.signedInUserDetails.eori
          )
          .value

        val session = SessionData(fillingOutClaim)

        val submitClaimRequest = C285ClaimRequest(
          fillingOutClaim.draftClaim.id,
          claim,
          fillingOutClaim.signedInUserDetails
        )

        val submitClaimResponse = sample[SubmitClaimResponse]

        val justSubmittedJourney = session.copy(journeyStatus =
          Some(
            JustSubmittedClaim(
              fillingOutClaim.ggCredId,
              fillingOutClaim.signedInUserDetails,
              claim,
              submitClaimResponse
            )
          )
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockSubmitClaim(submitClaimRequest)(Right(submitClaimResponse))
          mockStoreSession(justSubmittedJourney)(Left(Error("BOOM!")))
        }

        checkIsTechnicalErrorPage(controller.checkAllAnswersSubmit(FakeRequest()))
      }

    }

    "SubmissionError" should {
      "render the error page when the submission fails" in {
        val submitClaimFailed = sample[SubmitClaimFailed]

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(submitClaimFailed))
        }

        val page = controller.submissionError(FakeRequest())
        checkPageIsDisplayed(page, messages("submit-claim-error.title"))

      }
    }
  }
}