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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{AnyContent, MessagesRequest, Request, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.{contentAsString, status, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedRequestWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.{FillingOutClaim, JustSubmittedClaim, NonGovernmentGatewayJourney, SubmitClaimFailed}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SessionDataGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId

import scala.concurrent.Future

class StartControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with RedirectToStartBehaviour
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )
  implicit lazy val messagesApi: MessagesApi           = instanceOf[MessagesApi]
  lazy val controller: StartController                 = instanceOf[StartController]

  "Start controller" when {

    "handling requests to start a journey" when {

      def performAction(rh: Request[AnyContent] = FakeRequest()): Future[Result] = controller.start()(rh)

      "there is a signed in non gg user user and there is no current journey status" must {

        "redirect the user to the `we only support gg page`" in {

          lazy val authenticatedRequest =
            AuthenticatedRequestWithRetrievedData(
              RetrievedUserType.NonGovernmentGatewayRetrievedUser("some auth provider"),
              Some(UserType.NonGovernmentGatewayUser),
              messagesRequest
            )

          lazy val messagesRequest = new MessagesRequest(FakeRequest(), messagesApi)

          val sessionData =
            sample[SessionData].copy(journeyStatus = None, userType = Some(UserType.NonGovernmentGatewayUser))

          inSequence {
            mockAuthWithNonGGUserRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(
              sessionData.copy(
                userType = authenticatedRequest.userType,
                journeyStatus = Some(NonGovernmentGatewayJourney)
              )
            )(Right(()))
          }

          val result = performAction(authenticatedRequest)
          checkIsRedirect(result, routes.StartController.weOnlySupportGG())

        }

        "must return an error if the session cannot be updated" in {
          lazy val authenticatedRequest =
            AuthenticatedRequestWithRetrievedData(
              RetrievedUserType.NonGovernmentGatewayRetrievedUser("some auth provider"),
              Some(UserType.NonGovernmentGatewayUser),
              messagesRequest
            )

          lazy val messagesRequest = new MessagesRequest(FakeRequest(), messagesApi)

          val sessionData =
            sample[SessionData].copy(journeyStatus = None, userType = Some(UserType.NonGovernmentGatewayUser))

          inSequence {
            mockAuthWithNonGGUserRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(
              sessionData.copy(
                userType = authenticatedRequest.userType,
                journeyStatus = Some(NonGovernmentGatewayJourney)
              )
            )(Left(Error("Boom!")))
          }

          val result = performAction(authenticatedRequest)
          checkIsTechnicalErrorPage(result)

        }

      }

      "there is a signed in individual gg user user and there is no current journey status" must {

        "redirect the individual to the start of the journey" in {

          lazy val authenticatedRequest =
            AuthenticatedRequestWithRetrievedData(
              RetrievedUserType.Individual(
                GGCredId("gg-cred-id"),
                None,
                Eori("AB12345678901234Z"),
                Some(Name(Some("John Smith"), Some("Smith")))
              ),
              Some(UserType.Individual),
              messagesRequest
            )

          lazy val messagesRequest = new MessagesRequest(FakeRequest(), messagesApi)

          val sessionData =
            sample[SessionData].copy(journeyStatus = None, userType = Some(UserType.Individual))

          inSequence {
            mockAuthWithEoriEnrolmentRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(
              sessionData.copy(
                userType = authenticatedRequest.userType,
                journeyStatus = Some(
                  FillingOutClaim(
                    GGCredId("gg-cred-id"),
                    SignedInUserDetails(
                      None,
                      Eori("AB12345678901234Z"),
                      Email(""),
                      ContactName("John Smith")
                    ),
                    DraftC285Claim.newDraftC285Claim
                  )
                )
              )
            )(Right(()))
          }

          val result = performAction(authenticatedRequest)
          checkIsRedirect(result, claimRoutes.CheckEoriDetailsController.show())

        }

        "return an error if the session cannot be updated" in {

          lazy val authenticatedRequest =
            AuthenticatedRequestWithRetrievedData(
              RetrievedUserType.Individual(
                GGCredId("gg-cred-id"),
                None,
                Eori("AB12345678901234Z"),
                Some(Name(Some("John Smith"), Some("Smith")))
              ),
              Some(UserType.Individual),
              messagesRequest
            )

          lazy val messagesRequest = new MessagesRequest(FakeRequest(), messagesApi)

          val sessionData =
            sample[SessionData].copy(journeyStatus = None, userType = Some(UserType.Individual))

          inSequence {
            mockAuthWithEoriEnrolmentRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(
              sessionData.copy(
                userType = authenticatedRequest.userType,
                journeyStatus = Some(
                  FillingOutClaim(
                    GGCredId("gg-cred-id"),
                    SignedInUserDetails(
                      None,
                      Eori("AB12345678901234Z"),
                      Email(""),
                      ContactName("John Smith")
                    ),
                    DraftC285Claim.newDraftC285Claim
                  )
                )
              )
            )(Left((Error("boom!"))))
          }

          val result = performAction(authenticatedRequest)
          checkIsTechnicalErrorPage(result)

        }

      }

      "there is a signed in organisation  and there is no current journey status" must {

        "redirect the organisation to the start of the journey" in {

          lazy val authenticatedRequest =
            AuthenticatedRequestWithRetrievedData(
              RetrievedUserType.Organisation(
                GGCredId("gg-cred-id"),
                Eori("AB12345678901234Z"),
                Some(Name(Some("John Smith"), Some("Smith")))
              ),
              Some(UserType.Organisation),
              messagesRequest
            )

          lazy val messagesRequest = new MessagesRequest(FakeRequest(), messagesApi)

          val sessionData =
            sample[SessionData].copy(journeyStatus = None, userType = Some(UserType.Organisation))

          inSequence {
            mockAuthWithOrgWithEoriEnrolmentRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(
              sessionData.copy(
                userType = authenticatedRequest.userType,
                journeyStatus = Some(
                  FillingOutClaim(
                    GGCredId("gg-cred-id"),
                    SignedInUserDetails(
                      None,
                      Eori("AB12345678901234Z"),
                      Email(""),
                      ContactName("John Smith")
                    ),
                    DraftC285Claim.newDraftC285Claim
                  )
                )
              )
            )(Right(()))
          }

          val result = performAction(authenticatedRequest)
          checkIsRedirect(result, claimRoutes.CheckEoriDetailsController.show())

        }

      }

      "there is a claim submission failure journey status" must {

        "redirect the user to the submission error page" in {

          val submitClaimFailed = sample[SubmitClaimFailed]
          val sessionData       = sample[SessionData].copy(journeyStatus = Some(submitClaimFailed))

          inSequence {
            mockAuthWithEoriEnrolmentRetrievals()
            mockGetSession(sessionData)
          }

          val result = performAction()
          checkIsRedirect(result, claims.routes.CheckYourAnswersAndSubmitController.submissionError())

        }

      }

      "there is just submitted claim journey status" must {

        "redirect the user to the main CYA page" in {

          val justSubmittedClaim = sample[JustSubmittedClaim]
          val sessionData        = sample[SessionData].copy(journeyStatus = Some(justSubmittedClaim))

          inSequence {
            mockAuthWithEoriEnrolmentRetrievals()
            mockGetSession(sessionData)
          }

          val result = performAction()
          checkIsRedirect(result, claims.routes.CheckYourAnswersAndSubmitController.confirmationOfSubmission())

        }

      }

      "there is filling out claim journey status" must {

        "redirect the user to the main CYA page" in {

          val fillingOutClaim = sample[FillingOutClaim]
          val sessionData     = sample[SessionData].copy(journeyStatus = Some(fillingOutClaim))

          inSequence {
            mockAuthWithEoriEnrolmentRetrievals()
            mockGetSession(sessionData)
          }

          val result = performAction()
          checkIsRedirect(result, claims.routes.CheckYourAnswersAndSubmitController.checkAllAnswers())

        }

      }

      "there is non government way journey status" must {

        "redirect the user to the `we only support gg page`" in {

          val sessionData = sample[SessionData].copy(journeyStatus = Some(NonGovernmentGatewayJourney))

          inSequence {
            mockAuthWithEoriEnrolmentRetrievals()
            mockGetSession(sessionData)
          }

          val result = performAction()
          checkIsRedirect(result, routes.StartController.weOnlySupportGG())

        }

      }

    }

    "handling requests to start a new claim" when {

      def performAction(rh: Request[AnyContent] = FakeRequest()): Future[Result] = controller.startNewClaim()(rh)

      "the user has just submitted a claim" must {

        "redirect the user to the first page of the journey" in {

          val justSubmittedClaim = sample[JustSubmittedClaim]
          val sessionData        = sample[SessionData].copy(journeyStatus = Some(justSubmittedClaim))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(
              sessionData.copy(
                journeyStatus = Some(
                  FillingOutClaim(
                    justSubmittedClaim.ggCredId,
                    justSubmittedClaim.signedInUserDetails,
                    DraftC285Claim.newDraftC285Claim
                  )
                )
              )
            )(Right(()))
          }

          val result = performAction()
          checkIsRedirect(result, claimRoutes.EnterMovementReferenceNumberController.enterMrn())
        }

        "return an error if session update fails" in {
          val justSubmittedClaim = sample[JustSubmittedClaim]
          val sessionData        = sample[SessionData].copy(journeyStatus = Some(justSubmittedClaim))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(
              sessionData.copy(
                journeyStatus = Some(
                  FillingOutClaim(
                    justSubmittedClaim.ggCredId,
                    justSubmittedClaim.signedInUserDetails,
                    DraftC285Claim.newDraftC285Claim
                  )
                )
              )
            )(Left(Error("boom!")))
          }

          val result = performAction()
          checkIsTechnicalErrorPage(result)
        }

      }

      "the user has not just submitted a claim" must {

        "redirect the user to the error page" in {

          val fillingOutClaim = sample[FillingOutClaim]
          val sessionData     = sample[SessionData].copy(journeyStatus = Some(fillingOutClaim))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          val result = performAction()
          checkIsRedirect(result, routes.StartController.start())
        }

      }

    }

    "handling requests to sign out and sign in" must {

      def performAction(sessionData: Seq[(String, String)]): Future[Result] =
        controller.signOutAndSignIn()(
          FakeRequest().withSession(sessionData: _*)
        )

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(Seq.empty),
        {
          case NonGovernmentGatewayJourney => true
          case _                           => false
        }
      )

      "kill the session and redirect to the gg service" in {
        List(
          NonGovernmentGatewayJourney
        ).foreach { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(journey))
            )
          }

          val result = performAction(Seq("key" -> "value"))
          checkIsRedirect(result, routes.StartController.start())
          session(result).data shouldBe Map.empty
        }

      }

    }

    "handling requests to display the 'we only support gg' page" must {

      def performAction(): Future[Result] =
        controller.weOnlySupportGG()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        {
          case NonGovernmentGatewayJourney => true
          case _                           => false
        }
      )

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty
              .copy(journeyStatus = Some(NonGovernmentGatewayJourney))
          )
        }

        val result = performAction()
        status(result)        shouldBe OK
        contentAsString(result) should include(
          messageFromMessageKey("we-only-support-gg.title")
        )
      }
    }

    "handling requests to sign out and register for GG" must {

      def performAction(sessionData: Seq[(String, String)]): Future[Result] =
        controller.signOutAndRegisterForGG()(
          FakeRequest().withSession(sessionData: _*)
        )

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(Seq.empty),
        {
          case NonGovernmentGatewayJourney => true
          case _                           => false
        }
      )

      "trash the session adn redirect to the gg registration service" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty
              .copy(
                userType = Some(UserType.Individual),
                journeyStatus = Some(NonGovernmentGatewayJourney)
              )
          )
        }

        val result = performAction(Seq("key" -> "value"))
        checkIsRedirect(result, viewConfig.ggCreateAccountUrl)
        session(result).data shouldBe Map.empty
      }

    }

    "handling requests to keep alive" must {

      "return an ok response with an empty body" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Right(None))
        }

        val result = controller.keepAlive()(FakeRequest())
        status(result)          shouldBe OK
        contentAsString(result) shouldBe ""
      }

    }

    "handling requests to display the timed out page" must {

      "display the page" in {
        val result = controller.timedOut()(FakeRequest())
        status(result)        shouldBe OK
        contentAsString(result) should include(
          messageFromMessageKey("timed-out.title")
        )
      }

    }

  }

}
