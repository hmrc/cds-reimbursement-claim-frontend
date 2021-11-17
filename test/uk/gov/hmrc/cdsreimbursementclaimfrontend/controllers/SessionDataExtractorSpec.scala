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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import play.api.mvc.{AnyContent, MessagesRequest}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{TypeOfClaimAnswer, YesNo}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SessionDataGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, SessionData}

import scala.concurrent.Future

class SessionDataExtractorSpec extends AnyWordSpec with Matchers {

  def fakeRequest2MessageRequest[A](fakeRequest: FakeRequest[A]): MessagesRequest[A] =
    new MessagesRequest[A](fakeRequest, play.api.test.Helpers.stubMessagesApi())

  class SessionTester() extends SessionDataExtractor {
    def method(implicit
      extractor: DraftClaim => Option[YesNo],
      request: RequestWithSessionData[_]
    ) =
      withAnswers[YesNo] { (_, data) =>
        Future.successful(Ok(Json.toJson(data)))
      }
  }

  class SessionAndRouterTester() extends SessionDataExtractor {
    def method(expectedData: Option[YesNo], expecterRouter: ReimbursementRoutes)(implicit
      extractor: DraftClaim => Option[YesNo],
      request: RequestWithSessionData[_],
      journeyBindable: JourneyBindable
    ) =
      withAnswersAndRoutes[YesNo] { (_, data, router) =>
        data   shouldBe expectedData
        router shouldBe expecterRouter
        Future.successful(Ok(""))
      }
  }

  "withAnswers" should {
    "extract the data successfully" in {
      val sessionTester                              = new SessionTester()
      val dataExtractor: DraftClaim => Option[YesNo] = _.whetherNorthernIrelandAnswer

      val msgReq               = fakeRequest2MessageRequest(FakeRequest())
      val authenticatedRequest = AuthenticatedRequest[AnyContent](msgReq)
      val draftC285Claim       =
        sample[DraftClaim].copy(whetherNorthernIrelandAnswer = Some(Yes))
      val foc                  = sample[FillingOutClaim].copy(draftClaim = draftC285Claim)
      val sessionData          = sample[SessionData].copy(journeyStatus = Some(foc))
      val request              = RequestWithSessionData(Some(sessionData), authenticatedRequest)

      val result = sessionTester.method(dataExtractor, request)
      status(result)        shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(
        draftC285Claim.whetherNorthernIrelandAnswer.getOrElse(fail())
      )
    }

    "redirect to the start page if the session is empty" in {
      val sessionTester                              = new SessionTester()
      val dataExtractor: DraftClaim => Option[YesNo] = _.whetherNorthernIrelandAnswer

      val msgReq               = fakeRequest2MessageRequest(FakeRequest())
      val authenticatedRequest = AuthenticatedRequest[AnyContent](msgReq)
      val request              = RequestWithSessionData(None, authenticatedRequest)

      val result = sessionTester.method(dataExtractor, request)
      status(result) shouldBe 303
    }
  }

  "withAnswersAndRoutes" should {
    "extract the data and the router" when {
      "NumberOfClaims and MRN answers were provided" in {
        val sessionTester                              = new SessionAndRouterTester()
        val dataExtractor: DraftClaim => Option[YesNo] = _.whetherNorthernIrelandAnswer

        val expectedData         = Some(Yes)
        val msgReq               = fakeRequest2MessageRequest(FakeRequest())
        val authenticatedRequest = AuthenticatedRequest[AnyContent](msgReq)
        val draftC285Claim       =
          sample[DraftClaim].copy(
            typeOfClaim = Some(TypeOfClaimAnswer.Multiple),
            movementReferenceNumber = Some(sample[MRN]),
            whetherNorthernIrelandAnswer = expectedData
          )
        val foc                  = sample[FillingOutClaim].copy(draftClaim = draftC285Claim)
        val sessionData          = sample[SessionData].copy(journeyStatus = Some(foc))
        val request              = RequestWithSessionData(Some(sessionData), authenticatedRequest)

        val result =
          sessionTester.method(expectedData, MRNMultipleRoutes)(dataExtractor, request, JourneyBindable.Multiple)
        status(result) shouldBe 200
      }

      "NumberOfClaims and MRN answers were provided, but the JourneyBindable/URL was tampered with" in {
        val sessionTester                              = new SessionAndRouterTester()
        val dataExtractor: DraftClaim => Option[YesNo] = _.whetherNorthernIrelandAnswer

        val expectedData         = Some(Yes)
        val msgReq               = fakeRequest2MessageRequest(FakeRequest())
        val authenticatedRequest = AuthenticatedRequest[AnyContent](msgReq)
        val draftC285Claim       =
          sample[DraftClaim].copy(
            typeOfClaim = Some(TypeOfClaimAnswer.Multiple),
            movementReferenceNumber = Some(sample[MRN]),
            whetherNorthernIrelandAnswer = expectedData
          )
        val foc                  = sample[FillingOutClaim].copy(draftClaim = draftC285Claim)
        val sessionData          = sample[SessionData].copy(journeyStatus = Some(foc))
        val request              = RequestWithSessionData(Some(sessionData), authenticatedRequest)

        val result =
          sessionTester.method(expectedData, JourneyNotDetectedRoutes)(dataExtractor, request, JourneyBindable.Single)
        status(result) shouldBe 200
      }

      "Only the MRN answer was provided" in {
        val sessionTester                              = new SessionAndRouterTester()
        val dataExtractor: DraftClaim => Option[YesNo] = _.whetherNorthernIrelandAnswer

        val expectedData         = Some(Yes)
        val msgReq               = fakeRequest2MessageRequest(FakeRequest())
        val authenticatedRequest = AuthenticatedRequest[AnyContent](msgReq)
        val draftC285Claim       =
          sample[DraftClaim].copy(
            typeOfClaim = None,
            movementReferenceNumber = Some(sample[MRN]),
            whetherNorthernIrelandAnswer = expectedData
          )
        val foc                  = sample[FillingOutClaim].copy(draftClaim = draftC285Claim)
        val sessionData          = sample[SessionData].copy(journeyStatus = Some(foc))
        val request              = RequestWithSessionData(Some(sessionData), authenticatedRequest)

        val result =
          sessionTester.method(expectedData, MRNSingleRoutes)(dataExtractor, request, JourneyBindable.Single)
        status(result) shouldBe 200
      }

      "No previous answers were prodvided" in {
        val sessionTester                              = new SessionAndRouterTester()
        val dataExtractor: DraftClaim => Option[YesNo] = _.whetherNorthernIrelandAnswer

        val expectedData         = Some(Yes)
        val msgReq               = fakeRequest2MessageRequest(FakeRequest())
        val authenticatedRequest = AuthenticatedRequest[AnyContent](msgReq)
        val draftC285Claim       =
          sample[DraftClaim].copy(
            typeOfClaim = None,
            movementReferenceNumber = None,
            whetherNorthernIrelandAnswer = expectedData
          )
        val foc                  = sample[FillingOutClaim].copy(draftClaim = draftC285Claim)
        val sessionData          = sample[SessionData].copy(journeyStatus = Some(foc))
        val request              = RequestWithSessionData(Some(sessionData), authenticatedRequest)

        val result =
          sessionTester.method(expectedData, JourneyNotDetectedRoutes)(dataExtractor, request, JourneyBindable.Multiple)
        status(result) shouldBe 200
      }
    }
  }
}
