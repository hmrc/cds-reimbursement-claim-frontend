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
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec
import play.api.mvc.{AnyContent, MessagesRequest}
import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectNumberOfClaimsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, SessionData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SessionDataGen._

class JourneyExtractorSpec extends AnyWordSpec with Matchers {

  def fakeRequest2MessageRequest[A](fakeRequest: FakeRequest[A]): MessagesRequest[A] =
    new MessagesRequest[A](fakeRequest, play.api.test.Helpers.stubMessagesApi())

  "TemporaryJourneyExtractor" should {

    "Return the default JourneyBindable" in {
      val msgReq               = fakeRequest2MessageRequest(FakeRequest())
      val authenticatedRequest = AuthenticatedRequest[AnyContent](msgReq)
      val draftC285Claim       = sample[DraftClaim].copy(selectNumberOfClaimsAnswer = None)
      val foc                  = sample[FillingOutClaim].copy(draftClaim = draftC285Claim)
      val sessionData          = sample[SessionData].copy(journeyStatus = Some(foc))
      val request              = RequestWithSessionData(Some(sessionData), authenticatedRequest)

      JourneyExtractor.extractJourney(request) shouldBe JourneyBindable.Single
    }

    "Return the JourneyBindable belonging to the SelectNumberOfClaimsAnswer" in new TableDrivenPropertyChecks {

      val testCases = Table(
        ("NumberOfClaimsType", "JourneyBindable"),
        (SelectNumberOfClaimsAnswer.Individual, JourneyBindable.Single),
        (SelectNumberOfClaimsAnswer.Multiple, JourneyBindable.Multiple),
        (SelectNumberOfClaimsAnswer.Scheduled, JourneyBindable.Scheduled)
      )

      forAll(testCases) { (numberOfClaims, journeyBindable) =>
        val msgReq               = fakeRequest2MessageRequest(FakeRequest())
        val authenticatedRequest = AuthenticatedRequest[AnyContent](msgReq)
        val draftC285Claim       = sample[DraftClaim].copy(selectNumberOfClaimsAnswer = Some(numberOfClaims))
        val foc                  = sample[FillingOutClaim].copy(draftClaim = draftC285Claim)
        val sessionData          = sample[SessionData].copy(journeyStatus = Some(foc))
        val request              = RequestWithSessionData(Some(sessionData), authenticatedRequest)

        JourneyExtractor.extractJourney(request) shouldBe journeyBindable
      }

    }

  }
}
