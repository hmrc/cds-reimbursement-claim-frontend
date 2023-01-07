/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions

import org.scalamock.scalatest.MockFactory
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.mvc.MessagesRequest
import play.api.test.FakeRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.C285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RetrievedUserType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UserType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.ContactName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.C285ClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.RetrievedUserTypeGen.arbitraryIndividual
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney

class RequestWithSessionDataAndRetrievedDataSpec
    extends ControllerSpec
    with MockFactory
    with SessionSupport
    with AuthActionSpec
    with ScalaCheckDrivenPropertyChecks {

  implicit lazy val messagesApi: MessagesApi = instanceOf[MessagesApi]

  private lazy val messagesRequest =
    new MessagesRequest(FakeRequest(), messagesApi)

  def authenticatedRequest(user: RetrievedUserType.Individual): AuthenticatedRequestWithRetrievedData[_] =
    AuthenticatedRequestWithRetrievedData(
      user,
      Some(UserType.Individual),
      messagesRequest
    )

  def signedInUser(user: RetrievedUserType.Individual): SignedInUserDetails = SignedInUserDetails(
    user.email,
    user.eori,
    Email(""),
    ContactName(user.name.flatMap(_.name).get)
  )

  "Request with session data and retrieved data" should {
    "determine the signed in user details" when {
      "No claims journey has started" in forAll { randomIndividual: RetrievedUserType.Individual =>
        val request = RequestWithSessionDataAndRetrievedData(
          SessionData.empty,
          authenticatedRequest(randomIndividual)
        )
        request.signedInUserDetails shouldBe None
      }

      "We have a C285 Filling Out Claim" in forAll { randomIndividual: RetrievedUserType.Individual =>
        val fillingOutClaim: JourneyStatus =
          JourneyStatus.FillingOutClaim(
            randomIndividual.ggCredId,
            signedInUser(randomIndividual),
            DraftClaim.blank
          )
        val request                        = RequestWithSessionDataAndRetrievedData(
          SessionData(Some(fillingOutClaim)),
          authenticatedRequest(randomIndividual)
        )
        request.signedInUserDetails shouldBe Some(signedInUser(randomIndividual))
      }

      "We have a C285 Just Submitted Claim" in forAll { randomIndividual: RetrievedUserType.Individual =>
        val claim: C285Claim = sample[C285Claim]

        val justSubmittedClaim: JourneyStatus =
          JourneyStatus.JustSubmittedClaim(
            randomIndividual.ggCredId,
            signedInUser(randomIndividual),
            claim,
            SubmitClaimResponse("Case Number")
          )

        val request =
          RequestWithSessionDataAndRetrievedData(
            SessionData(Some(justSubmittedClaim)),
            authenticatedRequest(randomIndividual)
          )

        request.signedInUserDetails shouldBe Some(signedInUser(randomIndividual))
      }

      "We have a C285 Failed Submitted Claim" in forAll { randomIndividual: RetrievedUserType.Individual =>
        val claim: JourneyStatus =
          JourneyStatus.SubmitClaimFailed(
            randomIndividual.ggCredId,
            signedInUser(randomIndividual)
          )
        val request              = RequestWithSessionDataAndRetrievedData(
          SessionData(Some(claim)),
          authenticatedRequest(randomIndividual)
        )
        request.signedInUserDetails shouldBe Some(signedInUser(randomIndividual))
      }

      "We have a C&E1179 Single Journey Claim" in forAll { randomIndividual: RetrievedUserType.Individual =>
        val singleJourney = RejectedGoodsSingleJourney.empty(randomIndividual.eori)
        val request       = RequestWithSessionDataAndRetrievedData(
          SessionData(singleJourney),
          authenticatedRequest(randomIndividual)
        )
        request.signedInUserDetails shouldBe Some(signedInUser(randomIndividual))
      }

      "We have a C&E1179 Multiple Journey Claim" in forAll { randomIndividual: RetrievedUserType.Individual =>
        val multipleJourney = RejectedGoodsMultipleJourney.empty(randomIndividual.eori)
        val request         = RequestWithSessionDataAndRetrievedData(
          SessionData(multipleJourney),
          authenticatedRequest(randomIndividual)
        )
        request.signedInUserDetails shouldBe Some(signedInUser(randomIndividual))
      }

      "We have a C&E1179 Scheduled Journey Claim" in forAll { randomIndividual: RetrievedUserType.Individual =>
        val scheduledJourney = RejectedGoodsScheduledJourney.empty(randomIndividual.eori)
        val request          = RequestWithSessionDataAndRetrievedData(
          SessionData(scheduledJourney),
          authenticatedRequest(randomIndividual)
        )
        request.signedInUserDetails shouldBe Some(signedInUser(randomIndividual))
      }

      "We have a Securities Journey Claim" in forAll { randomIndividual: RetrievedUserType.Individual =>
        val securitiesJourney = SecuritiesJourney.empty(randomIndividual.eori)
        val request           = RequestWithSessionDataAndRetrievedData(
          SessionData(securitiesJourney),
          authenticatedRequest(randomIndividual)
        )
        request.signedInUserDetails shouldBe Some(signedInUser(randomIndividual))
      }
    }
  }
}
