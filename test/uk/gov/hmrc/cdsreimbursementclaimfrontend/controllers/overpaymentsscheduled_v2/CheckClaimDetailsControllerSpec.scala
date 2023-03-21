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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled_v2

import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled_v2.CheckClaimDetailsController.checkClaimDetailsKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.completeJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators.journeyWithMrnAndDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class CheckClaimDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: CheckClaimDetailsController = instanceOf[CheckClaimDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "multiple-check-claim-summary"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session: SessionData = SessionData(journeyWithMrnAndDeclaration)

  "Check Claim Details Controller" should {
    def performActionShow(): Future[Result] =
      controller.show()(FakeRequest())

    def performActionSubmit(data: (String, String)*): Future[Result] =
      controller.submit()(
        FakeRequest()
          .withFormUrlEncodedBody(data: _*)
      )

    "not find the page if rejected goods feature is disabled" in {
      featureSwitch.disable(Feature.RejectedGoods)

      status(controller.show()(FakeRequest())) shouldBe NOT_FOUND

    }

    "redirect to the select duty types page" when {
      "the user has not entered reimbursement amounts" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(performActionShow(), routes.SelectDutyTypesController.show)

      }

    }

    "show the page" when {

      "duties, tax codes and amounts have been filled" in {
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performActionShow(),
            messageFromMessageKey(messageKey = s"$checkClaimDetailsKey.scheduled.title"),
            doc => formAction(doc) shouldBe routes.CheckClaimDetailsController.submit.url
          )
        }
      }
    }

    "submit" must {

      "fail if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performActionSubmit()) shouldBe NOT_FOUND
      }

      "redirect to the next page if the answer is yes if not all of the questions have been answered" in
        forAll(completeJourneyGen) { completeJourney =>
          val incompleteJourney = completeJourney.submitContactDetails(None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(incompleteJourney))
          }

          checkIsRedirect(performActionSubmit("check-claim-summary" -> "true"), routes.CheckBankDetailsController.show)
        }

      "redirect to the check your answers page if the answer is yes if all of the questions have been answered" in {
        forAll(completeJourneyGen) { journey =>
          assert(journey.hasCompleteReimbursementClaims)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(performActionSubmit("check-claim-summary" -> "true"), routes.CheckYourAnswersController.show)
        }
      }

      "redirect back to select duty types page if the answer is no" in {
        forAll(completeJourneyGen) { journey =>
          assert(journey.hasCompleteReimbursementClaims)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(performActionSubmit("check-claim-summary" -> "false"), routes.SelectDutyTypesController.show)

        }
      }

    }

  }

}
