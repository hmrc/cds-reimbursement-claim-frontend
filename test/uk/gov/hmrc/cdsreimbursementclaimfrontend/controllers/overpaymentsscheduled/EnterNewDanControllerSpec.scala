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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import org.scalacheck.Gen
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectEoriAndDan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Dan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class EnterNewDanControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterNewDanController = instanceOf[EnterNewDanController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  val session: SessionData = SessionData(
    OverpaymentsScheduledJourney
      .empty(anotherExampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleDisplayDeclaration.getMRN, exampleDisplayDeclaration)
      .getOrFail
  )

  val journeyGen: Gen[OverpaymentsScheduledJourney] =
    buildJourneyFromAnswersGen(answersUpToBasisForClaimGen())

  "New Dan Controller" when {
    "Enter New Dan page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page on a new journey" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-new-dan.title"),
          doc => {
            doc.getElementById("enter-new-dan-inset-text").text() shouldBe messageFromMessageKey(
              "enter-new-dan.inset-text"
            )
            doc.getElementById("enter-new-dan-hint").text()       shouldBe messageFromMessageKey(
              "enter-new-dan.hint"
            )
            doc.select("#enter-new-dan").`val`()                  shouldBe ""
            doc.select("form").attr("action")                     shouldBe routes.EnterNewDanController.submit.url
          }
        )
      }
    }

    "Submit New Dan" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty Dan" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(controller.formKey -> ""),
          messageFromMessageKey("enter-new-dan.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-new-dan.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid DAN" in {
        val invalidDan = Dan("invalid")

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(controller.formKey -> invalidDan.value),
          messageFromMessageKey("enter-new-dan.title"),
          doc => {
            getErrorSummary(doc)                 shouldBe messageFromMessageKey("enter-new-dan.invalid.number")
            doc.select("#enter-new-dan").`val`() shouldBe invalidDan.value
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid DAN - breeches max length" in {
        val invalidDan = Dan("12345678890")

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(controller.formKey -> invalidDan.value),
          messageFromMessageKey("enter-new-dan.title"),
          doc => {
            getErrorSummary(doc)                 shouldBe messageFromMessageKey("enter-new-dan.error.maxLength")
            doc.select("#enter-new-dan").`val`() shouldBe invalidDan.value
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "submit a valid Dan" in forAll(
        journeyGen.flatMap(j => j.submitBasisOfClaim(IncorrectEoriAndDan))
      ) { journey =>
        val dan = Dan("1234567")
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(SessionData(journey.submitNewDan(dan)))(Right(()))
        }

        checkIsRedirect(
          performAction(controller.formKey -> dan.value),
          routes.EnterAdditionalDetailsController.show
        )

      }
    }
  }
}
