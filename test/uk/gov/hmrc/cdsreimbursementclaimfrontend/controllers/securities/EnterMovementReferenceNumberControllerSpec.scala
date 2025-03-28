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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging

import scala.concurrent.Future

class EnterMovementReferenceNumberControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with Logging {

  val enterMovementReferenceNumberKey: String          = "enter-movement-reference-number"
  val enterMovementReferenceNumberKeyAndSubKey: String = s"$enterMovementReferenceNumberKey.securities"

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterMovementReferenceNumberController = instanceOf[EnterMovementReferenceNumberController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = {
    featureSwitch.enable(Feature.Securities)
    featureSwitch.disable(Feature.LimitedAccessSecurities)
  }

  val journey              = SecuritiesJourney.empty(exampleEori)
  val session: SessionData = SessionData(journey)

  "Movement Reference Number Controller" when {
    "Enter MRN page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page on a new journey" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$enterMovementReferenceNumberKeyAndSubKey.title"),
          doc => {
            doc.select(s"#$enterMovementReferenceNumberKey").`val`() shouldBe ""
            doc.select("form").attr("action")                        shouldBe routes.EnterMovementReferenceNumberController.submit.url
          }
        )
      }

      "display the page on a pre-existing journey" in {

        val journey = buildCompleteJourneyGen(
          acc14DeclarantMatchesUserEori = false,
          submitContactDetails = false
        ).sample.getOrElse(fail("Unable to generate complete journey"))

        val mrn            = journey.answers.movementReferenceNumber.getOrElse(fail("No mrn found in journey"))
        val sessionToAmend = SessionData(journey)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(sessionToAmend)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$enterMovementReferenceNumberKeyAndSubKey.title"),
          doc => doc.select(s"#$enterMovementReferenceNumberKey").`val`() shouldBe mrn.value
        )
      }
    }

    "Submit MRN page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "save an MRN if valid and continue to the choose reason for security page" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockStoreSession(
            SessionData(journey.submitMovementReferenceNumber(exampleMrn))
          )(Right(()))
        }

        checkIsRedirect(
          performAction(enterMovementReferenceNumberKey -> exampleMrnAsString),
          routes.ChooseReasonForSecurityController.show
        )
      }

      "continue to the check choose reason for security page when MRN didn't change and NOT in a change mode" in {
        val initialJourney =
          journey
            .submitMovementReferenceNumber(exampleMrn)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkIsRedirect(
          performAction(enterMovementReferenceNumberKey -> exampleMrnAsString),
          routes.ChooseReasonForSecurityController.show
        )
      }

      "continue to the check declaration details page when MRN didn't change and in a change mode" in {
        val initialJourney =
          journey
            .submitMovementReferenceNumber(exampleMrn)
            .submitCheckDeclarationDetailsChangeMode(true)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkIsRedirect(
          performAction(enterMovementReferenceNumberKey -> exampleMrnAsString),
          routes.CheckDeclarationDetailsController.show
        )
      }

      "reject an invalid MRN" in {
        val invalidMRN = MRN("INVALID_MOVEMENT_REFERENCE_NUMBER")

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(enterMovementReferenceNumberKey -> invalidMRN.value),
          messageFromMessageKey(s"$enterMovementReferenceNumberKeyAndSubKey.title"),
          doc => {
            getErrorSummary(doc)                                        shouldBe messageFromMessageKey(
              s"$enterMovementReferenceNumberKeyAndSubKey.invalid.number"
            )
            doc.getElementById(enterMovementReferenceNumberKey).`val`() shouldBe invalidMRN.value
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an empty MRN" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(enterMovementReferenceNumberKey -> ""),
          messageFromMessageKey(s"$enterMovementReferenceNumberKeyAndSubKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$enterMovementReferenceNumberKeyAndSubKey.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

    }
  }
}
