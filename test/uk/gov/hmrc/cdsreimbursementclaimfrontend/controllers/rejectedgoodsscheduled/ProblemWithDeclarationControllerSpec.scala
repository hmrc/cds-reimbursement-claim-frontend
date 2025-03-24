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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import org.jsoup.Jsoup
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class ProblemWithDeclarationControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: ProblemWithDeclarationController = instanceOf[ProblemWithDeclarationController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.enable(Feature.RejectedGoods)

  val session: SessionData = SessionData(journeyWithMrnAndDeclaration)

  val messagesKey: String = "problem-with-declaration"

  "Problem with Declaration Controller" when {
    "Problem with Declaration page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "does not find the page if the rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page with no continue option when all tax codes are unsupported" in {
        val journey = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclarationWithOnlyUnsupportedCodes)
          .getOrElse(fail("Failed to submit MRN and declaration."))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc =>
            doc
              .select("main p")
              .get(0)
              .text() shouldBe Jsoup
              .parse(
                messageFromMessageKey(s"$messagesKey.paragraph.1", journey.answers.movementReferenceNumber.get.value)
              )
              .text()
        )
      }

      "display the page with continue option when some tax codes are unsupported" in {
        val journey = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclarationWithSomeUnsupportedCode)
          .getOrElse(fail("Failed to submit MRN and declaration."))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            doc
              .select("main p")
              .get(0)
              .text() shouldBe Jsoup
              .parse(
                messageFromMessageKey(s"$messagesKey.paragraph.1", journey.answers.movementReferenceNumber.get.value)
              )
              .text()
            doc
              .select("main legend")
              .get(0)
              .text() shouldBe Jsoup.parse(messageFromMessageKey(s"$messagesKey.continue.question")).text()
          }
        )
      }

      "redirect to check declaration details page when declaration contains no unsupported tax codes" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.CheckDeclarationDetailsController.show
        )
      }
    }

    "Submit problem with declaration page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty Yes/No answer" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageWithErrorIsDisplayed(
          performAction("problem-with-declaration" -> ""),
          messageFromMessageKey(s"$messagesKey.title"),
          messageFromMessageKey(s"$messagesKey.error.required")
        )
      }

      "submit when user selects Yes" in {
        val journey = emptyJourney
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclarationWithSomeUnsupportedCode)
          .getOrElse(fail("Failed to submit MRN and declaration."))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(
            SessionData(
              journey.removeUnsupportedTaxCodes()
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction("problem-with-declaration" -> "true"),
          routes.CheckDeclarationDetailsController.show
        )
      }

      "submit when user selects No" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction("problem-with-declaration" -> "false"),
          routes.EnterMovementReferenceNumberController.show
        )
      }

      "redirect to CYA page when user has seen CYA page" in {
        val journey = completeJourneyGen.sample
          .getOrElse(fail("Journey building has failed."))
          .submitCheckYourAnswersChangeMode(true)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction("problem-with-declaration" -> "true"),
          routes.CheckYourAnswersController.show
        )
      }
    }
  }
}
