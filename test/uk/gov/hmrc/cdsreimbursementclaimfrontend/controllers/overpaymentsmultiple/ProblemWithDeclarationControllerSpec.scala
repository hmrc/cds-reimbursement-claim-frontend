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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

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
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

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

  val session: SessionData = SessionData(journeyWithMrnAndDeclaration)

  val messagesKey: String = "problem-with-declaration"

  "Problem with Declaration Controller" when {
    "Problem with Declaration page" must {

      def performAction(): Future[Result] = controller.showNth(1)(FakeRequest())

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
                messageFromMessageKey(s"$messagesKey.paragraph.1", journey.getLeadMovementReferenceNumber.get.value)
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
                messageFromMessageKey(s"$messagesKey.paragraph.1", journey.getLeadMovementReferenceNumber.get.value)
              )
              .text()
            doc
              .select("main legend")
              .get(0)
              .text() shouldBe Jsoup.parse(messageFromMessageKey(s"$messagesKey.continue.question")).text()
          }
        )
      }

      "redirect to check movement reference numbers page when declaration contains no unsupported tax codes" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.CheckMovementReferenceNumbersController.show
        )
      }
    }

    "Submit problem with declaration page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submitNth(1)(FakeRequest().withFormUrlEncodedBody(data*))

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
          routes.CheckMovementReferenceNumbersController.show
        )
      }

      "submit when user selects No" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction("problem-with-declaration" -> "false"),
          routes.EnterMovementReferenceNumberController.show(1)
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
