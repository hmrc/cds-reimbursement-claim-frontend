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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.EoriDetailsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectEoriAndDan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class EnterNewEoriNumberControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  val mockConnector = mock[EoriDetailsConnector]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[EoriDetailsConnector].toInstance(mockConnector)
    )

  val controller: EnterNewEoriNumberController = instanceOf[EnterNewEoriNumberController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  val session: SessionData = SessionData(
    OverpaymentsMultipleJourney
      .empty(anotherExampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleDisplayDeclaration.getMRN, exampleDisplayDeclaration)
      .getOrFail
  )

  val journeyGen: Gen[OverpaymentsMultipleJourney] =
    buildJourneyFromAnswersGen(answersUpToBasisForClaimGen())

  def mockGetEoriDetails(eori: Eori)(
    response: Future[Option[connectors.EoriDetailsConnector.Response]]
  ) =
    (mockConnector
      .getEoriDetails(_: Eori)(_: HeaderCarrier))
      .expects(eori, *)
      .returning(response)

  "New Eori Number Controller" when {
    "Enter New Eori page" must {

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
          messageFromMessageKey("enter-new-eori-number.title"),
          doc => {
            doc.getElementById("enter-new-eori-inset-text").text()  shouldBe messageFromMessageKey(
              "enter-new-eori-number.inset-text"
            )
            doc.getElementById("enter-new-eori-number-hint").text() shouldBe messageFromMessageKey(
              "enter-new-eori-number.hint"
            )
            doc.select("#enter-new-eori-number").`val`()            shouldBe ""
            doc.select("form").attr("action")                       shouldBe routes.EnterNewEoriNumberController.submit.url
          }
        )
      }
    }

    "Submit New Eori" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty Eori" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(controller.formKey -> ""),
          messageFromMessageKey("enter-new-eori-number.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-new-eori-number.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid Eori" in {
        val invalidEori = Eori("invalid-eori")

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(controller.formKey -> invalidEori.value),
          messageFromMessageKey("enter-new-eori-number.title"),
          doc => {
            getErrorSummary(doc)                         shouldBe messageFromMessageKey("enter-new-eori-number.invalid.number")
            doc.select("#enter-new-eori-number").`val`() shouldBe invalidEori.value
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "submit a valid Eori format but eori does not exist" in forAll(
        journeyGen.flatMap(j => j.submitBasisOfClaim(IncorrectEoriAndDan))
      ) { journey =>
        val eori = Eori("GB123456123456")
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockGetEoriDetails(eori)(Future.successful(None))
        }

        checkPageIsDisplayed(
          performAction(controller.formKey -> eori.value),
          messageFromMessageKey("enter-new-eori-number.title"),
          doc => {
            getErrorSummary(doc)                         shouldBe messageFromMessageKey("enter-new-eori-number.doesNotExist")
            doc.select("#enter-new-eori-number").`val`() shouldBe eori.value
          },
          expectedStatus = BAD_REQUEST
        )

      }

      "submit a valid Eori - eori exists" in forAll(
        journeyGen.flatMap(j => j.submitBasisOfClaim(IncorrectEoriAndDan))
      ) { journey =>
        val eori = Eori("GB123456123456")
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockGetEoriDetails(eori)(
            Future.successful(
              Some(
                EoriDetailsConnector
                  .Response(
                    eoriGB = Eori("GB0123456789"),
                    eoriXI = Some(Eori("XI0123456789")),
                    fullName = "Foo Bar",
                    eoriEndDate = None
                  )
              )
            )
          )
          mockStoreSession(SessionData(journey.submitNewEori(eori)))(Right(()))
        }

        checkIsRedirect(
          performAction(controller.formKey -> eori.value),
          routes.EnterNewDanController.show
        )

      }
    }
  }
}
