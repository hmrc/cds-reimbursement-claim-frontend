/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpayments

import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import org.scalatest.BeforeAndAfterEach
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v2.{routes => overpaymentsSingleRoutes}
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple_v2.{routes => overpaymentsMultipleRoutes}
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled_v2.{routes => overpaymentsScheduledRoutes}
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators.exampleEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OverpaymentsJourneyType.Individual
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OverpaymentsJourneyType.Multiple
//import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OverpaymentsJourneyType.Scheduled
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.overpayments.choose_how_many_mrns

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import collection.JavaConverters._

class ChooseHowManyMrnsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  implicit val cc: MessagesControllerComponents = instanceOf[MessagesControllerComponents]
  implicit val errorHandler: ErrorHandler       = instanceOf[ErrorHandler]

  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData =
    instanceOf[AuthenticatedActionWithRetrievedData]
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData     =
    instanceOf[SessionDataActionWithRetrievedData]

  val chooseHowManyMrnsPage: choose_how_many_mrns = instanceOf[choose_how_many_mrns]

  val controller: ChooseHowManyMrnsController =
    new ChooseHowManyMrnsController(
      authenticatedActionWithRetrievedData,
      sessionDataActionWithRetrievedData,
      mockSessionCache,
      chooseHowManyMrnsPage
    )

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val featureSwitch: FeatureSwitchService = instanceOf[FeatureSwitchService]
  override def beforeEach(): Unit         = featureSwitch.enable(Feature.Overpayments_v2)

  private val formKey = "overpayments.choose-how-many-mrns"

  private val eoriExample = exampleEori

  "ChooseHowManyMrnsController" must {

    def performAction(): Future[Result] = controller.show()(FakeRequest())

    "display the page" in {
      inSequence {
        mockAuthWithEoriEnrolmentRetrievals(exampleEori)
        mockGetSession(SessionData.empty)
      }

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey(s"$formKey.title"),
        doc => {
          val buttons          = radioButtons(doc)
          val individualButton = extractButton(buttons, "Individual")
          val multipleButton   = extractButton(buttons, "Multiple")
          val scheduledButton  = extractButton(buttons, "Scheduled")
          extractLabel(individualButton) shouldBe messageFromMessageKey(s"$formKey.individual.title")
          extractHint(individualButton)  shouldBe ""
          extractLabel(multipleButton)   shouldBe messageFromMessageKey(s"$formKey.multiple.title")
          extractHint(multipleButton)    shouldBe messageFromMessageKey(s"$formKey.multiple.hint")
          extractLabel(scheduledButton)  shouldBe messageFromMessageKey(s"$formKey.scheduled.title")
          extractHint(scheduledButton)   shouldBe messageFromMessageKey(s"$formKey.scheduled.hint")
        }
      )
    }

    "Handle submissions" should {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "Redirect to (single route) EnterMovementReferenceNumber page when user chooses Individual" in {

        val updatedSession = SessionData(OverpaymentsSingleJourney.empty(eoriExample, Nonce.Any))

        inSequence {
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetSession(SessionData.empty)
          mockStoreSession(updatedSession)(Right(()))
        }

        val result = performAction(Seq("overpayments.choose-how-many-mrns" -> Individual.toString))
        checkIsRedirect(result, overpaymentsSingleRoutes.EnterMovementReferenceNumberController.show)
      }

      // "Redirect to (multiple route) EnterMovementReferenceNumber page when user chooses Multiple" in {

      //   val updatedSession = SessionData(OverpaymentsMultipleJourney.empty(eoriExample, Nonce.Any))

      //   inSequence {
      //     mockAuthWithEoriEnrolmentRetrievals(exampleEori)
      //     mockGetSession(SessionData.empty)
      //     mockStoreSession(updatedSession)(Right(()))
      //   }

      //   val result = performAction(Seq("overpayments.choose-how-many-mrns" -> Multiple.toString))
      //   checkIsRedirect(result, overpaymentsMultipleRoutes.EnterMovementReferenceNumberController.showFirst)
      // }

      // "Redirect to (scheduled route) EnterMovementReferenceNumber page when user chooses Scheduled" in {
      //   val updatedSession = SessionData(OverpaymentsScheduledJourney.empty(eoriExample, Nonce.Any))

      //   inSequence {
      //     mockAuthWithEoriEnrolmentRetrievals(exampleEori)
      //     mockGetSession(SessionData.empty)
      //     mockStoreSession(updatedSession)(Right(()))
      //   }

      //   val result = performAction(Seq("overpayments.choose-how-many-mrns" -> Scheduled.toString))
      //   checkIsRedirect(result, overpaymentsScheduledRoutes.EnterMovementReferenceNumberController.show)
      // }

      "Show error message when no data selected" in {
        inSequence {
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetSession(SessionData.empty)
        }

        checkPageIsDisplayed(
          performAction(Seq()),
          messageFromMessageKey(s"$formKey.title"),
          doc =>
            extractErrorMessage(
              doc
            ) shouldBe "<span class=\"govuk-visually-hidden\">Error:</span> " + messageFromMessageKey(
              s"$formKey.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }
    }
  }

  private def extractErrorMessage(doc: Document): String =
    doc.select(".govuk-error-message").html()

  private def radioButtons(doc: Document): Elements =
    doc.select(s"div.govuk-radios div.govuk-radios__item")

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  private def extractButton(buttons: Elements, requiredValue: String): Element =
    buttons.asScala.filterNot(button => button.select(s"""input[value="$requiredValue"]""").isEmpty).head

  private def extractLabel(button: Element): String =
    button.select("label").text()

  private def extractHint(button: Element): String =
    button.select("div.govuk-hint").text()

}
