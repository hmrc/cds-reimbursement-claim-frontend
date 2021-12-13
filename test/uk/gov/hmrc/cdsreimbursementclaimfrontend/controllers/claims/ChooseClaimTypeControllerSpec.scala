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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements
import play.api.{Logger, MarkerContext}
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.http.Status.BAD_REQUEST
import play.api.mvc.{MessagesControllerComponents, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.ChooseClaimTypeController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.{routes => rejectedGoodsSingleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.choose_claim_type
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import collection.JavaConverters._

class ChooseClaimTypeControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  implicit val cc: MessagesControllerComponents = instanceOf[MessagesControllerComponents]
  implicit val errorHandler: ErrorHandler       = instanceOf[ErrorHandler]

  val authenticatedAction: AuthenticatedAction = instanceOf[AuthenticatedAction]
  val sessionDataAction: SessionDataAction     = instanceOf[SessionDataAction]
  val chooseClaimTypePage: choose_claim_type   = instanceOf[choose_claim_type]

  class LoggerStub extends Logger(logger = null) {
    @SuppressWarnings(Array("org.wartremover.warts.Var"))
    private var loggedErrorMessages: Vector[String] = Vector.empty

    def resetMessages(): Unit = loggedErrorMessages = Vector.empty

    def verify(message: String): Boolean =
      loggedErrorMessages.contains(message)

    override def error(message: => String)(implicit mc: MarkerContext): Unit =
      loggedErrorMessages = loggedErrorMessages :+ message
  }

  val stubLogger: LoggerStub = new LoggerStub

  val controller: ChooseClaimTypeController =
    new ChooseClaimTypeController(
      authenticatedAction,
      sessionDataAction,
      mockSessionCache,
      chooseClaimTypePage
    ) {
      override val logger: Logger = stubLogger
    }

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val featureSwitch: FeatureSwitchService = instanceOf[FeatureSwitchService]

  private val formKey = "choose-claim-type"

  "ChooseClaimTypeController" must {

    featureSwitch.RejectedGoods.enable()

    def performAction(): Future[Result] = controller.show()(FakeRequest())

    "display the page" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(SessionData.empty)
      }

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey(s"$formKey.title"),
        doc => {
          val buttons             = radioButtons(doc)
          val c285Button          = extractButton(buttons, "C285")
          val rejectedGoodsButton = extractButton(buttons, "RejectedGoods")
          extractLabel(c285Button)          shouldBe messageFromMessageKey(s"$formKey.c285.title")
          extractHint(c285Button)           shouldBe ""
          extractLabel(rejectedGoodsButton) shouldBe messageFromMessageKey(s"$formKey.ce1179.title")
          extractHint(rejectedGoodsButton)  shouldBe messageFromMessageKey(s"$formKey.ce1179.hint")
        }
      )
    }

    "Handle submissions" should {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "Redirect to SelectNumberOfClaims if user chooses C285" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty)
        }

        val result = performAction(Seq(dataKey -> C285.toString))
        checkIsRedirect(result, routes.SelectTypeOfClaimController.show())
      }

      "Redirect to choose how many mrns if user chooses C&E1179" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty)
        }

        val result = performAction(Seq(dataKey -> RejectedGoods.toString))
        checkIsRedirect(
          result,
          rejectedGoodsSingleRoutes.EnterMovementReferenceNumberController.show()
        )
      }

      "Show error page when no data selected" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty)
        }

        checkPageIsDisplayed(
          performAction(Seq()),
          messageFromMessageKey(s"$formKey.title"),
          doc => extractErrorMessage(doc) shouldBe "Error: " + messageFromMessageKey(s"$dataKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "Raise an exception if the data received is not expected" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty)
        }

        checkPageIsDisplayed(
          performAction(Seq(dataKey -> "HACKED_VALUE")),
          messageFromMessageKey(s"$formKey.title"),
          doc => extractErrorMessage(doc) shouldBe "Error: " + messageFromMessageKey(s"$dataKey.error.required"),
          expectedStatus = BAD_REQUEST
        )

        stubLogger.verify("Invalid claim form type supplied - HACKED_VALUE") shouldBe true
      }
    }
  }

  private def extractErrorMessage(doc: Document): String =
    doc.select("span.govuk-error-message").text()

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
