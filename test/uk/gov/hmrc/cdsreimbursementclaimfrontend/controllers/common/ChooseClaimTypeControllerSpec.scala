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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common

import com.typesafe.config.ConfigFactory
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
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.inject.guice.GuiceableModule
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.Application
import play.api.Configuration
import play.api.Logger
import play.api.MarkerContext
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.EoriDetailsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.ChooseClaimTypeController.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpayments.routes as overpaymentsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoods.routes as rejectedGoodsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.routes as securitiesRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.TestDefaultMessagesApiProvider
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.choose_claim_type
import uk.gov.hmrc.mongo.play.PlayMongoModule

import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class ChooseClaimTypeControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[EoriDetailsConnector].toInstance(mockEoriDetailsConnector)
    )

  private val exampleEori: Eori = IdGen.genEori.sample.get

  private val encodedEori =
    new String(Base64.getEncoder.encode(exampleEori.value.getBytes(StandardCharsets.UTF_8)), StandardCharsets.UTF_8)

  override def buildFakeApplication(): Application =
    new GuiceApplicationBuilder()
      .configure(
        Configuration(
          ConfigFactory.parseString(
            s"""
              | metrics.jvm = false
              | metrics.enabled = false
              | metrics.logback = false
              | auditing.enabled = false
              | microservice.upscan-initiate.upscan-store.expiry-time = 1
              | limited-access-securities-eori-csv-base64 = "$encodedEori"
          """.stripMargin
          )
        )
      )
      .disable[PlayMongoModule]
      .overrides(featuresCacheBinding :: overrideBindings*)
      .overrides(bind[MessagesApi].toProvider[TestDefaultMessagesApiProvider])
      .build()

  lazy val featureSwitch = instanceOf[FeatureSwitchService]

  implicit val cc: MessagesControllerComponents = instanceOf[MessagesControllerComponents]
  implicit val errorHandler: ErrorHandler       = instanceOf[ErrorHandler]

  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData =
    instanceOf[AuthenticatedActionWithRetrievedData]
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData     =
    instanceOf[SessionDataActionWithRetrievedData]
  val authenticatedAction: AuthenticatedAction                                   = instanceOf[AuthenticatedAction]
  val sessionDataAction: SessionDataAction                                       = instanceOf[SessionDataAction]
  val chooseClaimTypePage: choose_claim_type                                     = instanceOf[choose_claim_type]

  class LoggerStub extends Logger(logger = null) {

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
      authenticatedActionWithRetrievedData,
      sessionDataActionWithRetrievedData,
      authenticatedAction,
      sessionDataAction,
      mockSessionCache,
      featureSwitch,
      chooseClaimTypePage
    ) {
      override val logger: Logger = stubLogger
    }

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val formKey = "choose-claim-type"

  "ChooseClaimTypeController" must {

    def performAction(): Future[Result] = controller.show(FakeRequest())

    "display the page" in {
      val eori: Eori = Eori("AB12345678901234Z")
      inSequence {
        mockAuthWithEoriEnrolmentRetrievals(eori)
        mockGetEoriDetails(eori)
        mockGetSession(SessionData.empty)
      }

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey(s"$formKey.title"),
        doc => {
          val buttons             = radioButtons(doc)
          val c285Button          = extractButton(buttons, "C285")
          val rejectedGoodsButton = extractButton(buttons, "RejectedGoods")
          val securitiesButton    = extractButton(buttons, "Securities")
          extractLabel(c285Button)                       shouldBe messageFromMessageKey(s"$formKey.c285.title")
          extractHint(c285Button)                        shouldBe messageFromMessageKey(s"$formKey.c285.hint")
          extractLabel(rejectedGoodsButton)              shouldBe messageFromMessageKey(s"$formKey.ce1179.title")
          extractHint(rejectedGoodsButton)               shouldBe messageFromMessageKey(s"$formKey.ce1179.hint")
          extractLabel(securitiesButton)                 shouldBe messageFromMessageKey(s"$formKey.securities.title")
          extractHint(securitiesButton)                  shouldBe messageFromMessageKey(s"$formKey.securities.hint")
          doc.select(".govuk-inset-text").text().isEmpty shouldBe false
        }
      )
    }

    "Handle submissions" should {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "Redirect to SelectNumberOfClaims if user chooses C285" in {
        inSequence {
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetEoriDetails(exampleEori)
          mockGetSession(SessionData.empty)
        }

        val result = performAction(Seq(dataKey -> C285.toString))
        checkIsRedirect(result, overpaymentsRoutes.ChooseHowManyMrnsController.show)
      }

      "Redirect to choose how many mrns if user chooses C&E1179" in {
        inSequence {
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetEoriDetails(exampleEori)
          mockGetSession(SessionData.empty)
        }

        val result = performAction(Seq(dataKey -> RejectedGoods.toString))
        checkIsRedirect(
          result,
          rejectedGoodsRoutes.ChooseHowManyMrnsController.show
        )
      }

      "Redirect to the enter mrn if user chooses Securities" in {
        inSequence {
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetEoriDetails(exampleEori)
          mockGetSession(SessionData.empty)
          mockStoreSession(
            SessionData(
              verifiedEmail = None,
              securitiesJourney = Some(SecuritiesJourney.empty(exampleEori, Nonce.Any))
            )
          )(
            Right(())
          )
        }

        val result = performAction(Seq(dataKey -> Securities.toString))
        checkIsRedirect(
          result,
          securitiesRoutes.EnterMovementReferenceNumberController.show
        )
      }

      "Show error page when no data selected" in {
        inSequence {
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetEoriDetails(exampleEori)
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
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetEoriDetails(exampleEori)
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
    doc.select(".govuk-error-message").text()

  private def radioButtons(doc: Document): Elements =
    doc.select("div.govuk-radios div.govuk-radios__item")

  private def hasButton(buttons: Elements, requiredValue: String): Boolean =
    buttons.asScala.exists(button => !button.select(s"""input[value="$requiredValue"]""").isEmpty)

  private def extractButton(buttons: Elements, requiredValue: String): Element =
    buttons.asScala.filterNot(button => button.select(s"""input[value="$requiredValue"]""").isEmpty).head

  private def extractLabel(button: Element): String =
    button.select("label").html()

  private def extractHint(button: Element): String =
    button.select("div.govuk-hint").html()
}
