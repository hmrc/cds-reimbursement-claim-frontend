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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoods

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.EoriDetailsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple.routes as rejectedGoodsMultipleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.routes as rejectedGoodsScheduledRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.routes as rejectedGoodsSingleRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaimGenerators.exampleEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaimGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RejectedGoodsClaimType.Individual
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RejectedGoodsClaimType.Multiple
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.RejectedGoodsClaimType.Scheduled
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.rejectedgoods.choose_how_many_mrns

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class ChooseHowManyMrnsControllerSpec
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

  implicit val cc: MessagesControllerComponents = instanceOf[MessagesControllerComponents]
  implicit val errorHandler: ErrorHandler       = instanceOf[ErrorHandler]

  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData =
    instanceOf[AuthenticatedActionWithRetrievedData]
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData     =
    instanceOf[SessionDataActionWithRetrievedData]

  val chooseHowManyMrnsPage: choose_how_many_mrns = instanceOf[choose_how_many_mrns]

  val featureSwitch: FeatureSwitchService = instanceOf[FeatureSwitchService]

  val controller: ChooseHowManyMrnsController =
    new ChooseHowManyMrnsController(
      authenticatedActionWithRetrievedData,
      sessionDataActionWithRetrievedData,
      mockSessionCache,
      chooseHowManyMrnsPage
    )

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val formKey = "rejected-goods.choose-how-many-mrns"

  private val eoriExample = exampleEori

  "ChooseHowManyMrnsController" must {

    def performAction(): Future[Result] = controller.show(FakeRequest())

    "redirect to the page when start" in {
      val result = controller.start(FakeRequest())
      checkIsRedirect(result, routes.ChooseHowManyMrnsController.show)
    }

    "display the page" in {
      inSequence {
        mockAuthWithEoriEnrolmentRetrievals(exampleEori)
        mockGetEoriDetails(exampleEori)
        mockGetSession(SessionData.empty)
      }

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey(s"$formKey.title"),
        doc => {
          val bullets = doc.select("ul.govuk-list li").asScala.toList
          bullets.length shouldBe 3

          val buttons          = radioButtons(doc)
          val individualButton = extractButton(buttons, "Individual")
          val multipleButton   = extractButton(buttons, "Multiple")
          val scheduledButton  = extractButton(buttons, "Scheduled")
          extractLabel(individualButton) shouldBe messageFromMessageKey(s"$formKey.individual.title")
          extractLabel(multipleButton)   shouldBe messageFromMessageKey(s"$formKey.multiple.title")
          extractLabel(scheduledButton)  shouldBe messageFromMessageKey(s"$formKey.scheduled.title")
        }
      )
    }

    "display the page with method of payment info" in {
      inSequence {
        mockAuthWithEoriEnrolmentRetrievals(exampleEori)
        mockGetEoriDetails(exampleEori)
        mockGetSession(SessionData.empty)
      }

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey(s"$formKey.title"),
        doc => {
          val bullets = doc.select("ul.govuk-list li").asScala.toList
          bullets.length shouldBe 3
          bullets.zipWithIndex.foreach { (bullet, index) =>
            bullet.text() shouldBe messageFromMessageKey(s"choose-how-many-mrns.p1.li${index + 1}")
          }

          val buttons          = radioButtons(doc)
          val individualButton = extractButton(buttons, "Individual")
          val multipleButton   = extractButton(buttons, "Multiple")
          val scheduledButton  = extractButton(buttons, "Scheduled")
          extractLabel(individualButton) shouldBe messageFromMessageKey(s"$formKey.individual.title")
          extractLabel(multipleButton)   shouldBe messageFromMessageKey(s"$formKey.multiple.title")
          extractLabel(scheduledButton)  shouldBe messageFromMessageKey(s"$formKey.scheduled.title")
        }
      )
    }

    "Handle submissions" should {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "Redirect to (single route) HaveDocumentsReady page when user chooses individual and the claim is already finalized" in {

        val initialSession = SessionData(
          rejectedGoodsSingleClaim = Some(
            RejectedGoodsSingleClaim
              .empty(eoriExample, Nonce.Any)
              .finalizeClaimWith("FOO-1234567890")
              .getOrElse(
                RejectedGoodsSingleClaim.empty(eoriExample, Nonce.Any)
              )
          )
        )

        val updatedSession =
          SessionData(rejectedGoodsSingleClaim = Some(RejectedGoodsSingleClaim.empty(eoriExample, Nonce.Any)))

        inSequence {
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetEoriDetails(exampleEori)
          mockGetSession(initialSession)
          mockStoreSession(updatedSession)(Right(()))
        }

        val result = performAction(Seq(controller.dataKey -> Individual.toString))
        checkIsRedirect(result, rejectedGoodsSingleRoutes.HaveDocumentsReadyController.show)
      }

      "Redirect to (multiple route) HaveDocumentsReady page when user chooses Multiple and claim is already finalized" in {

        val initialSession = SessionData(
          rejectedGoodsMultipleClaim = Some(
            RejectedGoodsMultipleClaim
              .empty(eoriExample, Nonce.Any)
              .finalizeClaimWith("FOO-1234567890")
              .getOrElse(
                RejectedGoodsMultipleClaim.empty(eoriExample, Nonce.Any)
              )
          )
        )

        val updatedSession =
          SessionData(rejectedGoodsMultipleClaim = Some(RejectedGoodsMultipleClaim.empty(eoriExample, Nonce.Any)))

        inSequence {
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetEoriDetails(exampleEori)
          mockGetSession(initialSession)
          mockStoreSession(updatedSession)(Right(()))
        }

        val result = performAction(Seq(controller.dataKey -> Multiple.toString))
        checkIsRedirect(result, rejectedGoodsMultipleRoutes.HaveDocumentsReadyController.show)
      }

      "Redirect to (scheduled route) HaveDocumentsReady page when user chooses Scheduled and claim is already finalized" in {

        val initialSession = SessionData(
          rejectedGoodsScheduledClaim = Some(
            RejectedGoodsScheduledClaim
              .empty(eoriExample, Nonce.Any)
              .finalizeClaimWith("FOO-1234567890")
              .getOrElse(
                RejectedGoodsScheduledClaim.empty(eoriExample, Nonce.Any)
              )
          )
        )

        val updatedSession =
          SessionData(rejectedGoodsScheduledClaim = Some(RejectedGoodsScheduledClaim.empty(eoriExample, Nonce.Any)))

        inSequence {
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetEoriDetails(exampleEori)
          mockGetSession(initialSession)
          mockStoreSession(updatedSession)(Right(()))
        }

        val result = performAction(Seq(controller.dataKey -> Scheduled.toString))
        checkIsRedirect(result, rejectedGoodsScheduledRoutes.HaveDocumentsReadyController.show)
      }

      "Redirect to (single route) HaveDocumentsReady page when user chooses Individual" in {

        val updatedSession = SessionData(RejectedGoodsSingleClaim.empty(eoriExample, Nonce.Any))

        inSequence {
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetEoriDetails(exampleEori)
          mockGetSession(SessionData.empty)
          mockStoreSession(updatedSession)(Right(()))
        }

        val result = performAction(Seq(controller.dataKey -> Individual.toString))
        checkIsRedirect(result, rejectedGoodsSingleRoutes.HaveDocumentsReadyController.show)
      }

      "Redirect to (multiple route) HaveDocumentsReady page when user chooses Multiple" in {

        val updatedSession = SessionData(RejectedGoodsMultipleClaim.empty(eoriExample, Nonce.Any))

        inSequence {
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetEoriDetails(exampleEori)
          mockGetSession(SessionData.empty)
          mockStoreSession(updatedSession)(Right(()))
        }

        val result = performAction(Seq(controller.dataKey -> Multiple.toString))
        checkIsRedirect(result, rejectedGoodsMultipleRoutes.HaveDocumentsReadyController.show)
      }

      "Redirect to (scheduled route) EnterMovementReferenceNumber page when user chooses Scheduled" in {
        val updatedSession = SessionData(RejectedGoodsScheduledClaim.empty(eoriExample, Nonce.Any))

        inSequence {
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetEoriDetails(exampleEori)
          mockGetSession(SessionData.empty)
          mockStoreSession(updatedSession)(Right(()))
        }

        val result = performAction(Seq(controller.dataKey -> Scheduled.toString))
        checkIsRedirect(result, rejectedGoodsScheduledRoutes.HaveDocumentsReadyController.show)
      }

      "Show error message when no data selected" in {
        inSequence {
          mockAuthWithEoriEnrolmentRetrievals(exampleEori)
          mockGetEoriDetails(exampleEori)
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
    doc.select("div.govuk-radios div.govuk-radios__item")

  private def extractButton(buttons: Elements, requiredValue: String): Element =
    buttons.asScala.filterNot(button => button.select(s"""input[value="$requiredValue"]""").isEmpty).head

  private def extractLabel(button: Element): String =
    button.select("label").text()
}
