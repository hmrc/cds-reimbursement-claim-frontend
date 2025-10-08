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

import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.alphaNumGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.genStringWithMaxSizeOfN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.Future

class EnterRejectedGoodsDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  private val session = SessionData(claimWithMrnAndDeclaration)

  val controller: EnterRejectedGoodsDetailsController = instanceOf[EnterRejectedGoodsDetailsController]

  val formKey: String = "enter-rejected-goods-details.rejected-goods"

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  "Enter Rejected Goods Details Controller" should {

    "show page" when {
      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "display the page on a new claim" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$formKey.title"),
          doc => {
            formAction(doc)           shouldBe routes.EnterRejectedGoodsDetailsController.submit.url
            selectedTextArea(doc).get shouldBe ""
          }
        )
      }

      "display the page on a pre-existing claim" in forAll(genStringWithMaxSizeOfN(50)) { details =>
        val claim = session.rejectedGoodsScheduledClaim.get.submitDetailsOfRejectedGoods(details)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$formKey.title"),
          doc => {
            formAction(doc)           shouldBe routes.EnterRejectedGoodsDetailsController.submit.url
            selectedTextArea(doc).get shouldBe details
          }
        )
      }
    }

    "submit page" when {
      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "reject an empty form" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$formKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$formKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an too large an amount of text" in forAll(alphaNumGenerator(600)) { details =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(formKey -> details),
          messageFromMessageKey(s"$formKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$formKey.error.maxLength"),
          expectedStatus = BAD_REQUEST
        )
      }

      "accept less than 500 characters text" in forAll(genStringWithMaxSizeOfN(50)) { details =>
        val updatedClaim   = session.rejectedGoodsScheduledClaim.get.submitDetailsOfRejectedGoods(details)
        val updatedSession = SessionData(updatedClaim)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(formKey -> details),
          routes.SelectDutyTypesController.show
        )
      }
    }
  }
}
