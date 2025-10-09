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

import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.i18n.*
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.alphaNumGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.genStringWithMaxSizeOfN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.Future

class EnterSpecialCircumstancesControllerSpec
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

  val controller: EnterSpecialCircumstancesController = instanceOf[EnterSpecialCircumstancesController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "enter-special-circumstances.rejected-goods"

  val session: SessionData = SessionData(claimWithMrnAndDeclaration)

  "Enter Special Circumstances Controller" must {
    "Show Page" when {
      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "display the page on a new claim" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            formAction(doc)       shouldBe routes.EnterSpecialCircumstancesController.submit.url
            selectedTextArea(doc) shouldBe Some("")
          }
        )
      }

      "display the page on a change claim" in forAll(completeClaimGenWithSpecialCircumstances) { claim =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            formAction(doc)       shouldBe routes.EnterSpecialCircumstancesController.submit.url
            selectedTextArea(doc) shouldBe claim.answers.basisOfClaimSpecialCircumstances
          }
        )
      }
    }

    "Submit Page" when {
      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "reject submit if no special circumstances entered" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messagesKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject submit if the special circumstances are too long" in forAll(alphaNumGenerator(600)) { details =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(messagesKey -> details),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messagesKey.error.maxLength"),
          expectedStatus = BAD_REQUEST
        )
      }

      "redirect to ineligible page if trying to submit special circumstances and this is not the basis of the claim" in forAll(
        genStringWithMaxSizeOfN(50),
        Gen.oneOf(BasisOfRejectedGoodsClaim.allButSpecialCircumstances)
      ) { (details, basisOfClaim) =>
        val claim = session.rejectedGoodsScheduledClaim.get.submitBasisOfClaim(basisOfClaim)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performAction(messagesKey -> details),
          baseRoutes.IneligibleController.ineligible
        )
      }

      "redirect to the choose disposal method if a valid set of special circumstances are entered" in forAll(
        genStringWithMaxSizeOfN(50)
      ) { details =>
        val claim        =
          session.rejectedGoodsScheduledClaim.get.submitBasisOfClaim(BasisOfRejectedGoodsClaim.SpecialCircumstances)
        val updatedClaim = claim
          .submitBasisOfClaimSpecialCircumstancesDetails(details)
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(SessionData(updatedClaim))(Right(()))
        }

        checkIsRedirect(
          performAction(messagesKey -> details),
          "choose-disposal-method"
        )
      }
    }
  }
}
