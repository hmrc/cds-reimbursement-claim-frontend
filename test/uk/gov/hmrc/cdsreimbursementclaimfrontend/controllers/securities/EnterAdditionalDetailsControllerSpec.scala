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
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.Future

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class EnterAdditionalDetailsControllerSpec
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

  val controller: EnterAdditionalDetailsController = instanceOf[EnterAdditionalDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val session: SessionData = SessionData(SecuritiesClaim.empty(exampleEori))

  private def mockCompleteClaim(claim: SecuritiesClaim) =
    inSequence {
      mockAuthWithDefaultRetrievals()
      mockGetSession(SessionData(claim))
    }

  "Enter Additional Details Controller" when {
    "Enter Additional Details page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "display the page when additional details given" in {
        forAll(buildCompleteClaimGen()) { claim =>
          val modifiedClaim     = claim.submitAdditionalDetails("additional details")
          mockCompleteClaim(modifiedClaim)
          val additionalDetails = modifiedClaim.answers.additionalDetails

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("enter-additional-details.securities.title"),
            doc =>
              doc
                .select("form textarea[name='enter-additional-details']")
                .`val`() shouldBe additionalDetails.get
          )
        }
      }

      "display the page when no additional details" in {
        forAll(buildCompleteClaimGen()) { claim =>
          val modifiedClaim = claim.submitAdditionalDetails("")
          mockCompleteClaim(modifiedClaim)

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("enter-additional-details.securities.title"),
            doc =>
              doc
                .select("form textarea[name='enter-additional-details']")
                .`val`() shouldBe ""
          )
        }
      }
    }

    "Submit Enter additional details page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data*))

      "submit valid additional details" in forAll(
        buildClaimGen(
          additionalDetailsVisited = true
        ).map(
          _.fold(e => throw new Exception(s"Cannot build complete SecuritiesClaim because of $e."), identity)
            .submitCheckYourAnswersChangeMode(false)
        )
      ) { claim =>
        val claim2 = claim.submitContactDetails(None)
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim2))
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim2))
          mockStoreSession(
            SessionData(
              claim2.submitAdditionalDetails("additional details")
            )
          )(Right(()))
        }

        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey("enter-additional-details.securities.title")
        )

        checkIsRedirect(
          performAction(
            "enter-additional-details" -> "additional details"
          ),
          routes.EnterContactDetailsController.show
        )
      }

      "submit valid additional details when contact details already provided" in forAll(
        buildClaimGen(
          additionalDetailsVisited = true
        ).map(
          _.fold(e => throw new Exception(s"Cannot build complete SecuritiesClaim because of $e."), identity)
            .submitCheckYourAnswersChangeMode(false)
        )
      ) { claim =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(
            SessionData(
              claim.submitAdditionalDetails("additional details")
            )
          )(Right(()))
        }

        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey("enter-additional-details.securities.title")
        )

        checkIsRedirect(
          performAction(
            "enter-additional-details" -> "additional details"
          ),
          routes.CheckYourAnswersController.show
        )
      }

      "submit valid additional details when claim is complete" in forAll(buildCompleteClaimGen()) { claim =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(
            session.copy(securitiesClaim =
              Some(
                claim.submitAdditionalDetails("additional details")
              )
            )
          )(Right(()))
        }

        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey("enter-additional-details.securities.title")
        )

        checkIsRedirect(
          performAction(
            "enter-additional-details" -> "additional details"
          ),
          routes.CheckYourAnswersController.show
        )
      }
    }
  }

}
