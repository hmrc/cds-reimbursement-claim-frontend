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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsScheduledClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen.genEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.Future

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class EnterContactDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterContactDetailsController = instanceOf[EnterContactDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val session: SessionData = SessionData(OverpaymentsScheduledClaim.empty(exampleEori))

  "Enter Contact Details Controller" when {
    "Enter Contact Details page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "display the page" in {
        forAll(buildCompleteClaimGen()) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("enter-contact-details.title"),
            doc => doc.select("form").attr("action") shouldBe routes.EnterContactDetailsController.submit.url
          )
        }
      }

      "display the page with contact details populated" in {
        forAll(buildCompleteClaimGen().map(_.submitContactDetails(Some(exampleContactDetails)))) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("enter-contact-details.title"),
            doc => {
              doc.select("form").attr("action")  shouldBe routes.EnterContactDetailsController.submit.url
              doc.select("input").get(0).`val`() shouldBe exampleContactDetails.fullName
              doc.select("input").get(1).`val`() shouldBe exampleContactDetails.emailAddress.get.value
              doc.select("input").get(2).`val`() shouldBe exampleContactDetails.phoneNumber.get.value
            }
          )
        }
      }
    }

    "Submit Contact Details" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data*))

      "reject an empty contact details form" in forAll(buildCompleteClaimGen()) { claim =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-contact-details.title"),
          doc => {
            getErrorSummary(doc) `contains` messageFromMessageKey(
              "enter-contact-details.contact-name.error.required"
            )
            getErrorSummary(doc) `contains` messageFromMessageKey(
              "enter-contact-details.contact-email.error.required"
            )
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "submit a valid contact details" in forAll(
        buildCompleteClaimGen(submitContactAddress = false),
        genEmail,
        genName
      ) { (claim, email, name) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(
            session.copy(overpaymentsScheduledClaim =
              Some(
                claim.submitContactDetails(
                  Some(
                    MrnContactDetails(name.toFullName, Some(email), None).computeChanges(
                      claim.answers.contactDetails
                    )
                  )
                )
              )
            )
          )(Right(()))
        }

        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey("enter-contact-details.title")
        )

        checkIsRedirect(
          performAction(
            "enter-contact-details.contact-name"  -> name.toFullName,
            "enter-contact-details.contact-email" -> email.value
          ),
          routes.ClaimantAddressController.redirectToALF
        )
      }

      "fast forward to CYA page when claim is complete" in forAll(buildCompleteClaimGen(), genEmail, genName) {
        (claim, email, name) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockStoreSession(
              session.copy(overpaymentsScheduledClaim =
                Some(
                  claim.submitContactDetails(
                    Some(
                      MrnContactDetails(name.toFullName, Some(email), None).computeChanges(
                        claim.answers.contactDetails
                      )
                    )
                  )
                )
              )
            )(Right(()))
          }

          checkPageIsDisplayed(
            controller.show()(FakeRequest()),
            messageFromMessageKey("enter-contact-details.title")
          )

          checkIsRedirect(
            performAction(
              "enter-contact-details.contact-name"  -> name.toFullName,
              "enter-contact-details.contact-email" -> email.value
            ),
            routes.CheckYourAnswersController.show
          )
      }
    }
  }

}
