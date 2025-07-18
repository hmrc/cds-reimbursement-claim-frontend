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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators.buildCompleteJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators.emptyJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen.genEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.Future

class EnterContactDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with JourneyTestData {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterContactDetailsController = instanceOf[EnterContactDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val session: SessionData = SessionData.empty.copy(overpaymentsMultipleJourney = Some(emptyJourney))

  val messageKey: String = "enter-movement-reference-number"

  private def mockCompleteJourney(
    journey: OverpaymentsMultipleJourney
  ) =
    inSequence {
      mockAuthWithDefaultRetrievals()
      mockGetSession(SessionData(journey))
    }

  "Enter Contact Details Controller" when {
    "Show Contact Details page" must {
      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "display the page" in {
        forAll(buildCompleteJourneyGen()) { journey =>
          mockCompleteJourney(journey)
          val contactDetails = journey.answers.contactDetails

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("enter-contact-details.title"),
            doc => {
              doc
                .select("form input[name='enter-contact-details.contact-name']")
                .`val`() shouldBe contactDetails.get.fullName
              doc
                .select("form input[name='enter-contact-details.contact-email']")
                .`val`() shouldBe contactDetails.get.emailAddress.get.value
            }
          )
        }
      }

      "display the page with contact details populated" in {
        forAll(buildCompleteJourneyGen().map(_.submitContactDetails(Some(exampleContactDetails)))) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
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

      "reject an empty contact details form" in forAll(buildCompleteJourneyGen()) { journey =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey.submitContactDetails(None)))
        }

        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey("enter-contact-details.title")
        )

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
        buildCompleteJourneyGen(submitContactAddress = false),
        genEmail,
        genName
      ) { (journey, email, name) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(
            session.copy(overpaymentsMultipleJourney =
              Some(
                journey.submitContactDetails(
                  Some(
                    MrnContactDetails(name.toFullName, Some(email), None).computeChanges(
                      journey.answers.contactDetails
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
          routes.CheckClaimantDetailsController.show
        )
      }

      "fast forward to CYA page when claim is complete" in forAll(buildCompleteJourneyGen(), genEmail, genName) {
        (journey, email, name) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(
              session.copy(overpaymentsMultipleJourney =
                Some(
                  journey.submitContactDetails(
                    Some(
                      MrnContactDetails(name.toFullName, Some(email), None).computeChanges(
                        journey.answers.contactDetails
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
