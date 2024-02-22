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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.AuthenticatedUserGen.individualGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen.genEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

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

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Overpayments_v2)

  val session: SessionData = SessionData.empty.copy(
    overpaymentsScheduledJourney = Some(OverpaymentsScheduledJourney.empty(exampleEori))
  )

  private def mockCompleteJourney(journey: OverpaymentsScheduledJourney, email: Email, name: contactdetails.Name) =
    inSequence {
      mockAuthorisedUserWithEoriNumber(journey.getClaimantEori, email.value, name.name, name.lastName)
      mockGetSession(session.copy(overpaymentsScheduledJourney = Some(journey)))
    }

  "Enter Contact Details Controller" when {
    "Enter Contact Details page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in {
        forAll(buildCompleteJourneyGen(), genEmail, genName, individualGen) { (journey, email, name, individual) =>
          mockCompleteJourney(journey, email, name)
          val contactDetails = journey.computeContactDetails(individual, individual.asVerifiedEmail)

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
    }

    "Submit Contact Details" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty contact details form" in forAll(buildCompleteJourneyGen(), genEmail, genName) {
        (journey, email, name) =>
          inSequence {
            mockAuthorisedUserWithEoriNumber(journey.getClaimantEori, email.value, name.name, name.lastName)
            mockGetSession(session.copy(overpaymentsScheduledJourney = Some(journey)))
            mockAuthorisedUserWithEoriNumber(journey.getClaimantEori, email.value, name.name, name.lastName)
            mockGetSession(session.copy(overpaymentsScheduledJourney = Some(journey.submitContactDetails(None))))
          }

          checkPageIsDisplayed(
            controller.show()(FakeRequest()),
            messageFromMessageKey("enter-contact-details.title")
          )

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("enter-contact-details.title"),
            doc => {
              getErrorSummary(doc) contains messageFromMessageKey(
                "enter-contact-details.contact-name.error.required"
              )
              getErrorSummary(doc) contains messageFromMessageKey(
                "enter-contact-details.contact-email.error.required"
              )
            },
            expectedStatus = BAD_REQUEST
          )
      }

      "submit a valid contact details" in forAll(buildCompleteJourneyGen(), genEmail, genName) {
        (journey, email, name) =>
          inSequence {
            mockAuthorisedUserWithEoriNumber(journey.getClaimantEori, email.value, name.name, name.lastName)
            mockGetSession(session.copy(overpaymentsScheduledJourney = Some(journey)))
            mockAuthorisedUserWithEoriNumber(journey.getClaimantEori, email.value, name.name, name.lastName)
            mockGetSession(session.copy(overpaymentsScheduledJourney = Some(journey)))
            mockStoreSession(
              session.copy(overpaymentsScheduledJourney =
                Some(
                  journey.submitContactDetails(
                    Some(
                      MrnContactDetails(name.toFullName, Some(email), None)
                        .computeChanges(
                          Some(
                            journey
                              .getInitialContactDetailsFromDeclarationAndCurrentUser(
                                uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AuthenticatedUser
                                  .Individual(Some(email), journey.getClaimantEori, Some(name)),
                                None
                              )
                          )
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
    }
  }

}
