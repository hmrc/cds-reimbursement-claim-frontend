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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen.genEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.RetrievedUserTypeGen.individualGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class EnterContactDetailsControllerSpec
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

  val controller: EnterContactDetailsController = instanceOf[EnterContactDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  val session: SessionData = SessionData.empty.copy(
    securitiesJourney = Some(SecuritiesJourney.empty(exampleEori))
  )

  "Enter Contact Details Controller" when {
    "Enter Contact Details page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in {
        forAll(buildCompleteJourneyGen()) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(securitiesJourney = Some(journey)))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("enter-contact-details-securities.change.title"),
            doc => {
              doc
                .select("form input[name='enter-contact-details-securities.contact-name']")
                .`val`() shouldBe ""
              doc
                .select("form input[name='enter-contact-details-securities.contact-email']")
                .`val`() shouldBe ""
            }
          )
        }
      }
    }

    "Submit Basis for claim page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty contact details form" in forAll(buildCompleteJourneyGen()) { journey =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(securitiesJourney = Some(journey)))
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(securitiesJourney = Some(journey.submitContactDetails(None))))
        }

        checkPageIsDisplayed(
          controller.show()(FakeRequest()),
          messageFromMessageKey("enter-contact-details-securities.change.title")
        )

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-contact-details-securities.change.title"),
          doc => {
            getErrorSummary(doc) contains messageFromMessageKey(
              "enter-your-contact-details.contact-name.error.required"
            )
            getErrorSummary(doc) contains messageFromMessageKey(
              "enter-your-contact-details.contact-email.error.required"
            )
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "submit a valid basis for claim" in forAll(buildCompleteJourneyGen(), genEmail, genName) {
        (journey, email, name) =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(securitiesJourney = Some(journey)))
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(securitiesJourney = Some(journey)))
            mockStoreSession(
              session.copy(securitiesJourney =
                Some(journey.submitContactDetails(Some(MrnContactDetails(name.toFullName, email, None))))
              )
            )(Right(()))
          }

          checkPageIsDisplayed(
            controller.show()(FakeRequest()),
            messageFromMessageKey("enter-contact-details-securities.change.title")
          )

          checkIsRedirect(
            performAction(
              "enter-contact-details-securities.contact-name"  -> name.toFullName,
              "enter-contact-details-securities.contact-email" -> email.value
            ),
            routes.CheckClaimantDetailsController.show()
          )
      }
    }
  }

}
