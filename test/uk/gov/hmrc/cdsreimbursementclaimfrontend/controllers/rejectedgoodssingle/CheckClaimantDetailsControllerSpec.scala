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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.{Credentials, Name}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.EnrolmentConfig.EoriEnrolment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.buildCompleteJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.{RejectedGoodsSingleJourney, RejectedGoodsSingleJourneyTestData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.EmailGen.genEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.{genEori, genName}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Feature, SessionData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import scala.concurrent.Future

class CheckClaimantDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with RejectedGoodsSingleJourneyTestData {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: CheckClaimantDetailsController = instanceOf[CheckClaimantDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session = SessionData.empty.copy(
    rejectedGoodsSingleJourney = Some(RejectedGoodsSingleJourney.empty(exampleEori))
  )

  "Check Claimant Details Controller" when {
    "Check Claimant Details page" must {

      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in {
        forAll(buildCompleteJourneyGen(), genEmail, genName) { (journey, email, name) =>
          val sessionToAmend = session.copy(rejectedGoodsSingleJourney = Some(journey))

          inSequence {
            mockAuthWithAllRetrievals(
              Some(AffinityGroup.Individual),
              Some(email.value),
              Set(
                Enrolment(EoriEnrolment.key)
                  .withIdentifier(EoriEnrolment.eoriEnrolmentIdentifier, journey.getClaimantEori.value)
              ),
              Some(Credentials("id", "GovernmentGateway")),
              Some(Name(name.name, name.lastName))
            )
            mockGetSession(sessionToAmend)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("check-claimant-details.title")
          )
        }
      }

      "redirect to the Mrn Entry page if no Acc14 response obtained yet" in {
        forAll(genEmail, genName, genEori) { (email, name, eori) =>
          inSequence {
            mockAuthWithAllRetrievals(
              Some(AffinityGroup.Individual),
              Some(email.value),
              Set(Enrolment(EoriEnrolment.key).withIdentifier(EoriEnrolment.eoriEnrolmentIdentifier, eori.value)),
              Some(Credentials("id", "GovernmentGateway")),
              Some(Name(name.name, name.lastName))
            )
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(),
            routes.EnterMovementReferenceNumberController.show()
          )
        }
      }
    }
  }
}
