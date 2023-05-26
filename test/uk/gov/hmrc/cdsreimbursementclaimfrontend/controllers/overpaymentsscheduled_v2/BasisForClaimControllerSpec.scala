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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled_v2

import org.jsoup.nodes.Document
import org.scalacheck.Gen
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaimsList
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future
import scala.jdk.CollectionConverters._

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class BasisForClaimControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: BasisForClaimController = instanceOf[BasisForClaimController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Overpayments_v2)

  val journeyGen: Gen[OverpaymentsScheduledJourney] =
    buildJourneyFromAnswersGen(answersUpToBasisForClaimGen())

  def assertPageContent(
    doc: Document,
    expectedBasisOfClaims: BasisOfOverpaymentClaimsList,
    selectedBasisOfClaim: Option[BasisOfOverpaymentClaim]
  ): Any =
    doc
      .select("div.govuk-radios__item > input[name='select-basis-for-claim']")
      .listIterator()
      .asScala
      .map(e =>
        (
          e.siblingElements().first().text(),
          e.attr("value").toInt,
          e.hasAttr("checked")
        )
      )
      .toSeq should contain theSameElementsAs (
      expectedBasisOfClaims
        .map(basisOfClaim =>
          (
            messages(expectedBasisOfClaims.buildKey("select-basis-for-claim", basisOfClaim)),
            BasisOfOverpaymentClaimsList.indexOf(basisOfClaim),
            selectedBasisOfClaim.contains(basisOfClaim)
          )
        )
    )

  "Basis For Claim Controller" when {
    "Basis For Claim page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display page the first time" in {
        forAll(journeyGen) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("select-basis-for-claim.title"),
            assertPageContent(_, journey.getAvailableClaimTypes, None)
          )
        }
      }

      "display page back with answer populated" in {
        forAll(journeyGen.flatMap(j => Gen.oneOf(j.getAvailableClaimTypes).map(b => (j, b)))) {
          case (journey, basisOfClaim) =>
            val journeyWithBasisOfClaim =
              journey.submitBasisOfClaim(basisOfClaim)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(journeyWithBasisOfClaim))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("select-basis-for-claim.title"),
              assertPageContent(
                _,
                journeyWithBasisOfClaim.getAvailableClaimTypes,
                journeyWithBasisOfClaim.answers.basisOfClaim
              )
            )
        }
      }

      "display page back in the change mode" in {
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("select-basis-for-claim.title"),
            assertPageContent(_, journey.getAvailableClaimTypes, journey.answers.basisOfClaim)
          )
        }
      }
    }

    "Submit Basis For Claim" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "submit a valid basis for claim index" in forAll(
        journeyGen.flatMap(j => Gen.oneOf(j.getAvailableClaimTypes).map(b => (j, b)))
      ) { case (journey, basisOfClaim) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(
            SessionData(journey.submitBasisOfClaim(basisOfClaim))
          )(Right(()))
        }

        val number: Int =
          BasisOfOverpaymentClaimsList.indexOf(basisOfClaim)

        checkIsRedirect(
          performAction("select-basis-for-claim" -> number.toString),
          routes.EnterAdditionalDetailsController.show
        )
      }

      "submit an invalid basis for claim index" in forAll(journeyGen) { journey =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction("select-basis-for-claim" -> "1000"),
          messageFromMessageKey("select-basis-for-claim.title"),
          assertPageContent(_, journey.getAvailableClaimTypes, None),
          expectedStatus = BAD_REQUEST
        )
      }
    }
  }

}
