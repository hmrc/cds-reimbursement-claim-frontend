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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

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

  val journeyGen: Gen[OverpaymentsMultipleJourney] =
    buildJourneyFromAnswersGen(answersUpToBasisForClaimGen())

  def assertPageContent(
    doc: Document,
    expectedBasisOfClaims: Set[BasisOfOverpaymentClaim],
    selectedBasisOfClaim: Option[BasisOfOverpaymentClaim]
  ): Any =
    doc
      .select("div.govuk-radios__item > input[name='select-basis-for-claim']")
      .listIterator()
      .asScala
      .map(e =>
        (
          e.siblingElements().first().text(),
          e.attr("value"),
          e.hasAttr("checked")
        )
      )
      .toSeq should contain theSameElementsAs (
      expectedBasisOfClaims
        .map(basisOfClaim =>
          (
            messages(s"select-basis-for-claim.reason.$basisOfClaim"),
            basisOfClaim.toString(),
            selectedBasisOfClaim.contains(basisOfClaim)
          )
        )
    )

  "Basis For Claim Controller" when {
    "Basis For Claim page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "display page the first time" in {
        forAll(journeyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
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
              mockAuthWithDefaultRetrievals()
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
            mockAuthWithDefaultRetrievals()
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
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "submit a valid basis for claim index" in forAll(
        journeyGen.flatMap(j => Gen.oneOf(j.getAvailableClaimTypes).map(b => (j, b)))
      ) { case (journey, basisOfClaim) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(
            SessionData(journey.submitBasisOfClaim(basisOfClaim))
          )(Right(()))
        }

        checkIsRedirect(
          performAction("select-basis-for-claim" -> basisOfClaim.toString),
          if basisOfClaim === BasisOfOverpaymentClaim.IncorrectEoriAndDan then routes.EnterNewEoriNumberController.show
          else routes.EnterAdditionalDetailsController.show
        )
      }

      "submit an invalid basis for claim index" in forAll(journeyGen) { journey =>
        inSequence {
          mockAuthWithDefaultRetrievals()
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
