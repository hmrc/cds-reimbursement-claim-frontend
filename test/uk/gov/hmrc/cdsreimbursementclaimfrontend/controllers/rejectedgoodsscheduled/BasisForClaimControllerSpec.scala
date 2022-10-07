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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import org.scalacheck.Gen
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.buildCompleteJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfRejectedGoodsClaim.SpecialCircumstances
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.alphaNumGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future
import play.api.mvc.Call

class BasisForClaimControllerSpec
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

  val controller: BasisForClaimController = instanceOf[BasisForClaimController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session = SessionData(journeyWithMrnAndDD)

  "Enter Basis for claim Controller" when {
    "Show Basis for claim page" must {

      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page on a new journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("select-basis-for-claim.rejected-goods.title"),
          doc => {
            doc
              .select("main p")
              .text()               shouldBe messageFromMessageKey("select-basis-for-claim.rejected-goods.help-text")
            selectedRadioValue(doc) shouldBe None
          }
        )
      }

      "display the page on a pre-existing journey" in forAll(buildCompleteJourneyGen()) { journey =>
        whenever(journey.answers.basisOfClaim.isDefined) {
          val basisOfClaims = journey.answers.basisOfClaim.map(_.toString)
          val session       = SessionData(journey)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("select-basis-for-claim.rejected-goods.title"),
            doc => {
              doc
                .select("main p")
                .text()               shouldBe messageFromMessageKey("select-basis-for-claim.rejected-goods.help-text")
              selectedRadioValue(doc) shouldBe basisOfClaims
            }
          )
        }
      }
    }

    "Submit Basis for claim page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty basis for claim" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(controller.formKey -> ""),
          messageFromMessageKey("select-basis-for-claim.rejected-goods.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey("select-basis-for-claim.rejected-goods.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid basis for claim" in forAll(alphaNumGenerator(20)) { invalidBasis =>
        whenever(invalidBasis.nonEmpty && !BasisOfRejectedGoodsClaim.has(invalidBasis)) {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(controller.formKey -> invalidBasis),
            messageFromMessageKey("select-basis-for-claim.rejected-goods.title"),
            doc =>
              getErrorSummary(doc) shouldBe messageFromMessageKey(
                "select-basis-for-claim.rejected-goods.error.required"
              ),
            expectedStatus = BAD_REQUEST
          )
        }
      }

      "submit a valid basis for claim" in forAll(Gen.oneOf(BasisOfRejectedGoodsClaim.values)) { basisOfClaim =>
        val journey        = session.rejectedGoodsScheduledJourney.getOrElse(fail("No rejected goods journey"))
        val updatedJourney = journey.submitBasisOfClaim(basisOfClaim)
        val updatedSession = SessionData(updatedJourney)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        val checkBasisOfClaim = basisOfClaim match {
          case SpecialCircumstances =>
            Call(
              "GET",
              "/claim-back-import-duty-vat/rejected-goods/scheduled/enter-special-circumstances"
            )
          case _                    =>
            Call(
              "GET",
              "/claim-back-import-duty-vat/rejected-goods/scheduled/choose-disposal-method"
            )
        }

        checkIsRedirect(
          performAction(controller.formKey -> basisOfClaim.toString),
          checkBasisOfClaim
        )
      }
    }
  }

}
