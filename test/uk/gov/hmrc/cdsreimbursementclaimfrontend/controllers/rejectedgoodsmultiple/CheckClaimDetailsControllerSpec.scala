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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import org.jsoup.nodes.Document
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class CheckClaimDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: CheckClaimDetailsController = instanceOf[CheckClaimDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = CheckClaimDetailsController.key

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  def validateCheckClaimDetailsPage(
    doc: Document,
    claims: Map[MRN, Map[TaxCode, Option[BigDecimal]]]
  ) = {
    val (keys, values) = summaryKeyValue(doc)

    doc.select(".heading-mrn").eachText() should contain theSameElementsAs claims.keys.zipWithIndex.map {
      case (mrn, i) =>
        messages("check-claim.rejected-goods.multiple.duty.label", OrdinalNumber.label(i + 1).capitalize, mrn.value)
    }
    doc.select("#overall-total").text() shouldBe claims.map(_._2.values.map(_.get).sum).sum.toPoundSterlingString

    hasContinueButton(doc)
    formAction(
      doc
    ) shouldBe s"/claim-for-reimbursement-of-import-duties/rejected-goods/multiple/check-claim"

    val claimedTaxCodes = claims.flatMap(_._2.keys).map(tc => messages(s"tax-code.${tc.value}")).toSet
    val claimedAmounts  = claims.flatMap(_._2.values.map(_.get)).map(_.toPoundSterlingString).toSet
    val mrnTotals       = claims.map(_._2.values.map(_.get).sum).map(_.toPoundSterlingString).toSet

    keys   should contain allElementsOf claimedTaxCodes
    values should contain allElementsOf claimedAmounts
    values should contain allElementsOf mrnTotals
  }

  "CheckClaimDetailsController" when {

    "Show claim summary" must {

      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if all selected claims provided" in {
        forAll(incompleteJourneyWithCompleteClaimsGen(9)) { case (journey, _) =>
          assert(journey.hasCompleteReimbursementClaims)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          val claims: Map[MRN, Map[TaxCode, Option[BigDecimal]]] =
            journey.answers.reimbursementClaims.get

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.multiple.title"),
            doc => validateCheckClaimDetailsPage(doc, claims)
          )
        }
      }
    }

    "Submit" must {

      def performAction(value: String): Future[Result] =
        controller.submit()(
          FakeRequest()
            .withFormUrlEncodedBody("check-claim.rejected-goods" -> value)
        )

      "fail if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction("true"))  shouldBe NOT_FOUND
        status(performAction("false")) shouldBe NOT_FOUND
      }

      "redirect to the next page if answer is yes" in {
        forAll(incompleteJourneyWithCompleteClaimsGen(2)) { case (journey, _) =>
          assert(journey.hasCompleteReimbursementClaims)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction("true"),
            "/claim-for-reimbursement-of-import-duties/rejected-goods/multiple/enter-inspection-date"
          )
        }
      }

      "redirect back to the duties selection if answer is no" in {
        forAll(incompleteJourneyWithCompleteClaimsGen(2)) { case (journey, _) =>
          assert(journey.hasCompleteReimbursementClaims)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(SessionData(journey.withDutiesChangeMode(true)))(Right(()))
          }

          checkIsRedirect(
            performAction("false"),
            "/claim-for-reimbursement-of-import-duties/rejected-goods/multiple/select-duties"
          )
        }
      }

      "when in change mode redirect to the CYA page if answer is yes" in {
        forAll(completeJourneyGen) { case journey =>
          assert(journey.hasCompleteReimbursementClaims)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction("true"),
            "/claim-for-reimbursement-of-import-duties/rejected-goods/multiple/check-your-answers"
          )
        }
      }

      "when in change mode redirect back to the duties selection if answer is no" in {
        forAll(completeJourneyGen) { case journey =>
          assert(journey.hasCompleteReimbursementClaims)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(SessionData(journey.withDutiesChangeMode(true)))(Right(()))
          }

          checkIsRedirect(
            performAction("false"),
            "/claim-for-reimbursement-of-import-duties/rejected-goods/multiple/select-duties"
          )
        }
      }
    }
  }

}
