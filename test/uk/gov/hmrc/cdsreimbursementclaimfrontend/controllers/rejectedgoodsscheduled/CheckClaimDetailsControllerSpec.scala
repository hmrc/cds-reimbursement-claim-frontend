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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.completeJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.journeyWithMrnAndDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.ClaimsTableValidator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ClaimsTableHelper

import scala.concurrent.Future

class CheckClaimDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ClaimsTableValidator {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: CheckClaimDetailsController = instanceOf[CheckClaimDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  def assertPageContent(
    doc: Document,
    journey: RejectedGoodsScheduledJourney
  ): Unit = {
    val claims                       = ClaimsTableHelper.sortReimbursementsByDisplayDuty(journey.getReimbursements)
    val nonExciseDutyClaims          = journey.getNonExciseDutyClaims
    val selectedExciseCategoryClaims = journey.getSelectedExciseCategoryClaims
    val selectedExciseCategories     = selectedExciseCategoryClaims.keys.map(_.repr).toList

    validateClaimsTablesForScheduled(
      doc,
      nonExciseDutyClaims,
      selectedExciseCategoryClaims,
      routes.EnterClaimController.show
    )

    summaryKeyValueList(doc) should containOnlyPairsOf(
      Seq(
        m("check-claim.duty-types-summary.key") -> claims.keys
          .map(dutyType => m(s"select-duty-types.${dutyType.repr}"))
          .mkString(" "),
        m("check-claim.table.total")            -> journey.getTotalReimbursementAmount.toPoundSterlingString
      ) ++
        nonExciseDutyClaims.map { case (dutyType, claims) =>
          m(s"select-duty-codes.title.${dutyType.repr}") -> claims
            .map(claim => m(s"tax-code.${claim.taxCode}"))
            .mkString(" ")
        } ++
        Seq(
          m("select-duty-codes.title.excise-duty") -> selectedExciseCategories
            .map(category => m(s"excise-category.$category"))
            .mkString(" ")
        ).filter(_ => selectedExciseCategoryClaims.nonEmpty) ++
        selectedExciseCategoryClaims
          .map { case (category, claims) =>
            m(
              s"check-claim.duties-selected-summary.key",
              messages(s"select-excise-duty-codes.h1.${category.repr}")
            ) -> claims.map(claim => m(s"tax-code.${claim.taxCode}")).mkString(" ")
          }
          .filter(_ => selectedExciseCategoryClaims.nonEmpty)
    )
  }

  val session: SessionData = SessionData(journeyWithMrnAndDeclaration)

  "Check Claim Details Controller" should {
    def performActionShow(): Future[Result] =
      controller.show(FakeRequest())

    def performActionSubmit(data: (String, String)*): Future[Result] =
      controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

    "not find the page if rejected goods feature is disabled" in {
      featureSwitch.disable(Feature.RejectedGoods)

      status(controller.show(FakeRequest())) shouldBe NOT_FOUND

    }

    "redirect to the select duty types page" when {
      "the user has not entered reimbursement amounts" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(performActionShow(), routes.SelectDutyTypesController.show)

      }

    }

    "show the page" when {

      "duties, tax codes and amounts have been filled" in {
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performActionShow(),
            messageFromMessageKey("check-claim.scheduled.title"),
            assertPageContent(_, journey)
          )
        }
      }
    }

    "submit" must {

      "fail if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performActionSubmit()) shouldBe NOT_FOUND
      }

      "redirect to the next page if not all of the questions have been answered" in
        forAll(completeJourneyGen) { completeJourney =>
          val incompleteJourney = completeJourney.submitContactDetails(None)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(incompleteJourney))
          }

          checkIsRedirect(performActionSubmit(), routes.EnterInspectionDateController.show)
        }

      "redirect to the check your answers page if all of the questions have been answered" in {
        forAll(completeJourneyGen) { journey =>
          assert(journey.hasCompleteReimbursementClaims)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(performActionSubmit(), routes.CheckYourAnswersController.show)
        }
      }
    }
  }
}
