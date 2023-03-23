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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import org.jsoup.nodes.Document
import org.scalatest.BeforeAndAfterEach
import play.api.http.Status.NOT_FOUND
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator

import scala.List
import scala.jdk.CollectionConverters._
import scala.concurrent.Future

class CheckClaimDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with TestWithJourneyGenerator[SecuritiesJourney]
    with SummaryMatchers {

  val mockClaimsService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService)
    )

  val controller: CheckClaimDetailsController = instanceOf[CheckClaimDetailsController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "check-claim.securities"

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  def validateCheckClaimDetailsPage(
    doc: Document,
    journey: SecuritiesJourney
  ) = {
    val headers       = doc.select("h2.govuk-heading-m").eachText().asScala.toList
    val paragraph     = doc.select("p.govuk-body").text()
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

    paragraph shouldBe messages("check-claim.securities.summary")

    headers should not be empty

    val expectedHeaders: Seq[String] =
      journey.getSelectedDepositIds.map((depositId: String) => s"Security ID: $depositId").toList

    headers should contain theSameElementsAs expectedHeaders

    val expectedSummaries: Seq[(String, Option[String])] =
      journey.getSelectedDepositIds.flatMap((sid: String) =>
        Seq(
          ("Claim full amount" -> Some(if (journey.isFullSecurityAmountClaimed(sid)) "Yes" else "No")),
          ("Duties selected"   -> Some(
            journey
              .getSelectedDutiesFor(sid)
              .get
              .sorted
              .map(taxCode => messages(s"tax-code.$taxCode"))
              .mkString(" ")
          )),
          ("Total"             -> Some(journey.getTotalReclaimAmountFor(sid).getOrElse(BigDecimal("0.00")).toPoundSterlingString))
        ) ++ journey.answers.securitiesReclaims.get
          .get(sid)
          .get
          .map {
            case (taxCode, Some(amount)) =>
              (messages(s"tax-code.$taxCode") -> Some(amount.toPoundSterlingString))
            case (taxCode, None)         =>
              throw new Exception(
                s"Expected claims to be provided for all duties for depositId=$sid, but missing one for the $taxCode"
              )
          }
      )

    summaries.toSeq should containOnlyDefinedPairsOf(expectedSummaries)

  }

  "CheckClaimDetailsController" when {
    "show page" must {
      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display page" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyWithClaimsEntered
        )
      ) { case (initialJourney, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(initialJourney))
          mockStoreSession(SessionData(initialJourney.submitCheckClaimDetailsChangeMode(true)))(Right(()))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateCheckClaimDetailsPage(doc, initialJourney)
        )

      }
    }

    "submit page" must {
      def performAction(): Future[Result] = controller.submit()(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "redirect to the next page" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyWithClaimsEntered
        )
      ) { case (initialJourney, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(initialJourney.submitCheckClaimDetailsChangeMode(true)))
        }

        checkIsRedirect(
          performAction(),
          if (initialJourney.needsBanksAccountDetailsSubmission)
            routes.CheckBankDetailsController.show()
          else if (initialJourney.needsDocumentTypeSelection)
            routes.ChooseFileTypeController.show()
          else
            routes.UploadFilesController.show()
        )
      }

      "redirect to the CYA page when in change your answers mode" in forAll(completeJourneyGen) { initialJourney =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkIsRedirect(
          performAction(),
          routes.CheckYourAnswersController.show()
        )
      }

      "when at least one security that has bank payment method but no bank details in ACC14, clicking continue should go to the enter bank details when confirm full repayment is yes" in {
        forAll(buildCompleteJourneyGen(submitFullAmount = true, submitBankAccountDetails = false)) { journey =>
          whenever(
            journey.needsBanksAccountDetailsSubmission && !journey.haveBankDetailsOnAcc14
          ) {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(journey))
            }

            checkIsRedirect(
              performAction(),
              routes.ChooseBankAccountTypeController.show()
            )
          }
        }
      }

      "when at least one security that has bank payment method but no bank details in ACC14, clicking continue should go to the enter bank details when confirm full repayment is no" in {
        forAll(buildCompleteJourneyGen(submitFullAmount = false, submitBankAccountDetails = false)) { journey =>
          whenever(
            journey.needsBanksAccountDetailsSubmission && !journey.haveBankDetailsOnAcc14
          ) {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(journey))
            }

            checkIsRedirect(
              performAction(),
              routes.ChooseBankAccountTypeController.show()
            )
          }
        }
      }
    }
  }

}
