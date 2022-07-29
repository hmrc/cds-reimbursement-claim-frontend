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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.EitherOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils

import scala.List
import scala.collection.JavaConverters._
import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator

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
            journey.getSelectedDutiesFor(sid).get.map(taxCode => messages(s"tax-code.${taxCode.value}")).mkString(" ")
          )),
          ("Total"             -> Some(journey.getTotalReclaimAmountFor(sid).getOrElse(BigDecimal("0.00")).toPoundSterlingString))
        ) ++ journey.answers.securitiesReclaims.get
          .get(sid)
          .get
          .map {
            case (taxCode, Some(amount)) =>
              (messages(s"tax-code.${taxCode.value}") -> Some(amount.toPoundSterlingString))
            case (taxCode, None)         =>
              throw new Exception(
                s"Expected claims to be provided for all duties for depositId=$sid, but missing one for the ${taxCode.value}"
              )
          }
      )

    summaries should containOnlyDefinedPairsOf(expectedSummaries)

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
          testParamsGenerator = mrnWithNonExportRfsWithDisplayDeclarationWithReclaimsGen,
          journeyBuilder = buildSecuritiesJourneyWithClaimsEntered
        )
      ) { case (initialJourney, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateCheckClaimDetailsPage(doc, initialJourney)
        )

      }
    }
  }

//      "redirect to the ineligible page if an invalid security deposit ID" in {
//        mrnWithNonExportRfsWithDisplayDeclarationGen.sample.map { case (mrn, rfs, decl) =>
//          val initialJourney = emptyJourney
//            .submitMovementReferenceNumber(mrn)
//            .submitReasonForSecurityAndDeclaration(rfs, decl)
//            .flatMap(_.submitClaimDuplicateCheckStatus(false))
//            .getOrFail
//
//          inSequence {
//            mockAuthWithNoRetrievals()
//            mockGetSession(SessionData(initialJourney))
//          }
//
//          checkIsRedirect(
//            performAction("foo-123-xyz"),
//            baseRoutes.IneligibleController.ineligible()
//          )
//
//        }
//      }
//
//      "display the page if a valid security deposit ID" in {
//        forAll(mrnWithNonExportRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
//          val initialJourney = emptyJourney
//            .submitMovementReferenceNumber(mrn)
//            .submitReasonForSecurityAndDeclaration(rfs, decl)
//            .flatMap(_.submitClaimDuplicateCheckStatus(false))
//            .getOrFail
//
//          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
//
//          for (depositId <- depositIds) {
//            inSequence {
//              mockAuthWithNoRetrievals()
//              mockGetSession(SessionData(initialJourney))
//            }
//
//            checkPageIsDisplayed(
//              performAction(depositId),
//              messageFromMessageKey(s"$messagesKey.title"),
//              doc => validateSelectSecuritiesPage(doc, initialJourney, depositId)
//            )
//          }
//        }
//      }
//
//    }
//
//    "submitting securities selection" must {
//
//      def performAction(id: String, data: (String, String)*): Future[Result] =
//        controller.submit(id)(FakeRequest().withFormUrlEncodedBody(data: _*))
//
//      "select the first security deposit and move to the next security" in {
//        forAll(mrnWithNonExportRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
//          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
//
//          whenever(depositIds.size >= 2) {
//            val firstDepositId  = depositIds(0)
//            val secondDepositId = depositIds(1)
//
//            val initialJourney = emptyJourney
//              .submitMovementReferenceNumber(mrn)
//              .submitReasonForSecurityAndDeclaration(rfs, decl)
//              .flatMap(_.submitClaimDuplicateCheckStatus(false))
//              .getOrFail
//
//            inSequence {
//              mockAuthWithNoRetrievals()
//              mockGetSession(SessionData(initialJourney))
//              mockStoreSession(SessionData(initialJourney.selectSecurityDepositId(firstDepositId).getOrFail))(Right(()))
//            }
//
//            checkIsRedirect(
//              performAction(firstDepositId, "select-securities" -> "true"),
//              routes.SelectSecuritiesController.show(secondDepositId)
//            )
//          }
//        }
//      }
//
//      "skip the first security deposit and move to the next security" in {
//        forAll(mrnWithNonExportRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
//          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
//
//          whenever(depositIds.size >= 1) {
//            val firstDepositId  = depositIds(0)
//            val secondDepositId = depositIds(1)
//
//            val initialJourney = emptyJourney
//              .submitMovementReferenceNumber(mrn)
//              .submitReasonForSecurityAndDeclaration(rfs, decl)
//              .flatMap(_.submitClaimDuplicateCheckStatus(false))
//              .getOrFail
//
//            inSequence {
//              mockAuthWithNoRetrievals()
//              mockGetSession(SessionData(initialJourney))
//            }
//
//            checkIsRedirect(
//              performAction(firstDepositId, "select-securities" -> "false"),
//              routes.SelectSecuritiesController.show(secondDepositId)
//            )
//          }
//        }
//      }
//
//      "select the last security deposit and move to the check declaration details page" in {
//        forAll(mrnWithNonExportRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
//          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
//
//          whenever(depositIds.size >= 2) {
//            val lastDepositId = depositIds.last
//
//            val initialJourney = emptyJourney
//              .submitMovementReferenceNumber(mrn)
//              .submitReasonForSecurityAndDeclaration(rfs, decl)
//              .flatMap(_.submitClaimDuplicateCheckStatus(false))
//              .getOrFail
//
//            inSequence {
//              mockAuthWithNoRetrievals()
//              mockGetSession(SessionData(initialJourney))
//              mockStoreSession(SessionData(initialJourney.selectSecurityDepositId(lastDepositId).getOrFail))(Right(()))
//            }
//
//            checkIsRedirect(
//              performAction(lastDepositId, "select-securities" -> "true"),
//              routes.CheckDeclarationDetailsController.show()
//            )
//          }
//        }
//      }
//
//      "skip the last security deposit and move to the check declaration details page" in {
//        forAll(mrnWithNonExportRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
//          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
//
//          whenever(depositIds.size >= 1) {
//            val lastDepositId = depositIds.last
//
//            val initialJourney = emptyJourney
//              .submitMovementReferenceNumber(mrn)
//              .submitReasonForSecurityAndDeclaration(rfs, decl)
//              .flatMap(_.submitClaimDuplicateCheckStatus(false))
//              .flatMap(_.selectSecurityDepositIds(depositIds))
//              .getOrFail
//
//            inSequence {
//              mockAuthWithNoRetrievals()
//              mockGetSession(SessionData(initialJourney))
//              mockStoreSession(SessionData(initialJourney.removeSecurityDepositId(lastDepositId).getOrFail))(Right(()))
//            }
//
//            checkIsRedirect(
//              performAction(lastDepositId, "select-securities" -> "false"),
//              routes.CheckDeclarationDetailsController.show()
//            )
//          }
//        }
//      }
//
//      "select the first security deposit and return to the check details page when in change mode" in {
//        forAll(mrnWithNonExportRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
//          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
//
//          whenever(depositIds.size >= 2) {
//            val firstDepositId = depositIds(0)
//
//            val initialJourney = emptyJourney
//              .submitMovementReferenceNumber(mrn)
//              .submitReasonForSecurityAndDeclaration(rfs, decl)
//              .flatMap(_.submitClaimDuplicateCheckStatus(false))
//              .map(_.submitCheckDeclarationDetailsChangeMode(true))
//              .getOrFail
//
//            inSequence {
//              mockAuthWithNoRetrievals()
//              mockGetSession(SessionData(initialJourney))
//              mockStoreSession(SessionData(initialJourney.selectSecurityDepositId(firstDepositId).getOrFail))(Right(()))
//            }
//
//            checkIsRedirect(
//              performAction(firstDepositId, "select-securities" -> "true"),
//              routes.CheckDeclarationDetailsController.show()
//            )
//          }
//        }
//      }
//
//      "de-select the first security deposit and return to the check details page when in change mode" in {
//        forAll(mrnWithNonExportRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
//          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
//
//          whenever(depositIds.nonEmpty) {
//            val firstDepositId = depositIds(0)
//
//            val initialJourney = emptyJourney
//              .submitMovementReferenceNumber(mrn)
//              .submitReasonForSecurityAndDeclaration(rfs, decl)
//              .flatMap(_.submitClaimDuplicateCheckStatus(false))
//              .flatMap(_.selectSecurityDepositIds(depositIds))
//              .map(_.submitCheckDeclarationDetailsChangeMode(true))
//              .getOrFail
//
//            inSequence {
//              mockAuthWithNoRetrievals()
//              mockGetSession(SessionData(initialJourney))
//              mockStoreSession(SessionData(initialJourney.removeSecurityDepositId(firstDepositId).getOrFail))(Right(()))
//            }
//
//            checkIsRedirect(
//              performAction(firstDepositId, "select-securities" -> "false"),
//              routes.CheckDeclarationDetailsController.show()
//            )
//          }
//        }
//      }
//
//    }
//
//  }

}
