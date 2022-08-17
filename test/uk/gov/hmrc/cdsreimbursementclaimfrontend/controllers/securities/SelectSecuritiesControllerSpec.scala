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
import play.api.test.Helpers.status
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.EitherOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils

import scala.collection.JavaConverters._
import scala.concurrent.Future

class SelectSecuritiesControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryMatchers
    with TestWithJourneyGenerator[SecuritiesJourney] {

  val mockClaimsService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService)
    )

  val controller: SelectSecuritiesController = instanceOf[SelectSecuritiesController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "select-securities"

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  def validateSelectSecuritiesPage(
    doc: Document,
    journey: SecuritiesJourney,
    securityDepositId: String
  ) = {
    val headers       = doc.select("h2.govuk-heading-m").eachText().asScala
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

    headers       should not be empty
    summaryKeys   should not be empty
    summaryValues should not be empty

    journey.getSecurityDetailsFor(securityDepositId).foreach { securityDetails =>
      summaries should containOnlyDefinedPairsOf(
        Seq(
          ("Import MRN"                   -> journey.getLeadMovementReferenceNumber
            .map(_.value)),
          ("Reason for security deposit"  -> journey.answers.reasonForSecurity
            .map(rfs => messages(s"choose-reason-for-security.securities.${ReasonForSecurity.keyOf(rfs)}"))),
          ("Total security deposit value" -> Some(securityDetails.getTotalAmount.toPoundSterlingString)),
          ("Security deposit paid"        -> Some(securityDetails.getPaidAmount.toPoundSterlingString)),
          ("Payment reference"            -> Some(securityDetails.paymentReference)),
          ("Payment method"               -> Some(
            if (securityDetails.isGuaranteeEligible) "Guarantee" else "Bank account"
          )),
          ("Acceptance date"              -> journey.getLeadDisplayDeclaration
            .flatMap(d => DateUtils.displayFormat(d.displayResponseDetail.acceptanceDate))),
          ("Date duty collected"          -> journey.getLeadDisplayDeclaration
            .flatMap(d => DateUtils.displayFormat(d.displayResponseDetail.btaDueDate)))
        )
      )
    }
  }

  "SelectSecuritiesController" when {

    "show page" must {

      def performAction(id: String): Future[Result] = controller.show(id)(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction("foo")) shouldBe NOT_FOUND
      }

      "redirect to the ineligible page if an invalid security deposit ID" in forSomeWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyReadyForSelectingSecurities
        )
      ) { case (initialJourney, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkIsRedirect(
          performAction("foo-123-xyz"),
          baseRoutes.IneligibleController.ineligible()
        )
      }

      "display the page if a valid security deposit ID" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyReadyForSelectingSecurities
        )
      ) { case (initialJourney, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        for (depositId <- depositIds) {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          checkPageIsDisplayed(
            performAction(depositId),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateSelectSecuritiesPage(doc, initialJourney, depositId)
          )
        }
      }

    }

    "submitting securities selection" must {

      def performAction(id: String, data: (String, String)*): Future[Result] =
        controller.submit(id)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "select the first security deposit and move to the next security" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyReadyForSelectingSecurities
        )
      ) { case (initialJourney, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        whenever(depositIds.size >= 2) {
          val firstDepositId  = depositIds(0)
          val secondDepositId = depositIds(1)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(initialJourney.selectSecurityDepositId(firstDepositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(firstDepositId, "select-securities" -> "true"),
            routes.SelectSecuritiesController.show(secondDepositId)
          )
        }

      }

      "skip the first security deposit and move to the next security" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyReadyForSelectingSecurities
        )
      ) { case (initialJourney, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        whenever(depositIds.size >= 1) {
          val firstDepositId  = depositIds(0)
          val secondDepositId = depositIds(1)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          checkIsRedirect(
            performAction(firstDepositId, "select-securities" -> "false"),
            routes.SelectSecuritiesController.show(secondDepositId)
          )
        }

      }

      "select the last security deposit and move to the check declaration details page" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyReadyForSelectingSecurities
        )
      ) { case (initialJourney, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        whenever(depositIds.size >= 2) {
          val lastDepositId = depositIds.last

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(initialJourney.selectSecurityDepositId(lastDepositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(lastDepositId, "select-securities" -> "true"),
            routes.CheckDeclarationDetailsController.show()
          )
        }
      }

      "skip the last security deposit and move to the check declaration details page" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelected
        )
      ) { case (initialJourney, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        whenever(depositIds.size >= 1) {
          val lastDepositId = depositIds.last

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(initialJourney.removeSecurityDepositId(lastDepositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(lastDepositId, "select-securities" -> "false"),
            routes.CheckDeclarationDetailsController.show()
          )
        }
      }

      "select the first security deposit and return to the check details page when in change mode" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyInChangeDeclarationDetailsMode
        )
      ) { case (initialJourney, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        whenever(depositIds.size >= 2) {
          val firstDepositId = depositIds(0)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(initialJourney.selectSecurityDepositId(firstDepositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(firstDepositId, "select-securities" -> "true"),
            routes.CheckDeclarationDetailsController.show()
          )
        }

      }

      "de-select the last security deposit and return to the check details page when in change mode" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyInChangeDeclarationDetailsMode
        )
      ) { case (initialJourney, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        whenever(depositIds.nonEmpty) {
          val depositId = depositIds.last

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(initialJourney.removeSecurityDepositId(depositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(depositId, "select-securities" -> "false"),
            routes.CheckDeclarationDetailsController.show()
          )
        }

      }

      "after de-selecting any security redirect back to the CYA page when in change your answers mode" in forAll(
        completeJourneyGen
      ) { initialJourney =>
        val selected = initialJourney.getSelectedDepositIds
        for (depositId <- selected) {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(initialJourney.removeSecurityDepositId(depositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(depositId, "select-securities" -> "false"),
            routes.CheckYourAnswersController.show()
          )
        }
      }

      "after selecting any missing security redirect back to the CYA page when in change your answers mode" in forAll(
        completeJourneyGen
      ) { initialJourney =>
        val unselected = initialJourney.getSecurityDepositIds.toSet -- initialJourney.getSelectedDepositIds.toSet
        for (depositId <- unselected) {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(initialJourney.selectSecurityDepositId(depositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(depositId, "select-securities" -> "true"),
            routes.CheckYourAnswersController.show()
          )
        }
      }

      "after selecting any missing security ignore change declaration details mode and redirect back to the CYA page when in change your answers mode" in forAll(
        completeJourneyGen
      ) { initialJourney =>
        val unselected = initialJourney.getSecurityDepositIds.toSet -- initialJourney.getSelectedDepositIds.toSet
        for (depositId <- unselected) {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney.submitCheckDeclarationDetailsChangeMode(true)))
            mockStoreSession(
              SessionData(
                initialJourney
                  .submitCheckDeclarationDetailsChangeMode(true)
                  .selectSecurityDepositId(depositId)
                  .getOrFail
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(depositId, "select-securities" -> "true"),
            routes.CheckYourAnswersController.show()
          )
        }
      }

    }

  }

}
