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
import org.scalacheck.ShrinkLowPriority
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.BeforeAndAfterEach
import org.scalatest.OptionValues
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.securityIdWithTaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.MissingPreferenceCertificate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.TemporaryAdmission3M
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SummaryInspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging

import java.text.NumberFormat
import java.util.Locale
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class ConfirmSingleDepositRepaymentControllerSpec
    extends PropertyBasedControllerSpec
    with SecuritiesJourneyTestData
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryInspectionAddress
    with SummaryMatchers
    with TypeCheckedTripleEquals
    with OptionValues
    with ShrinkLowPriority
    with Logging {

  val confirmFullRepaymentKey: String = "confirm-full-repayment"

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: ConfirmSingleDepositRepaymentController = instanceOf[ConfirmSingleDepositRepaymentController]
  implicit val messagesApi: MessagesApi                   = controller.messagesApi
  implicit val messages: Messages                         = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = {
    featureSwitch.enable(Feature.Securities)
    featureSwitch.disable(Feature.LimitedAccessSecurities)
  }

  val session: SessionData = SessionData(SecuritiesJourney.empty(exampleEori).submitMovementReferenceNumber(exampleMrn))

  private val incompleteJourney = buildJourneyGen(
    acc14ConsigneeMatchesUserEori = true,
    submitBankAccountDetails = false,
    submitBankAccountType = false,
    submitDeclarantDetails = false,
    numberOfSecurityDetails = Some(1)
  ).map(
    _.fold(
      error =>
        throw new Exception(
          s"Cannnot build complete SecuritiesJourney because of $error, fix the test data generator."
        ),
      identity
    )
  )

  private val incompleteJourneyNtas = buildJourneyGen(
    acc14ConsigneeMatchesUserEori = true,
    submitBankAccountDetails = false,
    submitBankAccountType = false,
    submitDeclarantDetails = false,
    numberOfSecurityDetails = Some(1),
    reasonsForSecurity = Set(TemporaryAdmission3M)
  ).map(
    _.fold(
      error =>
        throw new Exception(
          s"Cannnot build complete SecuritiesJourney because of $error, fix the test data generator."
        ),
      identity
    )
  )

  private val incompleteJourneyNidac = buildJourneyGen(
    acc14ConsigneeMatchesUserEori = true,
    submitBankAccountDetails = false,
    submitBankAccountType = false,
    submitDeclarantDetails = false,
    numberOfSecurityDetails = Some(1),
    reasonsForSecurity = Set(MissingPreferenceCertificate)
  ).map(
    _.fold(
      error =>
        throw new Exception(
          s"Cannnot build complete SecuritiesJourney because of $error, fix the test data generator."
        ),
      identity
    )
  )

  def validateConfirmSingleDepositRepaymentPage(
    securityId: String,
    doc: Document,
    journey: SecuritiesJourney,
    isError: Boolean = false
  ) = {
    val title             = doc.select("title").first().text()
    val heading           = doc.select(".govuk-heading-l").eachText().asScala.toList
    val legend            = doc.select(".govuk-fieldset__legend").eachText().asScala.toList
    val summaryKeys       = doc.select(".govuk-summary-list__key").eachText().asScala.toList
    val summaryValues     = doc.select(".govuk-summary-list__value").eachText().asScala.toList
    val currencyFormatter = NumberFormat.getCurrencyInstance(Locale.UK)

    val taxDetails = journey.getSecurityDetails.head.taxDetails.map(td =>
      messages(s"tax-code.${td.taxType}")
    ) + "Total security deposit or guarantee amount"
    val taxValues  = journey.getSecurityDetails.head.taxDetails.map(td =>
      currencyFormatter.format(td.getAmount)
    ) + currencyFormatter.format(journey.getSecurityDetails.head.getTotalAmount)

    title           should ===(
      (if isError then "Error: "
       else "") + "Tell us what you’re claiming - Claim back import duty and VAT - GOV.UK"
    )
    heading         should ===(
      List(
        "Tell us what you’re claiming"
      )
    )
    summaryKeys     should ===(taxDetails)
    summaryValues   should ===(taxValues)
    legend          should ===(List("Do you want to claim back the full amount?"))
    radioItems(doc) should contain theSameElementsAs Seq(
      ("Yes", "true"),
      ("No", "false")
    )
    hasContinueButton(doc)
  }

  "Confirm Single Deposit Repayment Controller" when {
    "show page is called" must {
      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page on a complete journey" in
        forAll(buildCompleteJourneyGen(numberOfSecurityDetails = Some(1))) { journey =>
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          val securityId     = securityIdWithTaxCodes(journey).value
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirm-full-repayment.single-deposit-id.title"),
            doc => validateConfirmSingleDepositRepaymentPage(securityId, doc, journey)
          )
        }
    }

    "submit page is called" must {
      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data*))

      "not succeed if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction(Seq.empty)) shouldBe NOT_FOUND
      }

      "continue to choose export method page when yes is selected and reason for security is NTAS" in {
        forAll(incompleteJourneyNtas) { journey =>
          val sessionData = SessionData(journey)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(Right(()))
          }

          checkIsRedirect(
            performAction(Seq(confirmFullRepaymentKey -> "true")),
            routes.ChooseExportMethodController.show
          )
        }
      }

      "continue to choose payee type page when yes is selected and reason for security is NIDAC" in {
        forAll(incompleteJourneyNidac) { journey =>
          val sessionData = SessionData(journey)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(Right(()))
          }

          checkIsRedirect(
            performAction(Seq(confirmFullRepaymentKey -> "true")),
            routes.ChoosePayeeTypeController.show
          )
        }
      }

      "continue to partial claims page when no is selected" in {
        forAll(incompleteJourney) { journey =>
          val sessionData = SessionData(journey)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(Right(()))
          }

          checkIsRedirect(
            performAction(Seq(confirmFullRepaymentKey -> "false")),
            routes.PartialClaimsController.show
          )
        }
      }

      "display error when no option selected" in {
        forAll(incompleteJourney) { journey =>
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          val securityId     = journey.getSecurityDetails.head.securityDepositId
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(Seq()),
            messageFromMessageKey("confirm-full-repayment.single-deposit-id.title"),
            doc => validateConfirmSingleDepositRepaymentPage(securityId, doc, journey, isError = true),
            400
          )
        }
      }

      "redirect back to CYA when user has come from CYA and yes is selected" in {
        forAll(
          buildCompleteJourneyGen(numberOfSecurityDetails = Some(1))
        ) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(Right(()))
          }

          checkIsRedirect(
            performAction(Seq(confirmFullRepaymentKey -> "true")),
            routes.CheckYourAnswersController.show
          )
        }
      }
    }
  }
}
