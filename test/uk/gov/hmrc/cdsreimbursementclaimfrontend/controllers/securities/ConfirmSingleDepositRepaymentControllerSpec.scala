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
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.securityIdWithTaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.MissingPreferenceCertificate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.TemporaryAdmission3M
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SecuritiesClaimModes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SummaryInspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging

import java.text.NumberFormat
import java.util.Locale
import scala.collection.immutable.SortedMap
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class ConfirmSingleDepositRepaymentControllerSpec
    extends PropertyBasedControllerSpec
    with SecuritiesClaimTestData
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

  val session: SessionData = SessionData(SecuritiesClaim.empty(exampleEori).submitMovementReferenceNumber(exampleMrn))

  private val incompleteClaim = buildClaimGen(
    acc14ConsigneeMatchesUserEori = true,
    submitBankAccountDetails = false,
    submitBankAccountType = false,
    submitDeclarantDetails = false,
    numberOfSecurityDetails = Some(1)
  ).map(
    _.fold(
      error =>
        throw new Exception(
          s"Cannnot build complete SecuritiesClaim because of $error, fix the test data generator."
        ),
      identity
    )
  )

  private val incompleteClaimNtas = buildClaimGen(
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
          s"Cannnot build complete SecuritiesClaim because of $error, fix the test data generator."
        ),
      identity
    )
  )

  private val incompleteClaimNidac = buildClaimGen(
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
          s"Cannnot build complete SecuritiesClaim because of $error, fix the test data generator."
        ),
      identity
    )
  )

  def validateConfirmSingleDepositRepaymentPage(
    securityId: String,
    doc: Document,
    claim: SecuritiesClaim,
    isError: Boolean = false
  ) = {
    val title             = doc.select("title").first().text()
    val heading           = doc.select(".govuk-heading-l").eachText().asScala.toList
    val legend            = doc.select(".govuk-fieldset__legend").eachText().asScala.toList
    val summaryKeys       = doc.select(".govuk-summary-list__key").eachText().asScala.toList
    val summaryValues     = doc.select(".govuk-summary-list__value").eachText().asScala.toList
    val currencyFormatter = NumberFormat.getCurrencyInstance(Locale.UK)

    val taxDetails = claim.getSecurityDetails.head.taxDetails.map(td =>
      messages(s"tax-code.${td.taxType}")
    ) + "Total security deposit or guarantee amount"
    val taxValues  = claim.getSecurityDetails.head.taxDetails.map(td =>
      currencyFormatter.format(td.getAmount)
    ) + currencyFormatter.format(claim.getSecurityDetails.head.getTotalAmount)

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

      "display the page on a complete claim" in
        forAll(buildCompleteClaimGen(numberOfSecurityDetails = Some(1))) { claim =>
          val updatedSession = SessionData.empty.copy(securitiesClaim = Some(claim))
          val securityId     = securityIdWithTaxCodes(claim).value
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirm-full-repayment.single-deposit-id.title"),
            doc => validateConfirmSingleDepositRepaymentPage(securityId, doc, claim)
          )
        }
    }

    "submit page is called" must {
      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data*))

      "continue to choose export method page when yes is selected and reason for security is NTAS" in {
        forAll(incompleteClaimNtas) { claim =>
          val sessionData = SessionData(claim)
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
        forAll(incompleteClaimNidac) { claim =>
          val sessionData = SessionData(claim)
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
        forAll(incompleteClaim) { claim =>
          val sessionData = SessionData(claim)
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

      "redirect back to check repayment total when no is selected and was the previous selection" in {
        val claim = SecuritiesClaim.unsafeModifyAnswers(
          completeClaimGen.sample.get,
          _.copy(
            modes = SecuritiesClaimModes(
              checkYourAnswersChangeMode = false,
              checkClaimDetailsChangeMode = true,
              claimFullAmountMode = false
            )
          )
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performAction(Seq(confirmFullRepaymentKey -> "false")),
          routes.CheckClaimDetailsSingleSecurityController.show
        )
      }

      "display error when no option selected" in {
        forAll(incompleteClaim) { claim =>
          val updatedSession = SessionData.empty.copy(securitiesClaim = Some(claim))
          val securityId     = claim.getSecurityDetails.head.securityDepositId
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(Seq()),
            messageFromMessageKey("confirm-full-repayment.single-deposit-id.title"),
            doc => validateConfirmSingleDepositRepaymentPage(securityId, doc, claim, isError = true),
            400
          )
        }
      }

      "redirect back to CYA when user has come from CYA and yes is selected" in {
        forAll(
          buildCompleteClaimGen(numberOfSecurityDetails = Some(1))
        ) { claim =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
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
