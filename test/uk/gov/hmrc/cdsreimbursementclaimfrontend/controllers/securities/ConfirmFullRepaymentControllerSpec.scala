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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.partialGenSingleDuty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.securityIdWithTaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SummaryInspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes

import java.text.NumberFormat
import java.util.Locale
import scala.collection.immutable.SortedMap
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class ConfirmFullRepaymentControllerSpec
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

  val controller: ConfirmFullRepaymentController = instanceOf[ConfirmFullRepaymentController]
  implicit val messagesApi: MessagesApi          = controller.messagesApi
  implicit val messages: Messages                = MessagesImpl(Lang("en"), messagesApi)

  val session: SessionData = SessionData(SecuritiesClaim.empty(exampleEori).submitMovementReferenceNumber(exampleMrn))

  def validateConfirmFullRepaymentPage(
    securityId: String,
    doc: Document,
    claim: SecuritiesClaim,
    isError: Boolean = false
  ) = {
    val title               = doc.select("title").first().text()
    val heading             = doc.select(".govuk-heading-xl").eachText().asScala.toList
    val legend              = doc.select(".govuk-fieldset__legend").eachText().asScala.toList
    val summaryKeys         = doc.select(".govuk-summary-list__key").eachText().asScala.toList
    val summaryValues       = doc.select(".govuk-summary-list__value").eachText().asScala.toList
    val currencyFormatter   = NumberFormat.getCurrencyInstance(Locale.UK)
    val amountPaidFormatted = currencyFormatter.format(
      claim.getSecurityDetailsFor(securityId).value.getTotalAmount
    )
    title           should ===(
      (if isError then "Error: "
       else "") + s"Claiming back security deposit ID: $securityId - Claim back import duty and VAT - GOV.UK"
    )
    heading         should ===(
      List(
        s"Claiming back security deposit ID: $securityId"
      )
    )
    summaryKeys     should ===(List("Movement Reference Number (MRN)", "Security deposit"))
    summaryValues   should ===(
      List(claim.getImportDeclarationIfValidSecurityDepositId(securityId).value.getMRN.value, amountPaidFormatted)
    )
    legend          should ===(List("Do you want to claim back the full amount?"))
    radioItems(doc) should contain theSameElementsAs Seq(
      ("Yes", "true"),
      ("No", "false")
    )
    hasContinueButton(doc)
  }

  "Confirm Full Repayment Controller" when {
    "show page is called" must {
      def performAction(securityId: String): Future[Result] = controller.show(securityId)(FakeRequest())
      def performActionShowFirst: Future[Result]            = controller.showFirst()(FakeRequest())

      "display the page on a complete claim" in
        forAll(completeClaimGen) { claim =>
          val updatedSession = SessionData.empty.copy(securitiesClaim = Some(claim))
          val securityId     = securityIdWithTaxCodes(claim).value
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(securityId),
            messageFromMessageKey(s"$confirmFullRepaymentKey.title", securityId),
            doc => validateConfirmFullRepaymentPage(securityId, doc, claim)
          )
        }

      "redirect to show when first selected security deposit ID is found" in
        forAll(completeClaimGen) { claim =>
          val securityId = claim.getSelectedDepositIds.head
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(performActionShowFirst, routes.ConfirmFullRepaymentController.show(securityId))
        }

      "redirect to check declaration details when correctedAmounts is None" in {
        val claim        = completeClaimGen.sample.getOrElse(fail("Failed to create claim"))
        val updatedClaim = SecuritiesClaim.unsafeModifyAnswers(claim, _.copy(correctedAmounts = None))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedClaim))
        }

        checkIsRedirect(performActionShowFirst, routes.CheckDeclarationDetailsController.show)
      }
    }

    "submit page is called" must {
      def performAction(securityId: String, data: Seq[(String, String)]): Future[Result] =
        controller.submit(securityId)(FakeRequest().withFormUrlEncodedBody(data*))

      "redirect to the error page if we have arrived with an invalid security deposit ID" in {
        mrnWithtRfsWithImportDeclarationWithoutIPROrENUGen.sample.map { case (mrn, rfs, decl) =>
          val initialClaim = emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(a => a.submitDeclarantEoriNumber(decl.getDeclarantEori))
            .flatMap(a => a.submitConsigneeEoriNumber(decl.getConsigneeEori.value))
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkIsTechnicalErrorPage(performAction("anySecurityId", Seq.empty))

        }
      }

      "move on to /check-claim page when yes is selected and there are no other security ids" in {
        forAll(mrnWithRfsWithImportDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
          whenever(
            Set[ReasonForSecurity](UKAPEntryPrice, OutwardProcessingRelief, RevenueDispute, ManualOverrideDeposit)
              .contains(rfs)
          ) {
            val depositIds: Seq[String]                                                = reclaims.map(_._1).take(1)
            val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
              reclaims
                .groupBy(_._1)
                .view
                .filterKeys(depositIds.contains)
                .view
                .mapValues(_.map { case (_, tc, _, amount) => (tc, amount) })
                .toSeq
            val claim: SecuritiesClaim                                                 =
              emptyClaim
                .submitMovementReferenceNumber(mrn)
                .submitReasonForSecurityAndDeclaration(rfs, decl)
                .flatMap(a => a.submitDeclarantEoriNumber(decl.getDeclarantEori))
                .flatMap(a => a.submitConsigneeEoriNumber(decl.getConsigneeEori.value))
                .flatMap(_.submitClaimDuplicateCheckStatus(false))
                .flatMap(_.selectSecurityDepositIds(depositIds))
                .flatMapEach(
                  reclaimsBySecurityDepositId,
                  (claim: SecuritiesClaim) =>
                    (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                      claim
                        .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                )
                .getOrFail
            val securityId                                                             = claim.getSelectedDepositIds.last
            val sessionData                                                            = SessionData(claim)
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(sessionData)
              mockStoreSession(Right(()))
            }

            checkIsRedirect(
              performAction(securityId, Seq(confirmFullRepaymentKey -> "true")),
              routes.CheckClaimDetailsController.show
            )
          }
        }
      }

      "move on to confirm full repayment for the next security ID when yes is selected and there are other security IDs to confirm" in
        forAll(
          completeClaimGen.map(claim =>
            SecuritiesClaim.unsafeModifyAnswers(
              claim,
              answers =>
                answers.copy(
                  modes = answers.modes.copy(checkClaimDetailsChangeMode = false),
                  correctedAmounts = answers.correctedAmounts.map(
                    _.map((id, _) => (id, SortedMap.empty[TaxCode, Option[BigDecimal]]))
                  )
                )
            )
          )
        ) { claim =>
          whenever(claim.getSelectedDepositIds.size > 1) {

            val firstSecurityId       = claim.getSelectedDepositIds.head
            val nextSelectedDepositId = claim.getSelectedDepositIds.nextAfter(firstSecurityId).get
            val updatedClaim          = claim.submitFullCorrectedAmounts(firstSecurityId).getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(claim))
              mockStoreSession(SessionData(updatedClaim))(Right(()))
            }

            checkIsRedirect(
              performAction(firstSecurityId, Seq(confirmFullRepaymentKey -> "true")),
              routes.ConfirmFullRepaymentController.show(nextSelectedDepositId)
            )
          }
        }

      "move on to /select-duties/:securityID page when no is selected and continue is clicked" in {
        forAll(mrnIncludingExportRfsWithImportDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
          val depositIds: Seq[String] = reclaims.map(_._1).distinct

          val claim: SecuritiesClaim =
            emptyClaim
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(a => a.submitDeclarantEoriNumber(decl.getDeclarantEori))
              .flatMap(a => a.submitConsigneeEoriNumber(decl.getConsigneeEori.value))
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositIds(depositIds))
              .getOrFail

          val securityId = claim.getSecurityDepositIds.head

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockStoreSession(SessionData(claim.submitClaimFullAmountMode(false)))(Right(()))
          }

          checkIsRedirect(
            performAction(securityId, Seq(confirmFullRepaymentKey -> "false")),
            routes.SelectDutiesController.show(securityId)
          )
        }

      }

      "Complete claim - clicking continue with no option selected should display error" in {
        forAll(
          buildCompleteClaimGen(
            submitFullAmount = true
          )
        ) { claim =>
          val updatedSession = SessionData.empty.copy(securitiesClaim = Some(claim))
          val securityId     = securityIdWithTaxCodes(claim).value
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(securityId, Seq()),
            messageFromMessageKey(s"$confirmFullRepaymentKey.title", securityId),
            doc => validateConfirmFullRepaymentPage(securityId, doc, claim, isError = true),
            400
          )
        }
      }

      "From CYA page, change from 'Yes' to 'No', clicking continue should go to the select duties controller" in {
        forAll(
          buildCompleteClaimGen(
            submitFullAmount = true
          )
        ) { claim =>
          val securityId = claim.getSelectedDepositIds.head
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockStoreSession(SessionData(claim.submitClaimFullAmountMode(false)))(Right(()))
          }

          checkIsRedirect(
            performAction(securityId, Seq(confirmFullRepaymentKey -> "false")),
            routes.SelectDutiesController.show(securityId)
          )
        }
      }

      "selecting NO, going back from CYA, changing to YES and clicking continue should redirect back to CYA" in {
        forAll(
          buildCompleteClaimGen(
            submitFullAmount = false
          )
        ) { claim =>
          for securityId <- claim.getSelectedDepositIds do {
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(claim))
              mockStoreSession(Right(()))
            }

            val result = performAction(securityId, Seq(confirmFullRepaymentKey -> "true"))

            checkIsRedirect(
              result,
              routes.CheckClaimDetailsController.show
            )
          }
        }
      }

      "selecting NO, going back from CYA, changing nothing and clicking continue should redirect back to CYA" in {
        forAll(
          buildCompleteClaimGen(
            submitFullAmount = false
          )
        ) { claim =>
          for securityId <- claim.getSelectedDepositIds do {
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(claim))
            }

            val result = performAction(securityId, Seq(confirmFullRepaymentKey -> "false"))

            checkIsRedirect(
              result,
              routes.CheckYourAnswersController.show
            )
          }
        }
      }

      "selecting YES, going back from CYA, changing nothing and clicking continue should redirect back to CYA" in {
        forAll(
          buildCompleteClaimGen(
            submitFullAmount = true,
            reasonsForSecurity = ReasonForSecurity.values - ReasonForSecurity.InwardProcessingRelief
          )
        ) { claim =>
          for securityId <- claim.getSelectedDepositIds do {
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(claim))
            }

            val result = performAction(securityId, Seq(confirmFullRepaymentKey -> "true"))

            checkIsRedirect(
              result,
              routes.CheckYourAnswersController.show
            )
          }
        }
      }

      "redirect to check claim details when duties are selected for the current deposit ID and no is selected" in
        forAll(
          completeClaimGen.map(claim =>
            SecuritiesClaim.unsafeModifyAnswers(
              claim,
              answers =>
                answers.copy(
                  modes = answers.modes.copy(checkClaimDetailsChangeMode = false, checkYourAnswersChangeMode = false)
                )
            )
          )
        ) { claim =>
          whenever(claim.getSelectedDepositIds.size > 1) {

            val firstSecurityId = claim.getSelectedDepositIds.head
            val updatedClaim    = claim.submitFullCorrectedAmounts(firstSecurityId).getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(claim))
              mockStoreSession(SessionData(updatedClaim))(Right(()))
            }

            checkIsRedirect(
              performAction(firstSecurityId, Seq(confirmFullRepaymentKey -> "false")),
              routes.CheckClaimDetailsController.show
            )
          }
        }

      "redirect to enter claim and select a single tax code when only one exists for the security deposit ID and no is selected" in
        forAll(
          partialGenSingleDuty.map(claim =>
            SecuritiesClaim.unsafeModifyAnswers(
              claim,
              answers =>
                answers.copy(
                  modes = answers.modes.copy(
                    checkClaimDetailsChangeMode = false,
                    checkYourAnswersChangeMode = false,
                    claimFullAmountMode = false
                  ),
                  correctedAmounts = answers.correctedAmounts.map(
                    _.map((id, _) => (id, SortedMap.empty[TaxCode, Option[BigDecimal]]))
                  )
                )
            )
          )
        ) { claim =>

          val firstSecurityId = claim.getSelectedDepositIds.head
          val updatedClaim    = claim
            .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(
              firstSecurityId,
              claim.getSecurityTaxCodesFor(firstSecurityId)
            )
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockStoreSession(SessionData(updatedClaim))(Right(()))
          }

          checkIsRedirect(
            performAction(firstSecurityId, Seq(confirmFullRepaymentKey -> "false")),
            routes.EnterClaimController.showFirst(firstSecurityId)
          )
        }

      "redirect to ineligible page when selecting a single duty fails and no is selected" in {
        val initialClaim    = partialGenSingleDuty.sample.getOrElse(fail("Failed to create claim"))
        val firstSecurityId = initialClaim.getSelectedDepositIds.head

        val claim = SecuritiesClaim.unsafeModifyAnswers(
          initialClaim,
          answers =>
            answers.copy(
              modes = answers.modes.copy(
                checkClaimDetailsChangeMode = false,
                checkYourAnswersChangeMode = false,
                claimFullAmountMode = false
              ),
              correctedAmounts = None
            )
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performAction(firstSecurityId, Seq(confirmFullRepaymentKey -> "false")),
          baseRoutes.IneligibleController.ineligible
        )
      }
    }
  }
}
