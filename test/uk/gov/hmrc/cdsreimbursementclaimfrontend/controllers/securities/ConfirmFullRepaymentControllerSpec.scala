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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyTestData
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

  val controller: ConfirmFullRepaymentController = instanceOf[ConfirmFullRepaymentController]
  implicit val messagesApi: MessagesApi          = controller.messagesApi
  implicit val messages: Messages                = MessagesImpl(Lang("en"), messagesApi)

  val session: SessionData = SessionData(SecuritiesJourney.empty(exampleEori).submitMovementReferenceNumber(exampleMrn))

  def validateConfirmFullRepaymentPage(
    securityId: String,
    doc: Document,
    journey: SecuritiesJourney,
    isError: Boolean = false
  ) = {
    val title               = doc.select("title").first().text()
    val heading             = doc.select(".govuk-heading-xl").eachText().asScala.toList
    val legend              = doc.select(".govuk-fieldset__legend").eachText().asScala.toList
    val summaryKeys         = doc.select(".govuk-summary-list__key").eachText().asScala.toList
    val summaryValues       = doc.select(".govuk-summary-list__value").eachText().asScala.toList
    val currencyFormatter   = NumberFormat.getCurrencyInstance(Locale.UK)
    val amountPaidFormatted = currencyFormatter.format(
      journey.getSecurityDetailsFor(securityId).value.getTotalAmount
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
      List(journey.getDisplayDeclarationIfValidSecurityDepositId(securityId).value.getMRN.value, amountPaidFormatted)
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

      "display the page on a complete journey" in
        forAll(completeJourneyGen) { journey =>
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          val securityId     = securityIdWithTaxCodes(journey).value
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(securityId),
            messageFromMessageKey(s"$confirmFullRepaymentKey.title", securityId),
            doc => validateConfirmFullRepaymentPage(securityId, doc, journey)
          )
        }

      "redirect to show when first selected security deposit ID is found" in
        forAll(completeJourneyGen) { journey =>
          val securityId = journey.getSelectedDepositIds.head
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(performActionShowFirst, routes.ConfirmFullRepaymentController.show(securityId))
        }

      "redirect to check declaration details when correctedAmounts is None" in {
        val journey        = completeJourneyGen.sample.getOrElse(fail("Failed to create journey"))
        val updatedJourney = SecuritiesJourney.unsafeModifyAnswers(journey, _.copy(correctedAmounts = None))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedJourney))
        }

        checkIsRedirect(performActionShowFirst, routes.CheckDeclarationDetailsController.show)
      }
    }

    "submit page is called" must {
      def performAction(securityId: String, data: Seq[(String, String)]): Future[Result] =
        controller.submit(securityId)(FakeRequest().withFormUrlEncodedBody(data*))

      "redirect to the error page if we have arrived with an invalid security deposit ID" in {
        mrnWithtRfsWithDisplayDeclarationWithoutIPROrENUGen.sample.map { case (mrn, rfs, decl) =>
          val initialJourney = emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(a => a.submitDeclarantEoriNumber(decl.getDeclarantEori))
            .flatMap(a => a.submitConsigneeEoriNumber(decl.getConsigneeEori.value))
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          checkIsTechnicalErrorPage(performAction("anySecurityId", Seq.empty))

        }
      }

      "move on to /check-claim page when yes is selected and there are no other security ids" in {
        forAll(mrnWithRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
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
            val journey: SecuritiesJourney                                             =
              emptyJourney
                .submitMovementReferenceNumber(mrn)
                .submitReasonForSecurityAndDeclaration(rfs, decl)
                .flatMap(a => a.submitDeclarantEoriNumber(decl.getDeclarantEori))
                .flatMap(a => a.submitConsigneeEoriNumber(decl.getConsigneeEori.value))
                .flatMap(_.submitClaimDuplicateCheckStatus(false))
                .flatMap(_.selectSecurityDepositIds(depositIds))
                .flatMapEach(
                  reclaimsBySecurityDepositId,
                  (journey: SecuritiesJourney) =>
                    (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                      journey
                        .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                )
                .getOrFail
            val securityId                                                             = journey.getSelectedDepositIds.last
            val sessionData                                                            = SessionData(journey)
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
          completeJourneyGen.map(journey =>
            SecuritiesJourney.unsafeModifyAnswers(
              journey,
              answers =>
                answers.copy(
                  modes = answers.modes.copy(checkClaimDetailsChangeMode = false),
                  correctedAmounts = answers.correctedAmounts.map(
                    _.map((id, _) => (id, SortedMap.empty[TaxCode, Option[BigDecimal]]))
                  )
                )
            )
          )
        ) { journey =>
          whenever(journey.getSelectedDepositIds.size > 1) {

            val firstSecurityId       = journey.getSelectedDepositIds.head
            val nextSelectedDepositId = journey.getSelectedDepositIds.nextAfter(firstSecurityId).get
            val updatedJourney        = journey.submitFullCorrectedAmounts(firstSecurityId).getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
              mockStoreSession(SessionData(updatedJourney))(Right(()))
            }

            checkIsRedirect(
              performAction(firstSecurityId, Seq(confirmFullRepaymentKey -> "true")),
              routes.ConfirmFullRepaymentController.show(nextSelectedDepositId)
            )
          }
        }

      "move on to /select-duties/:securityID page when no is selected and continue is clicked" in {
        forAll(mrnIncludingExportRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
          val depositIds: Seq[String] = reclaims.map(_._1).distinct

          val journey: SecuritiesJourney =
            emptyJourney
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(a => a.submitDeclarantEoriNumber(decl.getDeclarantEori))
              .flatMap(a => a.submitConsigneeEoriNumber(decl.getConsigneeEori.value))
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositIds(depositIds))
              .getOrFail

          val securityId = journey.getSecurityDepositIds.head

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(SessionData(journey.submitClaimFullAmountMode(false)))(Right(()))
          }

          checkIsRedirect(
            performAction(securityId, Seq(confirmFullRepaymentKey -> "false")),
            routes.SelectDutiesController.show(securityId)
          )
        }

      }

      "Complete journey - clicking continue with no option selected should display error" in {
        forAll(
          buildCompleteJourneyGen(
            submitFullAmount = true
          )
        ) { journey =>
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          val securityId     = securityIdWithTaxCodes(journey).value
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(securityId, Seq()),
            messageFromMessageKey(s"$confirmFullRepaymentKey.title", securityId),
            doc => validateConfirmFullRepaymentPage(securityId, doc, journey, isError = true),
            400
          )
        }
      }

      "From CYA page, change from 'Yes' to 'No', clicking continue should go to the select duties controller" in {
        forAll(
          buildCompleteJourneyGen(
            submitFullAmount = true
          )
        ) { journey =>
          val securityId = journey.getSelectedDepositIds.head
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(SessionData(journey.submitClaimFullAmountMode(false)))(Right(()))
          }

          checkIsRedirect(
            performAction(securityId, Seq(confirmFullRepaymentKey -> "false")),
            routes.SelectDutiesController.show(securityId)
          )
        }
      }

      "selecting NO, going back from CYA, changing to YES and clicking continue should redirect back to CYA" in {
        forAll(
          buildCompleteJourneyGen(
            submitFullAmount = false
          )
        ) { journey =>
          for securityId <- journey.getSelectedDepositIds do {
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
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
          buildCompleteJourneyGen(
            submitFullAmount = false
          )
        ) { journey =>
          for securityId <- journey.getSelectedDepositIds do {
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
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
          buildCompleteJourneyGen(
            submitFullAmount = true,
            reasonsForSecurity = ReasonForSecurity.values - ReasonForSecurity.InwardProcessingRelief
          )
        ) { journey =>
          for securityId <- journey.getSelectedDepositIds do {
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
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
          completeJourneyGen.map(journey =>
            SecuritiesJourney.unsafeModifyAnswers(
              journey,
              answers =>
                answers.copy(
                  modes = answers.modes.copy(checkClaimDetailsChangeMode = false, checkYourAnswersChangeMode = false)
                )
            )
          )
        ) { journey =>
          whenever(journey.getSelectedDepositIds.size > 1) {

            val firstSecurityId = journey.getSelectedDepositIds.head
            val updatedJourney  = journey.submitFullCorrectedAmounts(firstSecurityId).getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
              mockStoreSession(SessionData(updatedJourney))(Right(()))
            }

            checkIsRedirect(
              performAction(firstSecurityId, Seq(confirmFullRepaymentKey -> "false")),
              routes.CheckClaimDetailsController.show
            )
          }
        }

      "redirect to enter claim and select a single tax code when only one exists for the security deposit ID and no is selected" in
        forAll(
          partialGenSingleDuty.map(journey =>
            SecuritiesJourney.unsafeModifyAnswers(
              journey,
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
        ) { journey =>

          val firstSecurityId = journey.getSelectedDepositIds.head
          val updatedJourney  = journey
            .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(
              firstSecurityId,
              journey.getSecurityTaxCodesFor(firstSecurityId)
            )
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
            mockStoreSession(SessionData(updatedJourney))(Right(()))
          }

          checkIsRedirect(
            performAction(firstSecurityId, Seq(confirmFullRepaymentKey -> "false")),
            routes.EnterClaimController.showFirst(firstSecurityId)
          )
        }

      "redirect to ineligible page when selecting a single duty fails and no is selected" in {
        val initialJourney  = partialGenSingleDuty.sample.getOrElse(fail("Failed to create journey"))
        val firstSecurityId = initialJourney.getSelectedDepositIds.head

        val journey = SecuritiesJourney.unsafeModifyAnswers(
          initialJourney,
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
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction(firstSecurityId, Seq(confirmFullRepaymentKey -> "false")),
          baseRoutes.IneligibleController.ineligible
        )
      }
    }
  }
}
