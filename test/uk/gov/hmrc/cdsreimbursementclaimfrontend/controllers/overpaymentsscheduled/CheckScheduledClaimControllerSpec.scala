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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import cats.implicits.catsSyntaxOptionId
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.CheckScheduledClaimController.checkClaimSummaryKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.{routes => overpaymentsScheduledRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType.EuDuty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType.UkDuty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType.Wine
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.A30
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.A70
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.B00
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer.dutyTypesOrdering
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer.taxCodesOrdering
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftClaimGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithCorrect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

import scala.collection.immutable.SortedMap
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.OverpaymentsRoutes

class CheckScheduledClaimControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: CheckScheduledClaimController = instanceOf[CheckScheduledClaimController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  "Check Reimbursement Claim Controller" should {

    "redirect to the select duties page" when {
      "no answer provided" in {
        val session = sessionWithAnswer()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          controller.showReimbursements()(FakeRequest()),
          overpaymentsScheduledRoutes.SelectDutyTypesController.showDutyTypes
        )
      }

      "user discontinues and chooses to select correct duties" in {
        val session = sessionWithAnswer()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          controller.submitReimbursements()(FakeRequest().withFormUrlEncodedBody(checkClaimSummaryKey -> "false")),
          overpaymentsScheduledRoutes.SelectDutyTypesController.showDutyTypes
        )
      }
    }

    "show totals" when {
      "duties and tax codes are selected and amounts filled" in {
        val session = sessionWithAnswer(
          SelectedDutyTaxCodesReimbursementAnswer(
            SortedMap(
              UkDuty -> SortedMap(
                A30 -> AmountPaidWithCorrect(paidAmount = 100, correctAmount = 45),
                B00 -> AmountPaidWithCorrect(paidAmount = 89, correctAmount = 3)
              ),
              EuDuty -> SortedMap(A70 -> AmountPaidWithCorrect(paidAmount = 33, correctAmount = 15)),
              Wine   -> SortedMap.empty
            )
          ).some
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.showReimbursements()(FakeRequest()),
          messageFromMessageKey(
            messageKey = s"$checkClaimSummaryKey.scheduled.title"
          ),
          doc => {
            def messageOf(taxCode: TaxCode) =
              messages(s"$checkClaimSummaryKey.duty-code.row.key", messages(s"tax-code.${taxCode.value}"))

            val a70ClaimedFunds   = BigDecimal(33 - 15)
            val a30ClaimedFunds   = BigDecimal(100 - 45)
            val b00ClaimedFunds   = BigDecimal(89 - 3)
            val ukClaimedSubtotal = a30ClaimedFunds + b00ClaimedFunds
            val total             = ukClaimedSubtotal + a70ClaimedFunds

            val summaries = doc.select("dl > div")

            summaries.get(0).select("dt").get(0).html() should be(messageOf(A30))
            summaries.get(0).select("dd").get(0).text() should be(a30ClaimedFunds.toPoundSterlingString)

            summaries.get(1).select("dt").get(0).html() should be(messageOf(B00))
            summaries.get(1).select("dd").get(0).text() should be(b00ClaimedFunds.toPoundSterlingString)

            summaries.get(2).select("dt").get(0).html() should be(
              messages(s"$checkClaimSummaryKey.duty-code.total.key", messages(s"duty-type.${UkDuty.repr}"))
            )
            summaries.get(2).select("dd").get(0).text() should be(ukClaimedSubtotal.toPoundSterlingString)

            summaries.get(3).select("dt").get(0).html() should be(messageOf(A70))
            summaries.get(3).select("dd").get(0).text() should be(a70ClaimedFunds.toPoundSterlingString)

            summaries.get(4).select("dt").get(0).html() should be(messages(s"$checkClaimSummaryKey.total"))
            summaries.get(4).select("dd").get(0).text() should be(total.toPoundSterlingString)
          }
        )
      }
    }

    "continue with the next page" in {
      val session = sessionWithAnswer(
        SelectedDutyTaxCodesReimbursementAnswer(
          SortedMap(UkDuty -> SortedMap(A30 -> AmountPaidWithCorrect(paidAmount = 99, correctAmount = 78)))
        ).some
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockStoreSession(Right(()))
      }

      checkIsRedirect(
        controller.submitReimbursements()(FakeRequest().withFormUrlEncodedBody(checkClaimSummaryKey -> "true")),
        OverpaymentsRoutes.BankAccountController.checkBankAccountDetails(JourneyBindable.Scheduled)
      )
    }

    "continue with Check Your Answers page" in {
      val session = sessionWithClaim(
        sample(genValidDraftClaim(TypeOfClaimAnswer.Scheduled)).copy(
          selectedDutyTaxCodesReimbursementAnswer = Some(
            SelectedDutyTaxCodesReimbursementAnswer(
              SortedMap(UkDuty -> SortedMap(A30 -> AmountPaidWithCorrect(paidAmount = 99, correctAmount = 78)))
            )
          )
        )
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockStoreSession(Right(()))
      }

      checkIsRedirect(
        controller.submitReimbursements()(FakeRequest().withFormUrlEncodedBody(checkClaimSummaryKey -> "true")),
        OverpaymentsRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(JourneyBindable.Scheduled)
      )
    }
  }

  def sessionWithAnswer(
    selectedDutyTaxCodesReimbursementAnswer: Option[SelectedDutyTaxCodesReimbursementAnswer] = None
  ): SessionData =
    sessionWithClaim(
      DraftClaim.blank.copy(
        selectedDutyTaxCodesReimbursementAnswer = selectedDutyTaxCodesReimbursementAnswer
      )
    )

  def sessionWithClaim(c285Claim: DraftClaim): SessionData = {
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    SessionData.empty.copy(journeyStatus = FillingOutClaim(ggCredId, signedInUserDetails, c285Claim).some)
  }
}
