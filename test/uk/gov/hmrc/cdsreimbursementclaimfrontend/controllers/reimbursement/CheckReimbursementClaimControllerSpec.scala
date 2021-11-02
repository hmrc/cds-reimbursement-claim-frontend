package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement

import cats.implicits.catsSyntaxOptionId
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable.Scheduled
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.CheckReimbursementClaimController.checkClaimSummaryKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType.{EuDuty, UkDuty, Wine}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.{A30, A70, B00}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer.{dutyTypesOrdering, taxCodesOrdering}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BigDecimalOps, DraftClaim, Reimbursement, SessionData, SignedInUserDetails, TaxCode}

import scala.collection.immutable.SortedMap

class CheckReimbursementClaimControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: CheckReimbursementClaimController = instanceOf[CheckReimbursementClaimController]

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
          routes.SelectDutyTypesController.showDutyTypes()
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
          routes.SelectDutyTypesController.showDutyTypes()
        )
      }
    }

    "show totals" when {
      "duties and tax codes are selected and amounts filled" in {
        val session = sessionWithAnswer(
          SelectedDutyTaxCodesReimbursementAnswer(
            SortedMap(
              UkDuty -> SortedMap(
                A30 -> Reimbursement(paidAmount = 100, shouldOfPaid = 45),
                B00 -> Reimbursement(paidAmount = 89, shouldOfPaid = 3)
              ),
              EuDuty -> SortedMap(A70 -> Reimbursement(paidAmount = 33, shouldOfPaid = 15)),
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
            messageKey = s"$checkClaimSummaryKey.title"
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

            summaries.get(0).select("dt").get(0).text() should be(messageOf(A30))
            summaries.get(0).select("dd").get(0).text() should be(a30ClaimedFunds.toPoundSterlingString)

            summaries.get(1).select("dt").get(0).text() should be(messageOf(B00))
            summaries.get(1).select("dd").get(0).text() should be(b00ClaimedFunds.toPoundSterlingString)

            summaries.get(2).select("dt").get(0).text() should be(
              messages(s"$checkClaimSummaryKey.duty-code.total.key", messages(s"duty-type.${UkDuty.repr}"))
            )
            summaries.get(2).select("dd").get(0).text() should be(ukClaimedSubtotal.toPoundSterlingString)

            summaries.get(3).select("dt").get(0).text() should be(messageOf(A70))
            summaries.get(3).select("dd").get(0).text() should be(a70ClaimedFunds.toPoundSterlingString)

            summaries.get(4).select("dt").get(0).text() should be(messages(s"$checkClaimSummaryKey.total"))
            summaries.get(4).select("dd").get(0).text() should be(total.toPoundSterlingString)
          }
        )
      }
    }

    "continue with the next page" in {
      val session = sessionWithAnswer(
        SelectedDutyTaxCodesReimbursementAnswer(
          SortedMap(UkDuty -> SortedMap(A30 -> Reimbursement(paidAmount = 99, shouldOfPaid = 78)))
        ).some
      )

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockStoreSession(Right(()))
      }

      checkIsRedirect(
        controller.submitReimbursements()(FakeRequest().withFormUrlEncodedBody(checkClaimSummaryKey -> "true")),
        claimsRoutes.BankAccountController.checkBankAccountDetails(Scheduled)
      )
    }
  }

  def sessionWithAnswer(
    selectedDutyTaxCodesReimbursementAnswer: Option[SelectedDutyTaxCodesReimbursementAnswer] = None
  ): SessionData = {
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val draftC285Claim      = DraftClaim.blank.copy(
      selectedDutyTaxCodesReimbursementAnswer = selectedDutyTaxCodesReimbursementAnswer
    )
    SessionData.empty.copy(journeyStatus = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim).some)
  }
}
