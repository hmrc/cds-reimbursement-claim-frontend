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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.implicits.catsSyntaxOptionId
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterScheduledClaimControllerSpec.formatter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer.dutyTypesOrdering
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer.taxCodesOrdering
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ReimbursementGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.TaxCodeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.genBigDecimal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

import java.text.DecimalFormat
import scala.collection.immutable.SortedMap

class EnterScheduledClaimControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterScheduledClaimController = instanceOf[EnterScheduledClaimController]
  val enterScheduledClaimKey: String            = "enter-scheduled-claim"

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2)

  "Enter Reimbursement Controller" should {

    "redirect to the check reimbursements page" when {
      "no reimbursements to claim" in {
        val (session, _) = sessionWithAnswer()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          controller.iterate()(FakeRequest()),
          routes.CheckScheduledClaimController.showReimbursements()
        )
      }
    }

    "show enter claim amount page" when {
      "user has selected duty and tax codes" in forAll { (duty: DutyType, taxCode: TaxCode) =>
        val (session, _) = sessionWithAnswer(
          SelectedDutyTaxCodesReimbursementAnswer(SortedMap(duty -> SortedMap(taxCode -> Reimbursement.unclaimed))).some
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          controller.iterate()(FakeRequest()),
          routes.EnterScheduledClaimController.enterClaim(duty, taxCode)
        )
      }
    }

    "display already entered amounts" when {
      "user revisits enter claim page again" in forAll {
        (duty: DutyType, taxCode: TaxCode, reimbursement: Reimbursement) =>
          val (session, _) = sessionWithAnswer(
            SelectedDutyTaxCodesReimbursementAnswer(SortedMap(duty -> SortedMap(taxCode -> reimbursement))).some
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            controller.enterClaim(duty, taxCode)(FakeRequest()),
            messageFromMessageKey(
              messageKey = s"$enterScheduledClaimKey.title",
              messages(s"duty-type.${duty.repr}"),
              taxCode.value
            ),
            doc => {
              val elements = doc.select("input")
              BigDecimal(elements.get(0).`val`()) should be(reimbursement.paidAmount)
              BigDecimal(elements.get(1).`val`()) should be(reimbursement.shouldOfPaid)
            }
          )
      }
    }

    "save user defined amounts and ask user to enter next amounts for upcoming reimbursement" in {
      forAll(Gen.oneOf(DutyTypes.custom), Gen.oneOf(DutyTypes.excise), genReimbursement) {
        (customDuty, exciseDuty, reimbursement) =>
          val (session, draftClaim) = sessionWithAnswer(
            SelectedDutyTaxCodesReimbursementAnswer(
              SortedMap(
                customDuty -> SortedMap(customDuty.taxCodes(0) -> Reimbursement.unclaimed),
                exciseDuty -> SortedMap(exciseDuty.taxCodes(0) -> Reimbursement.unclaimed)
              )
            ).some
          )

          val updatedSession: SessionData =
            session.copy(journeyStatus = session.journeyStatus.collect { case fillingOutClaim: FillingOutClaim =>
              fillingOutClaim.copy(
                draftClaim = draftClaim.copy(
                  selectedDutyTaxCodesReimbursementAnswer = SelectedDutyTaxCodesReimbursementAnswer(
                    SortedMap(
                      customDuty -> SortedMap(customDuty.taxCodes(0) -> reimbursement),
                      exciseDuty -> SortedMap(exciseDuty.taxCodes(0) -> Reimbursement.unclaimed)
                    )
                  ).some
                )
              )
            })

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            controller.submitClaim(customDuty, customDuty.taxCodes(0))(
              FakeRequest().withFormUrlEncodedBody(
                Seq(
                  s"$enterScheduledClaimKey.paid-amount"   -> formatter.format(reimbursement.paidAmount),
                  s"$enterScheduledClaimKey.actual-amount" -> formatter.format(reimbursement.shouldOfPaid)
                ): _*
              )
            ),
            routes.EnterScheduledClaimController.enterClaim(exciseDuty, exciseDuty.taxCodes(0))
          )
      }
    }

    "save user defined amounts and redirect to the next page" in {
      forAll(Gen.oneOf(DutyTypes.custom), genReimbursement) { (duty, reimbursement) =>
        val (session, draftClaim) = sessionWithAnswer(
          SelectedDutyTaxCodesReimbursementAnswer(
            SortedMap(duty -> SortedMap(duty.taxCodes(0) -> Reimbursement.unclaimed))
          ).some
        )

        val updatedSession: SessionData =
          session.copy(journeyStatus = session.journeyStatus.collect { case fillingOutClaim: FillingOutClaim =>
            fillingOutClaim.copy(
              draftClaim = draftClaim.copy(
                selectedDutyTaxCodesReimbursementAnswer = SelectedDutyTaxCodesReimbursementAnswer(
                  SortedMap(
                    duty -> SortedMap(duty.taxCodes(0) -> reimbursement)
                  )
                ).some
              )
            )
          })

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          controller.submitClaim(duty, duty.taxCodes(0))(
            FakeRequest().withFormUrlEncodedBody(
              Seq(
                s"$enterScheduledClaimKey.paid-amount"   -> formatter.format(reimbursement.paidAmount),
                s"$enterScheduledClaimKey.actual-amount" -> formatter.format(reimbursement.shouldOfPaid)
              ): _*
            )
          ),
          routes.CheckScheduledClaimController.showReimbursements()
        )
      }
    }

    "show an error summary" when {
      "duty amounts missing or invalid" in forAll { (duty: DutyType, taxCode: TaxCode) =>
        val (session, _) = sessionWithAnswer()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.submitClaim(duty, taxCode)(
            FakeRequest().withFormUrlEncodedBody(
              Seq(
                s"$enterScheduledClaimKey.paid-amount"   -> "",
                s"$enterScheduledClaimKey.actual-amount" -> "bad"
              ): _*
            )
          ),
          messageFromMessageKey(
            messageKey = s"$enterScheduledClaimKey.title",
            messages(s"duty-type.${duty.repr}"),
            taxCode.value
          ),
          doc => {
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(s"$enterScheduledClaimKey.paid-amount.error.required")
            doc
              .select(".govuk-error-summary__list > li:nth-child(2) > a")
              .text() shouldBe messageFromMessageKey(s"$enterScheduledClaimKey.actual-amount.error.invalid")
          },
          BAD_REQUEST
        )
      }

      "actual amount is greater or equal to paid amount" in {
        forAll(genDuty, genTaxCode, genBigDecimal, Gen.choose(0, 100)) { (duty, taxCode, amount, n) =>
          val (session, _) = sessionWithAnswer()

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            controller.submitClaim(duty, taxCode)(
              FakeRequest().withFormUrlEncodedBody(
                Seq(
                  s"$enterScheduledClaimKey.paid-amount"   -> formatter.format(amount),
                  s"$enterScheduledClaimKey.actual-amount" -> formatter.format(amount + n)
                ): _*
              )
            ),
            messageFromMessageKey(
              messageKey = s"$enterScheduledClaimKey.title",
              messages(s"duty-type.${duty.repr}"),
              taxCode.value
            ),
            doc =>
              doc
                .select(".govuk-error-summary__list > li:nth-child(1) > a")
                .text() shouldBe messageFromMessageKey(s"$enterScheduledClaimKey.actual-amount.invalid.claim"),
            BAD_REQUEST
          )
        }
      }
    }
  }

  def sessionWithAnswer(
    selectedDutyTaxCodesReimbursementAnswer: Option[SelectedDutyTaxCodesReimbursementAnswer] = None
  ): (SessionData, DraftClaim) = {
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val draftC285Claim      = DraftClaim.blank.copy(
      selectedDutyTaxCodesReimbursementAnswer = selectedDutyTaxCodesReimbursementAnswer
    )
    (
      SessionData.empty.copy(journeyStatus = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim).some),
      draftC285Claim
    )
  }
}

object EnterScheduledClaimControllerSpec {

  private val formatter = new DecimalFormat()

  formatter.setMinimumFractionDigits(2)
  formatter.setGroupingUsed(false)
}
