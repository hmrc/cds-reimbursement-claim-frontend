/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement

import cats.implicits.catsSyntaxOptionId
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.EnterReimbursementClaimController.enterReimbursementClaimKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SelectedDutyTaxCodesReimbursementAnswer.{dutyTypesOrdering, taxCodesOrdering}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, DutyType, Reimbursement, SessionData, SignedInUserDetails, TaxCode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ReimbursementGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.TaxCodeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._

import scala.collection.immutable.SortedMap

class EnterReimbursementClaimControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterReimbursementClaimController = instanceOf[EnterReimbursementClaimController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 10)

  "Enter Reimbursement Controller" should {

    "redirect to the check reimbursements page" when {
      "no reimbursements to claim" in {
        val (session, _) = sessionWithDutyCodesState()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          controller.iterate()(FakeRequest()),
          routes.CheckReimbursementClaimController.showReimbursements()
        )
      }
    }

    "show enter claim amount page" when {
      "user has selected duty and tax codes" in forAll { (duty: DutyType, taxCode: TaxCode) =>
        val (session, _) = sessionWithDutyCodesState(
          SelectedDutyTaxCodesReimbursementAnswer(SortedMap(duty -> SortedMap(taxCode -> Reimbursement.unclaimed))).some
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          controller.iterate()(FakeRequest()),
          routes.EnterReimbursementClaimController.enterClaim(duty, taxCode)
        )
      }
    }

    "display already entered amounts" when {
      "user revisits enter claim page again" in forAll {
        (duty: DutyType, taxCode: TaxCode, reimbursement: Reimbursement) =>
          val (session, _) = sessionWithDutyCodesState(
            SelectedDutyTaxCodesReimbursementAnswer(SortedMap(duty -> SortedMap(taxCode -> reimbursement))).some
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            controller.enterClaim(duty, taxCode)(FakeRequest()),
            messageFromMessageKey(
              messageKey = s"$enterReimbursementClaimKey.title",
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

    "save user defined amounts and ask user to enter next amounts for the next reimbursement" in {

    }
  }

  def sessionWithDutyCodesState(
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
