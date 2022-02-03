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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import org.scalacheck.magnolia.gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService

class CheckBankDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with TableDrivenPropertyChecks
    with MockFactory {

  private val journeys = Table(
    "JourneyBindable",
    JourneyBindable.Single,
    JourneyBindable.Multiple,
    JourneyBindable.Scheduled
  )

  val claimService: ClaimService = mock[ClaimService]

  lazy val controller: CheckBankDetailsController = instanceOf[CheckBankDetailsController]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(claimService)
    )

  private def sessionWithMaskedBankDetails(maybeMaskedBankDetails: Option[BankDetails]): SessionData = {
    val displayResponseDetail = sample[DisplayResponseDetail].copy(maskedBankDetails = maybeMaskedBankDetails)
    val displayDeclaration    = sample[DisplayDeclaration].copy(displayResponseDetail = displayResponseDetail)

    val rejectedGoodsSingleJourney: RejectedGoodsSingleJourney =
      RejectedGoodsSingleJourney
        .empty(sample[Eori])
        .submitMovementReferenceNumberAndDeclaration(displayDeclaration.getMRN, displayDeclaration)
        .getOrElse(fail())

    SessionData.empty.copy(
      rejectedGoodsSingleJourney = Some(rejectedGoodsSingleJourney)
    )
  }

  "Check Bank Details Controller" when {

    "Check Bank Account Details" should {

      "Redirect when MaskedBankDetails is empty" in forAll(journeys) { _ =>
        val maskedBankDetails = BankDetails(None, None)
        val session           = sessionWithMaskedBankDetails(Some(maskedBankDetails))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val request = FakeRequest()
        val result  = controller.show()(request)

        checkIsRedirect(result, routes.ChooseBankAccountTypeController.show())

      }

      "Redirect when MaskedBankDetails is None" in forAll(journeys) { _ =>
        val session = sessionWithMaskedBankDetails(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val request = FakeRequest()
        val result  = controller.show()(request)
        checkIsRedirect(result, routes.ChooseBankAccountTypeController.show())
      }

      "Ok when MaskedBankDetails has consigneeBankDetails" in forAll(journeys) { _ =>
        val consigneeDetails  = sample[BankAccountDetails]
        val maskedBankDetails = BankDetails(Some(consigneeDetails), None)
        val session           = sessionWithMaskedBankDetails(Some(maskedBankDetails))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val request           = FakeRequest()
        val result            = controller.show()(request)
        status(result) shouldBe OK
      }

      "Ok when MaskedBankDetails has declarantBankDetails" in forAll(journeys) { _ =>
        val declarantBankDetails = sample[BankAccountDetails]
        val maskedBankDetails    = BankDetails(None, Some(declarantBankDetails))
        val session              = sessionWithMaskedBankDetails(Some(maskedBankDetails))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val request              = FakeRequest()
        val result               = controller.show()(request)
        status(result) shouldBe OK
      }
    }
  }
}
