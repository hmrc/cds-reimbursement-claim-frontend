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

import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator

class CheckBankDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with TestWithJourneyGenerator[SecuritiesJourney] {

  val claimService: ClaimService = mock[ClaimService]

  lazy val controller: CheckBankDetailsController = instanceOf[CheckBankDetailsController]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(claimService)
    )

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch enable Feature.Securities

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1)

  private def sessionWithBankDetailsInACC14(maybeBankDetails: Option[BankDetails]): SessionData = {
    val displayDeclaration = displayDeclarationNotCMAEligibleGen.sample.get
      .withBankDetails(maybeBankDetails)
      .withReasonForSecurity(ReasonForSecurity.CommunitySystemsOfDutyRelief)

    val securitiesJourney: SecuritiesJourney =
      emptyJourney
        .submitMovementReferenceNumber(displayDeclaration.getMRN)
        .submitReasonForSecurityAndDeclaration(ReasonForSecurity.CommunitySystemsOfDutyRelief, displayDeclaration)
        .getOrFail

    SessionData.empty.copy(
      securitiesJourney = Some(securitiesJourney)
    )
  }

  private def sessionWithBankDetailsStored(
    session: SessionData,
    bankAccountDetails: BankAccountDetails
  ): SessionData =
    session.copy(securitiesJourney =
      session.securitiesJourney
        .flatMap(_.submitBankAccountDetails(bankAccountDetails).toOption)
    )

  "Check Bank Details Controller" when {

    "Check Bank Account Details" should {

      "Redirect when BankDetails is empty and required" in {
        val bankDetails = BankDetails(None, None)
        val session     = sessionWithBankDetailsInACC14(Some(bankDetails))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val request = FakeRequest()
        val result  = controller.show()(request)

        checkIsRedirect(result, routes.CheckYourAnswersController.show())

      }

      "Redirect when BankDetails is None and required" in {
        val session = sessionWithBankDetailsInACC14(None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val request = FakeRequest()
        val result  = controller.show()(request)
        checkIsRedirect(result, routes.CheckYourAnswersController.show())
      }

      "Ok when BankDetails has consigneeBankDetails" in forAll(genBankAccountDetails) {
        consigneeBankDetails: BankAccountDetails =>
          val bankDetails = BankDetails(Some(consigneeBankDetails), None)
          val session     = sessionWithBankDetailsInACC14(Some(bankDetails))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          val request     = FakeRequest()
          val result      = controller.show()(request)

          checkIsRedirect(result, routes.CheckYourAnswersController.show())
      }

      "Ok when BankDetails has declarantBankDetails" in forAll(genBankAccountDetails) {
        declarantBankDetails: BankAccountDetails =>
          val bankDetails = BankDetails(None, Some(declarantBankDetails))
          val session     = sessionWithBankDetailsInACC14(Some(bankDetails))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          val request     = FakeRequest()
          val result      = controller.show()(request)

          checkIsRedirect(result, routes.CheckYourAnswersController.show())
      }
    }
  }
}
