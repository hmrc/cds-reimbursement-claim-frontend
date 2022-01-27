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

import cats.implicits.catsSyntaxOptionId
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import shapeless.lens
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer.CurrentMonthAdjustment
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.BankAccountGen.arbitraryBankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class ChooseBankAccountTypeControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with RejectedGoodsSingleJourneyTestData {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val session = SessionData.empty.copy(
    rejectedGoodsSingleJourney = Some(emptyJourney)
  )

  val controller: ChooseBankAccountTypeController = instanceOf[ChooseBankAccountTypeController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch enable Feature.RejectedGoods

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1)

  "Choose Bank Account Type Controller" should {

    def showPage(): Future[Result] =
      controller.show()(FakeRequest())

    def submitBankAccountType(data: (String, String)*): Future[Result] =
      controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

    "display page" in forAll { maybeBankAccountType: Option[BankAccountType] =>
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(
          maybeBankAccountType.toList.foldLeft(session)((session, bankAccountType) =>
            session.copy(rejectedGoodsSingleJourney = emptyJourney.submitBankAccountType(bankAccountType).toOption)
          )
        )
      }

      checkPageIsDisplayed(
        showPage(),
        messageFromMessageKey("select-bank-account-type.title")
      )
    }

    "fail to submit bank account type" when {
      "nothing is selected" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          submitBankAccountType(),
          messageFromMessageKey("select-bank-account-type.title"),
          doc =>
            doc
              .select(".govuk-error-summary__list > li > a")
              .text() shouldBe messageFromMessageKey(
              "select-bank-account-type.error.required"
            ),
          BAD_REQUEST
        )
      }

      "reimbursement method is current month adjustment" in forAll {
        (bankAccountType: BankAccountType, declaration: DisplayDeclaration, ndrc: NdrcDetails) =>
          val updatedDeclaration = lens[DisplayDeclaration].displayResponseDetail.modify(declaration)(
            _.copy(ndrcDetails = List(ndrc.copy(cmaEligible = "1".some)).some)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session.copy(
                rejectedGoodsSingleJourney = emptyJourney
                  .submitDisplayDeclaration(updatedDeclaration)
                  .selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode(ndrc.taxType)))
                  .flatMap(_.submitReimbursementMethod(CurrentMonthAdjustment))
                  .toOption
              )
            )
          }

          checkIsRedirect(
            submitBankAccountType("select-bank-account-type" -> bankAccountType.toString),
            baseRoutes.IneligibleController.ineligible()
          )
      }
    }

    "successfully submit bank account type" when {
      "one of the options selected" in forAll { bankAccountType: BankAccountType =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(
            session.copy(
              rejectedGoodsSingleJourney = emptyJourney.submitBankAccountType(bankAccountType).toOption
            )
          )(Right(()))
        }

        checkIsRedirect(
          submitBankAccountType("select-bank-account-type" -> bankAccountType.toString),
          "/enter-bank-account-details"
        )
      }
    }
  }
}
