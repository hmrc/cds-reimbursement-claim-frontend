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

import cats.implicits.catsSyntaxEq
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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.completeJourneyCMAEligibleGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ReimbursementMethodAnswer._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.genStringWithMaxSizeOfN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import scala.concurrent.Future

@SuppressWarnings(Array("org.wartremover.warts.EitherProjectionPartial"))
class ChooseRepaymentMethodControllerSpec
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

  val controller: ChooseRepaymentMethodController = instanceOf[ChooseRepaymentMethodController]

  private val formKey = "choose-payment-method.rejected-goods.single"

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session = SessionData.empty.copy(
    rejectedGoodsSingleJourney = Some(RejectedGoodsSingleJourney.empty(exampleEori))
  )

  def sessionWithNdrcDetails(ndrcDetails: List[NdrcDetails], displayDeclaration: DisplayDeclaration) = {
    val drd       = displayDeclaration.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
    val updatedDd = displayDeclaration.copy(displayResponseDetail = drd)
    val taxCode   = ndrcDetails.map(details => TaxCode(details.taxType))
    val journey   = RejectedGoodsSingleJourney
      .empty(exampleEori)
      .submitDisplayDeclaration(updatedDd)
      .selectAndReplaceTaxCodeSetForReimbursement(taxCode)
      .right
      .get
    SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))
  }

  "Choose Payment Method Controller" when {
    "Show Choose Payment Method page" must {

      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "show the page on a new journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$formKey.title"),
          doc => {
            doc.select(s"input[name='$formKey'][checked]").size() shouldBe 0
            doc.select("form").attr("action")                     shouldBe routes.ChooseRepaymentMethodController.submit().url
          }
        )
      }

      "show the page on a pre-existing journey" in {
        forAll(completeJourneyCMAEligibleGen) { journey =>
          val session = SessionData.empty.copy(
            rejectedGoodsSingleJourney = Some(journey)
          )

          val expectedCheckedKey = journey.answers.reimbursementMethod match {
            case Some(CurrentMonthAdjustment) => "input[value=0]"
            case _                            => "input[value=1]"
          }

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$formKey.title"),
            doc => doc.select(expectedCheckedKey).hasAttr("checked") shouldBe true
          )
        }
      }
    }

    "Submit Choose Payment Method page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty Repayment Method" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$formKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$formKey.error.required"),
          BAD_REQUEST
        )
      }

      "reject an unknown Repayment Method" in {
        forAll(genStringWithMaxSizeOfN(5)) { answer =>
          whenever(answer =!= "0" && answer =!= "1") {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            checkPageIsDisplayed(
              performAction(formKey -> answer),
              messageFromMessageKey(s"$formKey.title"),
              doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$formKey.error.required"),
              BAD_REQUEST
            )
          }
        }
      }

      "reject any repayment method and move on to the check your answers page when not eligible for CMA" in {
        forAll { (ndrcDetails: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
          whenever(!ndrcDetails.isCmaEligible) {
            val session = sessionWithNdrcDetails(List(ndrcDetails), displayDeclaration)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            checkIsRedirect(
              performAction(formKey -> "0"),
              routes.CheckYourAnswersController.show()
            )
          }
        }
      }

      "accept Current Month Adjustment and move on to the upload supporting evidence page" in {
        forAll { (ndrcDetails: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
          whenever(ndrcDetails.isCmaEligible) {
            val session        = sessionWithNdrcDetails(List(ndrcDetails), displayDeclaration)
            val updatedJourney =
              session.rejectedGoodsSingleJourney.get.submitReimbursementMethod(CurrentMonthAdjustment).right.get
            val updatedSession = session.copy(rejectedGoodsSingleJourney = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              performAction(formKey -> "0"),
              routes.ChooseFileTypeController.show()
            )
          }
        }
      }

      "accept Bank Transfer and move on to the check these bank details are correct page" in {
        forAll { (ndrcDetails: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
          whenever(ndrcDetails.isCmaEligible) {
            val session        = sessionWithNdrcDetails(List(ndrcDetails), displayDeclaration)
            val updatedJourney =
              session.rejectedGoodsSingleJourney.get.submitReimbursementMethod(BankAccountTransfer).right.get
            val updatedSession = session.copy(rejectedGoodsSingleJourney = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              performAction(formKey -> "1"),
              "check-these-bank-details-are-correct"
            )
          }
        }
      }
    }
  }
}
