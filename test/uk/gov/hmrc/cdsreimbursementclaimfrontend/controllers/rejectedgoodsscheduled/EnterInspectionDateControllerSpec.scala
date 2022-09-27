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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AdjustDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReplaceEstablishmentAddresses
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DateGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class EnterInspectionDateControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with AdjustDisplayDeclaration
    with ReplaceEstablishmentAddresses {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterInspectionDateController = instanceOf[EnterInspectionDateController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch  = instanceOf[FeatureSwitchService]
  private val messagesKey: String = "enter-inspection-date.rejected-goods"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session: SessionData = SessionData.empty.copy(
    rejectedGoodsScheduledJourney = Some(RejectedGoodsScheduledJourney.empty(exampleEori))
  )

  "Enter Special Circumstances Controller" must {
    "Show Page" when {
      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page on a new journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            formAction(doc)                                                     shouldBe routes.EnterInspectionDateController.submit().url
            selectedInputBox(doc, "enter-inspection-date.rejected-goods.day")   shouldBe Some("")
            selectedInputBox(doc, "enter-inspection-date.rejected-goods.month") shouldBe Some("")
            selectedInputBox(doc, "enter-inspection-date.rejected-goods.year")  shouldBe Some("")
          }
        )
      }

      "the user has answered this question before" in forAll(buildCompleteJourneyGen()) { journey =>
        val inspectionDate = journey.answers.inspectionDate
        val updatedSession = session.copy(rejectedGoodsScheduledJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            selectedInputBox(doc, "enter-inspection-date.rejected-goods.day")   shouldBe Some(
              inspectionDate.get.value.getDayOfMonth.toString
            )
            selectedInputBox(doc, "enter-inspection-date.rejected-goods.month") shouldBe Some(
              inspectionDate.get.value.getMonthValue.toString
            )
            selectedInputBox(doc, "enter-inspection-date.rejected-goods.year")  shouldBe Some(
              inspectionDate.get.value.getYear.toString
            )
          }
        )
      }
    }

    "Submit Page" when {
      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "the user submits an empty date" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$messagesKey.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "the user submits an invalid date" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(
            s"${controller.formKey}.day"   -> "a",
            s"${controller.formKey}.month" -> "b",
            s"${controller.formKey}.year"  -> "c"
          ),
          messageFromMessageKey(s"$messagesKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$messagesKey.error.invalid"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "the user enters a date for the first time and Acc14 has returned contact details for the importer or declarant" in forAll {
        (
          displayDeclaration: DisplayDeclaration,
          address: EstablishmentAddress,
          date: InspectionDate
        ) =>
          whenever(address.postalCode.isDefined) {
            val adjustedDeclaration = adjustWithDeclarantEori(
              replaceEstablishmentAddresses(displayDeclaration, address),
              session.rejectedGoodsScheduledJourney.get
            )
            val journey             = session.rejectedGoodsScheduledJourney.get
              .submitMovementReferenceNumberAndDeclaration(adjustedDeclaration.getMRN, adjustedDeclaration)
              .getOrFail
            val initialSession      = SessionData.empty.copy(rejectedGoodsScheduledJourney = Some(journey))

            val updatedJourney = journey.submitInspectionDate(date)
            val updatedSession = SessionData(updatedJourney)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(initialSession)
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              performAction(
                s"$messagesKey.day"   -> date.value.getDayOfMonth.toString,
                s"$messagesKey.month" -> date.value.getMonthValue.toString,
                s"$messagesKey.year"  -> date.value.getYear.toString
              ),
              routes.ChooseInspectionAddressTypeController.show()
            )
          }
      }

      "the user enters a date for the first time and Acc14 hasn't returned any contact details" in forAll {
        (
          displayDeclaration: DisplayDeclaration,
          date: InspectionDate
        ) =>
          val addressWithoutPostCode =
            displayDeclaration.getDeclarantDetails.establishmentAddress.copy(postalCode = None)
          val adjustedDeclaration    = adjustWithDeclarantEori(
            replaceEstablishmentAddresses(displayDeclaration, addressWithoutPostCode),
            session.rejectedGoodsScheduledJourney.get
          )

          val journey        = session.rejectedGoodsScheduledJourney.get
            .submitMovementReferenceNumberAndDeclaration(adjustedDeclaration.getMRN, adjustedDeclaration)
            .getOrFail
          val initialSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = Some(journey))

          val updatedJourney = journey.submitInspectionDate(date)
          val updatedSession = SessionData(updatedJourney)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(initialSession)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(
              s"$messagesKey.day"   -> date.value.getDayOfMonth.toString,
              s"$messagesKey.month" -> date.value.getMonthValue.toString,
              s"$messagesKey.year"  -> date.value.getYear.toString
            ),
            routes.ChooseInspectionAddressTypeController.redirectToALF()
          )
      }

      "redirect to CYA page if journey is complete" in forAll(buildCompleteJourneyGen(), genDate) { (journey, date) =>
        val initialSession = SessionData(journey)
        val updatedJourney = journey.submitInspectionDate(date)
        val updatedSession = SessionData(updatedJourney)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(initialSession)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(
            s"${controller.formKey}.day"   -> date.value.getDayOfMonth.toString,
            s"${controller.formKey}.month" -> date.value.getMonthValue.toString,
            s"${controller.formKey}.year"  -> date.value.getYear.toString
          ),
          routes.CheckYourAnswersController.show()
        )
      }
    }
  }
}
