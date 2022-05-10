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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AdjustDisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReplaceEstablishmentAddresses
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DateGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import scala.concurrent.Future

class EnterInspectionDateControllerSpec
    extends ControllerSpec
    with AddAcc14
    with AdjustDisplayDeclaration
    with ReplaceEstablishmentAddresses
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with JourneyTestData {

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

  def showPage(): Future[Result] =
    controller.show()(FakeRequest())

  def submitInspectionDate(data: (String, String)*): Future[Result] =
    controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session: SessionData = SessionData.empty.copy(
    rejectedGoodsMultipleJourney = Some(RejectedGoodsMultipleJourney.empty(exampleEori))
  )

  "Enter Inspection Date Controller" must {

    "not find the page if rejected goods feature is disabled" in {
      featureSwitch.disable(Feature.RejectedGoods)

      status(showPage()) shouldBe NOT_FOUND
    }

    "display the page" when {
      "the user has not answered this question before" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          showPage(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            doc
              .select("main p")
              .html()          shouldBe messageFromMessageKey(s"$messagesKey.help-text")
            formAction(doc)    shouldBe routes.EnterInspectionDateController.submit().url
            selectedInput(doc) shouldBe empty
          }
        )
      }

      "the user has answered this question before" in forAll(buildCompleteJourneyGen()) { journey =>
        val inspectionDate = journey.answers.inspectionDate
        val updatedSession = session.copy(rejectedGoodsMultipleJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          showPage(),
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

    "handle submit requests" when {

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(submitInspectionDate()) shouldBe NOT_FOUND
      }

      "the user submits an empty date" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          submitInspectionDate(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$messagesKey.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "the user enters a date for the first time and Acc14 has returned contact details for the importer or declarant" in forAll {
        (
          firstDisplayDeclaration: DisplayDeclaration,
          secondDisplayDeclaration: DisplayDeclaration,
          address: EstablishmentAddress,
          date: InspectionDate
        ) =>
          whenever(
            address.postalCode.isDefined && (firstDisplayDeclaration.getMRN !== secondDisplayDeclaration.getMRN)
          ) {
            val journey        = (for {
              j1 <- addAcc14(
                      session.rejectedGoodsMultipleJourney.get,
                      replaceEstablishmentAddresses(firstDisplayDeclaration, address)
                    )
              j2 <- addAcc14(j1, replaceEstablishmentAddresses(secondDisplayDeclaration, address))
            } yield j2).getOrFail
            val initialSession = SessionData.empty.copy(rejectedGoodsMultipleJourney = Some(journey))

            val updatedJourney = journey.submitInspectionDate(date)
            val updatedSession = session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(initialSession)
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              submitInspectionDate(
                s"${controller.formKey}.day"   -> date.value.getDayOfMonth.toString,
                s"${controller.formKey}.month" -> date.value.getMonthValue.toString,
                s"${controller.formKey}.year"  -> date.value.getYear.toString
              ),
              routes.ChooseInspectionAddressTypeController.show()
            )
          }
      }

      "the user enters a date for the first time and Acc14 hasn't returned any contact details" in forAll {
        (
          firstDisplayDeclaration: DisplayDeclaration,
          secondDisplayDeclaration: DisplayDeclaration,
          date: InspectionDate
        ) =>
          whenever(firstDisplayDeclaration.getMRN !== secondDisplayDeclaration.getMRN) {
            val addressWithoutPostCode =
              firstDisplayDeclaration.getDeclarantDetails.establishmentAddress.copy(postalCode = None)
            val journey                = (for {
              j1 <- addAcc14(
                      session.rejectedGoodsMultipleJourney.get,
                      replaceEstablishmentAddresses(firstDisplayDeclaration, addressWithoutPostCode)
                    )
              j2 <- addAcc14(j1, replaceEstablishmentAddresses(secondDisplayDeclaration, addressWithoutPostCode))
            } yield j2).getOrFail
            val initialSession         = SessionData.empty.copy(rejectedGoodsMultipleJourney = Some(journey))

            val updatedJourney = journey.submitInspectionDate(date)
            val updatedSession = session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(initialSession)
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              submitInspectionDate(
                s"${controller.formKey}.day"   -> date.value.getDayOfMonth.toString,
                s"${controller.formKey}.month" -> date.value.getMonthValue.toString,
                s"${controller.formKey}.year"  -> date.value.getYear.toString
              ),
              routes.ChooseInspectionAddressTypeController.redirectToALF()
            )
          }
      }

      "redirect to CYA page if journey is complete" in forAll(buildCompleteJourneyGen(), genDate) { (journey, date) =>
        val initialSession = session.copy(rejectedGoodsMultipleJourney = Some(journey))
        val updatedJourney = journey.submitInspectionDate(date)
        val updatedSession = session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(initialSession)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          submitInspectionDate(
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
