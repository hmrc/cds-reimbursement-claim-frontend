/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.buildCompleteJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.displayDeclarationGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarantDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.EstablishmentAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DateGen.genDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class EnterInspectionDateControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks {

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

  val session: SessionData = SessionData(journeyWithMrnAndDeclaration)

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
            selectedInput(doc) shouldBe empty
          }
        )
      }

      "the user has answered this question before" in forAll(buildCompleteJourneyGen()) { journey =>
        val inspectionDate = journey.answers.inspectionDate
        val updatedSession = session.copy(rejectedGoodsSingleJourney = Some(journey))

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
      "the user enters a date for the first time and Acc14 has returned contact details for the importer or declarant" in forAll(
        displayDeclarationGen,
        genDate
      ) { (displayDeclaration, date) =>
        val address: EstablishmentAddress = sample[EstablishmentAddress].copy(postalCode = Some("BN16 1A9"))
        val declarant                     = sample[DeclarantDetails].copy(establishmentAddress = address)
        val consignee                     = sample[ConsigneeDetails].copy(establishmentAddress = address)

        val updatedDisplayResponseDetails = displayDeclaration.displayResponseDetail
          .copy(consigneeDetails = Some(consignee), declarantDetails = declarant)
        val updatedDisplayDeclaration     = displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)

        val initialJourney =
          RejectedGoodsSingleJourney
            .empty(updatedDisplayDeclaration.getDeclarantEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, updatedDisplayDeclaration)
            .getOrFail

        val initialSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(initialJourney))
        val updatedJourney = initialJourney.submitInspectionDate(date)
        val updatedSession = session.copy(rejectedGoodsSingleJourney = Some(updatedJourney))

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

      "the user enters a date for the first time and Acc14 hasn't returned any contact details" in forAll(
        genDate,
        displayDeclarationGen
      ) { (date, displayDeclaration) =>
        val address: EstablishmentAddress = sample[EstablishmentAddress].copy(postalCode = None)
        val declarant                     = sample[DeclarantDetails].copy(contactDetails = None, establishmentAddress = address)
        val consignee                     = sample[ConsigneeDetails].copy(contactDetails = None, establishmentAddress = address)

        val updatedDisplayResponseDetails = displayDeclaration.displayResponseDetail
          .copy(consigneeDetails = Some(consignee), declarantDetails = declarant)
        val updatedDisplayDeclaration     = displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)

        val journey =
          RejectedGoodsSingleJourney
            .empty(updatedDisplayDeclaration.getDeclarantEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, updatedDisplayDeclaration)
            .getOrFail

        val requiredSession = session.copy(rejectedGoodsSingleJourney = Some(journey))
        val updatedJourney  = journey.submitInspectionDate(date)
        val updatedSession  = session.copy(rejectedGoodsSingleJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(requiredSession)
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

    "show an error summary" when {
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
    }

    "redirect to CYA page" when {
      "journey is complete" in forAll(buildCompleteJourneyGen(), genDate) { (journey, date) =>
        val journeyWithInspectionDate = journey.submitInspectionDate(date)
        val sessionWithInspectionDate = session.copy(rejectedGoodsSingleJourney = Some(journeyWithInspectionDate))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(rejectedGoodsSingleJourney = Some(journey)))
          mockStoreSession(sessionWithInspectionDate)(Right(()))
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
