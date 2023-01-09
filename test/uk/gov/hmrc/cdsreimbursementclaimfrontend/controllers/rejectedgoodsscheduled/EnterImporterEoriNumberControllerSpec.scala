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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import cats.implicits._
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class EnterImporterEoriNumberControllerSpec
    extends ControllerSpec
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

  val controller: EnterImporterEoriNumberController = instanceOf[EnterImporterEoriNumberController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  private val session = SessionData(
    RejectedGoodsScheduledJourney
      .empty(anotherExampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleDisplayDeclaration.getMRN, exampleDisplayDeclaration)
      .getOrFail
  )

  "Importer Eori Number Controller" when {
    "Enter Importer Eori page" must {

      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      "do not find the page if rejected goods feature is disabled" in {
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
          messageFromMessageKey("enter-importer-eori-number.title"),
          doc => {
            doc
              .select("form div#enter-importer-eori-number-hint")
              .text()                                         shouldBe messageFromMessageKey("enter-importer-eori-number.help-text")
            doc.select("#enter-importer-eori-number").`val`() shouldBe ""
            doc.select("form").attr("action")                 shouldBe routes.EnterImporterEoriNumberController.submit().url
          }
        )
      }

      "redirect to the upload mrn list page when eori check not needed (user eori is declarant eori)" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData(
              RejectedGoodsScheduledJourney
                .empty(exampleDisplayDeclaration.getDeclarantEori)
                .submitMovementReferenceNumberAndDeclaration(
                  exampleDisplayDeclaration.getMRN,
                  exampleDisplayDeclaration
                )
                .getOrFail
            )
          )
        }

        checkIsRedirect(
          performAction(),
          routes.UploadMrnListController.show()
        )
      }

      "display the page on a pre-existing journey" in {
        val journey        = buildCompleteJourneyGen(
          acc14DeclarantMatchesUserEori = false,
          acc14ConsigneeMatchesUserEori = false,
          hasConsigneeDetailsInACC14 = true
        ).sample.getOrElse(
          fail("Unable to generate complete journey")
        )
        val eori           = journey.answers.consigneeEoriNumber.getOrElse(fail("No consignee eori found"))
        val sessionToAmend = session.copy(rejectedGoodsScheduledJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionToAmend)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-importer-eori-number.title"),
          doc => {
            doc
              .select("form div#enter-importer-eori-number-hint")
              .text()                                         shouldBe messageFromMessageKey("enter-importer-eori-number.help-text")
            doc.select("#enter-importer-eori-number").`val`() shouldBe eori.value
          }
        )
      }
    }

    "Submit Importer Eori  page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty Eori" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(controller.eoriNumberFormKey -> ""),
          messageFromMessageKey("enter-importer-eori-number.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-importer-eori-number.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid Eori" in {
        val invalidEori = Eori("INVALID_MRN")

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(controller.eoriNumberFormKey -> invalidEori.value),
          messageFromMessageKey("enter-importer-eori-number.title"),
          doc => {
            getErrorSummary(doc)                              shouldBe messageFromMessageKey("enter-importer-eori-number.invalid.number")
            doc.select("#enter-importer-eori-number").`val`() shouldBe ""
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "submit a valid Eori which is the Consignee Eori" in forAll {
        (
          mrn: MRN,
          eori: Eori,
          initialDisplayDeclaration: DisplayDeclaration,
          initialConsigneeDetails: ConsigneeDetails
        ) =>
          val initialJourney                = session.rejectedGoodsScheduledJourney.getOrElse(fail("No rejected goods journey"))
          val displayDeclaration            = initialDisplayDeclaration.withDeclarationId(mrn.value)
          val consigneeDetails              = initialConsigneeDetails.copy(consigneeEORI = eori.value)
          val updatedDisplayResponseDetails =
            displayDeclaration.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails))
          val updatedDisplayDeclaration     = displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)
          val journey                       =
            initialJourney
              .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
              .getOrFail

          val requiredSession = session.copy(rejectedGoodsScheduledJourney = Some(journey))
          val updatedJourney  = journey.submitConsigneeEoriNumber(eori).getOrElse(fail("Unable to update eori"))
          val updatedSession  = session.copy(rejectedGoodsScheduledJourney = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(requiredSession)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(controller.eoriNumberFormKey -> eori.value),
            routes.EnterDeclarantEoriNumberController.show()
          )
      }

      "submit a valid Eori which is not the consignee" in forAll {
        (
          mrn: MRN,
          enteredConsigneeEori: Eori,
          wantedConsignee: Eori,
          initialDisplayDeclaration: DisplayDeclaration
        ) =>
          whenever(enteredConsigneeEori =!= wantedConsignee) {
            val initialJourney                = session.rejectedGoodsScheduledJourney.getOrElse(fail("No rejected goods journey"))
            val displayDeclaration            = initialDisplayDeclaration.withDeclarationId(mrn.value)
            val updatedConsigneDetails        =
              displayDeclaration.getConsigneeDetails.map(_.copy(consigneeEORI = wantedConsignee.value))
            val updatedDisplayResponseDetails =
              displayDeclaration.displayResponseDetail.copy(consigneeDetails = updatedConsigneDetails)
            val updatedDisplayDeclaration     =
              displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)
            val journey                       =
              initialJourney
                .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
                .getOrFail
            val requiredSession               = session.copy(rejectedGoodsScheduledJourney = Some(journey))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(requiredSession)
            }

            checkIsRedirect(
              performAction(controller.eoriNumberFormKey -> enteredConsigneeEori.value),
              baseRoutes.IneligibleController.ineligible()
            )
          }
      }
    }
  }
}
