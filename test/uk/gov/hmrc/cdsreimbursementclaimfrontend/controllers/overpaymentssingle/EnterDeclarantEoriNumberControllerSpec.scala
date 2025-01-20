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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import cats.implicits.*
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarantDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class EnterDeclarantEoriNumberControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterDeclarantEoriNumberController = instanceOf[EnterDeclarantEoriNumberController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  val session: SessionData = SessionData(
    OverpaymentsSingleJourney
      .empty(anotherExampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleDisplayDeclaration.getMRN, exampleDisplayDeclaration)
      .getOrFail
  )

  "Declarant Eori Number Controller" when {
    "Enter Declarant Eori page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page on a new journey" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc => {
            doc
              .select("form div#enter-declarant-eori-number-hint")
              .text()                                          shouldBe messageFromMessageKey("enter-declarant-eori-number.help-text")
            doc.select("#enter-declarant-eori-number").`val`() shouldBe ""
            doc.select("form").attr("action")                  shouldBe routes.EnterDeclarantEoriNumberController.submit.url
          }
        )
      }

      "redirect to basis of claim selection when eori check not needed (user eori is declarant eori)" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(
            SessionData(
              OverpaymentsSingleJourney
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
          routes.BasisForClaimController.show
        )
      }

      "redirect to basis of claim selection when eori check not needed (user eori is consignee eori)" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(
            SessionData(
              OverpaymentsSingleJourney
                .empty(exampleDisplayDeclaration.getConsigneeEori.get)
                .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
                .getOrFail
            )
          )
        }

        checkIsRedirect(
          performAction(),
          routes.BasisForClaimController.show
        )
      }

      "display the page on a pre-existing journey" in {
        val journey        = buildCompleteJourneyGen(
          acc14DeclarantMatchesUserEori = false,
          acc14ConsigneeMatchesUserEori = false
        ).sample.getOrElse(
          fail("Unable to generate complete journey")
        )
        val eori           = journey.answers.eoriNumbersVerification
          .flatMap(_.declarantEoriNumber)
          .getOrElse(fail("No consignee eori found"))
        val sessionToAmend = SessionData(journey)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(sessionToAmend)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc => {
            doc
              .select("form div#enter-declarant-eori-number-hint")
              .text()                                          shouldBe messageFromMessageKey("enter-declarant-eori-number.help-text")
            doc.select("#enter-declarant-eori-number").`val`() shouldBe eori.value
          }
        )
      }
    }

    "Submit Declarant Eori  page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty Eori" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(controller.eoriNumberFormKey -> ""),
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-declarant-eori-number.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid Eori" in {
        val invalidEori = Eori("INVALID_MRN")

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(controller.eoriNumberFormKey -> invalidEori.value),
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc => {
            getErrorSummary(doc)                               shouldBe messageFromMessageKey("enter-declarant-eori-number.invalid.number")
            doc.select("#enter-declarant-eori-number").`val`() shouldBe ""
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "submit a valid Eori which is the Consignee Eori" in forAll { (mrn: MRN, eori: Eori) =>
        val initialJourney                = session.overpaymentsSingleJourney.getOrElse(fail("No overpayments journey"))
        val displayDeclaration            = buildDisplayDeclaration().withDeclarationId(mrn.value)
        val declarantDetails              = sample[DeclarantDetails].copy(declarantEORI = eori.value)
        val updatedDisplayResponseDetails =
          displayDeclaration.displayResponseDetail.copy(declarantDetails = declarantDetails)
        val updatedDisplayDeclaration     = displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)
        val journey                       =
          initialJourney
            .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
            .getOrFail
        val requiredSession               = SessionData(journey)
        val updatedJourney                = journey.submitDeclarantEoriNumber(eori).getOrElse(fail("Unable to update eori"))
        val updatedSession                = SessionData(updatedJourney)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(requiredSession)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(controller.eoriNumberFormKey -> eori.value),
          routes.CheckDeclarationDetailsController.show
        )
      }

      "submit a valid Eori which is not the declarant" in forAll {
        (mrn: MRN, enteredDeclarantEori: Eori, wantedDeclarant: Eori) =>
          whenever(enteredDeclarantEori =!= wantedDeclarant) {
            val initialJourney                = session.overpaymentsSingleJourney.getOrElse(fail("No overpayments journey"))
            val displayDeclaration            = buildDisplayDeclaration().withDeclarationId(mrn.value)
            val updatedDeclarantDetails       =
              displayDeclaration.getDeclarantDetails.copy(declarantEORI = wantedDeclarant.value)
            val updatedDisplayResponseDetails =
              displayDeclaration.displayResponseDetail.copy(declarantDetails = updatedDeclarantDetails)
            val updatedDisplayDeclaration     =
              displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)
            val journey                       =
              initialJourney
                .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
                .getOrFail
            val requiredSession               = SessionData(journey)

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(requiredSession)
            }

            checkIsRedirect(
              performAction(controller.eoriNumberFormKey -> enteredDeclarantEori.value),
              baseRoutes.IneligibleController.ineligible
            )
          }
      }
    }
  }
}
