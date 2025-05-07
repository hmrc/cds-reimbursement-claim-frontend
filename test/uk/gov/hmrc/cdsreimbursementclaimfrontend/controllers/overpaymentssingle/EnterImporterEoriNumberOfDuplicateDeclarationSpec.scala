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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class EnterImporterEoriNumberOfDuplicateDeclarationSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterImporterEoriNumberOfDuplicateDeclaration =
    instanceOf[EnterImporterEoriNumberOfDuplicateDeclaration]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  val originalDeclaration  = exampleDisplayDeclaration
  val duplicateDeclaration = buildDisplayDeclaration(id = anotherExampleMrn.value, consigneeEORI = Some(exampleEori))

  val journey: OverpaymentsSingleJourney = OverpaymentsSingleJourney
    .empty(originalDeclaration.getConsigneeEori.get)
    .submitMovementReferenceNumberAndDeclaration(originalDeclaration.getMRN, originalDeclaration)
    .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
    .flatMap(
      _.submitDuplicateMovementReferenceNumberAndDeclaration(
        duplicateDeclaration.getMRN,
        duplicateDeclaration
      )
    )
    .getOrFail

  "Importer Eori Number Controller" when {
    "Enter Importer Eori page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page on a new journey" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-importer-eori-number.title"),
          doc => {
            doc
              .select("form div#enter-importer-eori-number-hint")
              .text()                                         shouldBe messageFromMessageKey("enter-importer-eori-number.help-text")
            doc.select("#enter-importer-eori-number").`val`() shouldBe ""
            doc.select("form").attr("action")                 shouldBe routes.EnterImporterEoriNumberOfDuplicateDeclaration.submit.url
          }
        )
      }

      "redirect to enter additional details when eori check not needed (user eori is consignee eori)" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(
            SessionData(
              OverpaymentsSingleJourney
                .empty(exampleDisplayDeclaration.getConsigneeEori.get)
                .submitMovementReferenceNumberAndDeclaration(
                  exampleDisplayDeclaration.getMRN,
                  exampleDisplayDeclaration
                )
                .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
                .flatMap(
                  _.submitDuplicateMovementReferenceNumberAndDeclaration(
                    duplicateDeclaration.getMRN,
                    duplicateDeclaration.withConsigneeEori(exampleDisplayDeclaration.getConsigneeEori.get)
                  )
                )
                .getOrFail
            )
          )
        }

        checkIsRedirect(
          performAction(),
          routes.EnterAdditionalDetailsController.show
        )
      }

      "display the page on a pre-existing journey" in {
        val journey        = buildCompleteJourneyGen()
          .map(
            _.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry)
              .submitDuplicateMovementReferenceNumberAndDeclaration(
                duplicateDeclaration.getMRN,
                duplicateDeclaration
              )
              .getOrFail
          )
          .sample
          .getOrElse(
            fail("Unable to generate complete journey")
          )
        val sessionToAmend = SessionData(journey)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(sessionToAmend)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-importer-eori-number.title"),
          doc => {
            doc
              .select("form div#enter-importer-eori-number-hint")
              .text()                                         shouldBe messageFromMessageKey("enter-importer-eori-number.help-text")
            doc.select("#enter-importer-eori-number").`val`() shouldBe ""
          }
        )
      }
    }

    "Submit Importer Eori  page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "do not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty Eori" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-importer-eori-number.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-importer-eori-number.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid Eori" in {
        val invalidEori = Eori("INVALID_MRN")

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(controller.eoriNumberFormKey -> invalidEori.value),
          messageFromMessageKey("enter-importer-eori-number.title"),
          doc => {
            getErrorSummary(doc)                              shouldBe messageFromMessageKey("enter-importer-eori-number.invalid.number")
            doc.select("#enter-importer-eori-number").`val`() shouldBe "INVALID_MRN"
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "submit a valid Eori which is the Consignee Eori" in forAll { (mrn: MRN, eori: Eori) =>
        val displayDeclaration                 =
          buildDisplayDeclaration(consigneeEORI = Some(anotherExampleEori)).withDeclarationId(mrn.value)
        val journey: OverpaymentsSingleJourney = OverpaymentsSingleJourney
          .empty(anotherExampleEori)
          .submitMovementReferenceNumberAndDeclaration(displayDeclaration.getMRN, displayDeclaration)
          .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
          .flatMap(
            _.submitDuplicateMovementReferenceNumberAndDeclaration(
              duplicateDeclaration.getMRN,
              duplicateDeclaration.withConsigneeEori(eori)
            )
          )
          .getOrFail

        val updatedJourney = journey.checkConsigneeEoriNumberWithDuplicateDeclaration(eori).getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(SessionData(updatedJourney))(Right(()))
        }

        checkIsRedirect(
          performAction(controller.eoriNumberFormKey -> eori.value),
          routes.EnterDeclarantEoriNumberOfDuplicateDeclarationController.show
        )
      }

      "submit a valid Eori which is not the consignee" in forAll {
        (mrn: MRN, enteredConsigneeEori: Eori, wantedConsignee: Eori) =>
          whenever(enteredConsigneeEori =!= wantedConsignee) {
            val displayDeclaration                 =
              buildDisplayDeclaration(consigneeEORI = Some(exampleEori)).withDeclarationId(mrn.value)
            val journey: OverpaymentsSingleJourney = OverpaymentsSingleJourney
              .empty(exampleEori)
              .submitMovementReferenceNumberAndDeclaration(displayDeclaration.getMRN, displayDeclaration)
              .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
              .flatMap(
                _.submitDuplicateMovementReferenceNumberAndDeclaration(
                  duplicateDeclaration.getMRN,
                  duplicateDeclaration.withDeclarantEori(wantedConsignee)
                )
              )
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
            }

            checkPageIsDisplayed(
              performAction(controller.eoriNumberFormKey -> enteredConsigneeEori.value),
              messageFromMessageKey("enter-importer-eori-number.title"),
              doc => {
                getErrorSummary(doc)                              shouldBe messageFromMessageKey(
                  "enter-importer-eori-number.eori-should-match-importer"
                )
                doc.select("#enter-importer-eori-number").`val`() shouldBe enteredConsigneeEori.value
              },
              expectedStatus = BAD_REQUEST
            )
          }
      }
    }
  }
}
