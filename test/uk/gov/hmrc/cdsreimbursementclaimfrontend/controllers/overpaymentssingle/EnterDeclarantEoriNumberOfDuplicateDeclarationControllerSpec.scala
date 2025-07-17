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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.DuplicateEntry
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.Future

class EnterDeclarantEoriNumberOfDuplicateDeclarationControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterDeclarantEoriNumberOfDuplicateDeclarationController =
    instanceOf[EnterDeclarantEoriNumberOfDuplicateDeclarationController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val originalDeclaration  = exampleDisplayDeclaration
  val duplicateDeclaration = buildDisplayDeclaration(id = anotherExampleMrn.value)

  val journey: OverpaymentsSingleJourney = OverpaymentsSingleJourney
    .empty(originalDeclaration.getDeclarantEori)
    .submitMovementReferenceNumberAndDeclaration(originalDeclaration.getMRN, originalDeclaration)
    .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
    .flatMap(
      _.submitDuplicateMovementReferenceNumberAndDeclaration(
        duplicateDeclaration.getMRN,
        duplicateDeclaration
      )
    )
    .getOrFail

  "Declarant Eori Number of Duplicate Declaration Controller" when {
    "Enter Declarant Eori Number of Duplicate Declaration page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "display the page on a new journey" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc => {
            doc
              .select("form div#enter-declarant-eori-number-hint")
              .text()                                          shouldBe messageFromMessageKey("enter-declarant-eori-number.help-text")
            doc.select("#enter-declarant-eori-number").`val`() shouldBe ""
            doc
              .select("form")
              .attr("action")                                  shouldBe routes.EnterDeclarantEoriNumberOfDuplicateDeclarationController.submit.url
          }
        )
      }

      "redirect to enter additional details when eori check not needed (user eori is declarant eori)" in {
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
                .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
                .flatMap(
                  _.submitDuplicateMovementReferenceNumberAndDeclaration(
                    duplicateDeclaration.getMRN,
                    duplicateDeclaration.withDeclarantEori(exampleDisplayDeclaration.getDeclarantEori)
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
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc => {
            doc
              .select("form div#enter-declarant-eori-number-hint")
              .text()                                          shouldBe messageFromMessageKey("enter-declarant-eori-number.help-text")
            doc.select("#enter-declarant-eori-number").`val`() shouldBe ""
          }
        )
      }
    }

    "Submit Declarant Eori  page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "reject an empty Eori" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-declarant-eori-number.error.required"),
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
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc => {
            getErrorSummary(doc)                               shouldBe messageFromMessageKey("enter-declarant-eori-number.invalid.number")
            doc.select("#enter-declarant-eori-number").`val`() shouldBe invalidEori.value
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "submit a valid Eori which is the declarant Eori" in forAll { (mrn: MRN, eori: Eori) =>
        val displayDeclaration                 = buildDisplayDeclaration().withDeclarationId(mrn.value).withDeclarantEori(exampleEori)
        val journey: OverpaymentsSingleJourney = OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(displayDeclaration.getMRN, displayDeclaration)
          .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
          .flatMap(
            _.submitDuplicateMovementReferenceNumberAndDeclaration(
              duplicateDeclaration.getMRN,
              duplicateDeclaration.withDeclarantEori(eori)
            )
          )
          .getOrFail
        val updatedJourney                     = journey.checkDeclarantEoriNumberWithDuplicateDeclaration(eori).getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(SessionData(updatedJourney))(Right(()))
        }

        checkIsRedirect(
          performAction(controller.eoriNumberFormKey -> eori.value),
          routes.CheckDuplicateDeclarationDetailsController.show
        )
      }

      "submit a valid Eori which is not the declarant" in forAll {
        (mrn: MRN, enteredDeclarantEori: Eori, wantedDeclarant: Eori) =>
          whenever(enteredDeclarantEori =!= wantedDeclarant) {
            val displayDeclaration                 =
              buildDisplayDeclaration().withDeclarationId(mrn.value).withDeclarantEori(exampleEori)
            val journey: OverpaymentsSingleJourney = OverpaymentsSingleJourney
              .empty(exampleEori)
              .submitMovementReferenceNumberAndDeclaration(displayDeclaration.getMRN, displayDeclaration)
              .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
              .flatMap(
                _.submitDuplicateMovementReferenceNumberAndDeclaration(
                  duplicateDeclaration.getMRN,
                  duplicateDeclaration.withDeclarantEori(wantedDeclarant)
                )
              )
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
            }

            checkPageIsDisplayed(
              performAction(controller.eoriNumberFormKey -> enteredDeclarantEori.value),
              messageFromMessageKey("enter-declarant-eori-number.title"),
              doc => {
                getErrorSummary(doc)                               shouldBe messageFromMessageKey(
                  "enter-declarant-eori-number.eori-should-match-declarant"
                )
                doc.select("#enter-declarant-eori-number").`val`() shouldBe enteredDeclarantEori.value
              },
              expectedStatus = BAD_REQUEST
            )
          }
      }
    }
  }
}
