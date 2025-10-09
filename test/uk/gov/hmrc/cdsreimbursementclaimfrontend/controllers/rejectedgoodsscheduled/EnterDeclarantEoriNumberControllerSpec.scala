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

import org.scalatest.BeforeAndAfterEach
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsScheduledClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarantDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.Future

class EnterDeclarantEoriNumberControllerSpec
    extends ControllerSpec
    with ClaimTestData
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with OptionValues {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterDeclarantEoriNumberController = instanceOf[EnterDeclarantEoriNumberController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val session = SessionData(
    RejectedGoodsScheduledClaim
      .empty(anotherExampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleDisplayDeclaration.getMRN, exampleDisplayDeclaration)
      .getOrFail
  )

  "Declarant Eori Number Controller" when {
    "Enter Declarant Eori page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "display the page on a new claim" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc => {
            doc.select("form div#enter-declarant-eori-number-hint").text() shouldBe messageFromMessageKey(
              "enter-declarant-eori-number.help-text"
            )
            doc.select("#enter-declarant-eori-number").`val`()             shouldBe ""
            doc.select("form").attr("action")                              shouldBe routes.EnterDeclarantEoriNumberController.submit.url
          }
        )
      }

      "redirect to the upload mrn list page when eori check not needed (user eori is declarant eori)" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(
            SessionData(
              RejectedGoodsScheduledClaim
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
          routes.UploadMrnListController.show
        )
      }

      "display the page on a pre-existing claim" in forAll(
        buildCompleteClaimGen(
          acc14DeclarantMatchesUserEori = false,
          acc14ConsigneeMatchesUserEori = false
        )
      ) { claim =>
        val eori           = claim.answers.eoriNumbersVerification.flatMap(_.declarantEoriNumber).value
        val sessionToAmend = SessionData(claim)

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

      "reject an empty Eori" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-declarant-eori-number.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid Eori" in {
        val invalidEori = Eori("INVALID_EORI")

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
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

      "submit a valid Eori which is the Consignee Eori" in forAll {
        (mrn: MRN, eori: Eori, declaration: DisplayDeclaration, details: DeclarantDetails) =>
          val initialClaim = session.rejectedGoodsScheduledClaim.value

          val displayDeclaration            = declaration.withDeclarationId(mrn.value)
          val declarantDetails              = details.copy(declarantEORI = eori.value)
          val updatedDisplayResponseDetails =
            displayDeclaration.displayResponseDetail.copy(declarantDetails = declarantDetails)
          val updatedDisplayDeclaration     = displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)
          val claim                         =
            initialClaim
              .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
              .getOrFail

          val requiredSession = SessionData(claim)
          val updatedClaim    = claim.submitDeclarantEoriNumber(eori)
          val updatedSession  = session.copy(rejectedGoodsScheduledClaim = updatedClaim.toOption)

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
        (mrn: MRN, enteredDeclarantEori: Eori, wantedDeclarant: Eori, declaration: DisplayDeclaration) =>
          whenever(enteredDeclarantEori !== wantedDeclarant) {
            val initialClaim = session.rejectedGoodsScheduledClaim.value

            val displayDeclaration            = declaration.withDeclarationId(mrn.value)
            val declarantDetails              = displayDeclaration.getDeclarantDetails.copy(declarantEORI = wantedDeclarant.value)
            val updatedDisplayResponseDetails =
              displayDeclaration.displayResponseDetail.copy(declarantDetails = declarantDetails)
            val updatedDisplayDeclaration     =
              displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)
            val claim                         =
              initialClaim
                .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
                .getOrFail

            val requiredSession = SessionData(claim)

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(requiredSession)
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
