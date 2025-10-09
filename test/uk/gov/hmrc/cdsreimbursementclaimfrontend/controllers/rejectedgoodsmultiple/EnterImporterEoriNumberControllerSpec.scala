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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import cats.implicits.*
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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.ClaimTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData

import scala.concurrent.Future

class EnterImporterEoriNumberControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with ClaimTestData {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterImporterEoriNumberController = instanceOf[EnterImporterEoriNumberController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val session = SessionData(
    RejectedGoodsMultipleClaim
      .empty(anotherExampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleDisplayDeclaration.getMRN, exampleDisplayDeclaration)
      .getOrFail
  )

  "Importer Eori Number Controller" when {
    "Enter Importer Eori page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "display the page on a new claim" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
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
            doc.select("form").attr("action")                 shouldBe routes.EnterImporterEoriNumberController.submit.url
          }
        )
      }

      "redirect to basis of claim selection when eori check not needed (user eori is declarant eori)" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(
            SessionData(
              RejectedGoodsMultipleClaim
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

      "display the page on a pre-existing claim" in {
        val claim          = buildCompleteClaimGen(
          acc14DeclarantMatchesUserEori = false,
          acc14ConsigneeMatchesUserEori = false,
          hasConsigneeDetailsInACC14 = true
        ).sample.getOrElse(
          fail("Unable to generate complete claim")
        )
        val eori           =
          claim.answers.eoriNumbersVerification
            .flatMap(_.consigneeEoriNumber)
            .getOrElse(fail("No consignee eori found"))
        val sessionToAmend = SessionData(claim)

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
            doc.select("#enter-importer-eori-number").`val`() shouldBe eori.value
          }
        )
      }
    }

    "Submit Importer Eori  page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "reject an empty Eori" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
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
          mockGetSession(session)
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

      "submit a valid Eori which is the Consignee Eori" in forAll {
        (
          mrn: MRN,
          eori: Eori,
          initialDisplayDeclaration: DisplayDeclaration,
          initialConsigneeDetails: ConsigneeDetails
        ) =>
          val initialClaim                  = session.rejectedGoodsMultipleClaim.getOrElse(fail("No rejected goods claim"))
          val displayDeclaration            = initialDisplayDeclaration.withDeclarationId(mrn.value)
          val consigneeDetails              = initialConsigneeDetails.copy(consigneeEORI = eori.value)
          val updatedDisplayResponseDetails =
            displayDeclaration.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails))
          val updatedDisplayDeclaration     = displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)
          val claim                         =
            initialClaim
              .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
              .getOrFail

          val requiredSession = SessionData(claim)
          val updatedClaim    = claim.submitConsigneeEoriNumber(eori).getOrElse(fail("Unable to update eori"))
          val updatedSession  = SessionData(updatedClaim)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(requiredSession)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(controller.eoriNumberFormKey -> eori.value),
            routes.EnterDeclarantEoriNumberController.show
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
            val initialClaim                  = session.rejectedGoodsMultipleClaim.getOrElse(fail("No rejected goods claim"))
            val displayDeclaration            = initialDisplayDeclaration.withDeclarationId(mrn.value)
            val updatedConsigneDetails        =
              displayDeclaration.getConsigneeDetails.map(_.copy(consigneeEORI = wantedConsignee.value))
            val updatedDisplayResponseDetails =
              displayDeclaration.displayResponseDetail.copy(consigneeDetails = updatedConsigneDetails)
            val updatedDisplayDeclaration     =
              displayDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetails)
            val claim                         =
              initialClaim
                .submitMovementReferenceNumberAndDeclaration(mrn, updatedDisplayDeclaration)
                .getOrFail
            val requiredSession               = SessionData(claim)

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(requiredSession)
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
