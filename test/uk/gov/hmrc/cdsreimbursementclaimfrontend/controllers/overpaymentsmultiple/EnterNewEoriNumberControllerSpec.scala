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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

import org.scalacheck.Gen
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.EoriDetailsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectEoriAndDan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class EnterNewEoriNumberControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  val mockConnector = mock[EoriDetailsConnector]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[EoriDetailsConnector].toInstance(mockConnector)
    )

  val controller: EnterNewEoriNumberController = instanceOf[EnterNewEoriNumberController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val session: SessionData = SessionData(
    OverpaymentsMultipleClaim
      .empty(anotherExampleEori)
      .submitMovementReferenceNumberAndDeclaration(exampleDisplayDeclaration.getMRN, exampleDisplayDeclaration)
      .getOrFail
  )

  val claimGen: Gen[OverpaymentsMultipleClaim] =
    buildClaimFromAnswersGen(answersUpToBasisForClaimGen())

  def mockGetEoriDetails(eori: Eori)(
    response: Future[Option[connectors.EoriDetailsConnector.Response]]
  ) =
    (mockConnector
      .getEoriDetails(_: Eori)(_: HeaderCarrier))
      .expects(eori, *)
      .returning(response)

  "New Eori Number Controller" when {
    "Enter New Eori page" must {

      def performAction(): Future[Result] =
        controller.show(FakeRequest())

      "display the page on a new claim" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-new-eori-number.title"),
          doc => {
            doc.getElementById("enter-new-eori-inset-text").text()  shouldBe messageFromMessageKey(
              "enter-new-eori-number.inset-text"
            )
            doc.getElementById("enter-new-eori-number-hint").text() shouldBe messageFromMessageKey(
              "enter-new-eori-number.hint"
            )
            doc.select("#enter-new-eori-number").`val`()            shouldBe ""
            doc.select("form").attr("action")                       shouldBe routes.EnterNewEoriNumberController.submit.url
          }
        )
      }
    }

    "Submit New Eori" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "reject an empty Eori" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-new-eori-number.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey("enter-new-eori-number.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an invalid Eori" in {
        val invalidEori = Eori("invalid-eori")

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(controller.formKey -> invalidEori.value),
          messageFromMessageKey("enter-new-eori-number.title"),
          doc => {
            getErrorSummary(doc)                         shouldBe messageFromMessageKey("enter-new-eori-number.invalid.number")
            doc.select("#enter-new-eori-number").`val`() shouldBe invalidEori.value
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "submit a valid Eori format but new eori does not start with GB" in forAll(
        claimGen.flatMap(j => j.submitBasisOfClaim(IncorrectEoriAndDan))
      ) { claim =>
        val eori = Eori("FR123456123456")
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkPageIsDisplayed(
          performAction(controller.formKey -> eori.value),
          messageFromMessageKey("enter-new-eori-number.title"),
          doc => {
            getErrorSummary(doc)                         shouldBe messageFromMessageKey("enter-new-eori-number.mustStartWithGB")
            doc.select("#enter-new-eori-number").`val`() shouldBe eori.value
          },
          expectedStatus = BAD_REQUEST
        )

      }

      "submit a valid Eori format, importer has XI eori but new eori starts with GB" in forAll(
        buildClaimFromAnswersGen(answersUpToBasisForClaimGen(currentUserEoriNumber = IdGen.genXiEori)).flatMap(j =>
          j.submitBasisOfClaim(IncorrectEoriAndDan)
        )
      ) { claim =>
        val eori = Eori("GB123456123456")
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkPageIsDisplayed(
          performAction(controller.formKey -> eori.value),
          messageFromMessageKey("enter-new-eori-number.title"),
          doc => {
            getErrorSummary(doc)                         shouldBe messageFromMessageKey("enter-new-eori-number.mustNotStartWithGB")
            doc.select("#enter-new-eori-number").`val`() shouldBe eori.value
          },
          expectedStatus = BAD_REQUEST
        )

      }

      "submit a valid Eori format but eori does not exist" in forAll(
        claimGen.flatMap(j => j.submitBasisOfClaim(IncorrectEoriAndDan))
      ) { claim =>
        val eori = Eori("GB123456123456")
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockGetEoriDetails(eori)(Future.successful(None))
        }

        checkPageIsDisplayed(
          performAction(controller.formKey -> eori.value),
          messageFromMessageKey("enter-new-eori-number.title"),
          doc => {
            getErrorSummary(doc)                         shouldBe messageFromMessageKey("enter-new-eori-number.doesNotExist")
            doc.select("#enter-new-eori-number").`val`() shouldBe eori.value
          },
          expectedStatus = BAD_REQUEST
        )

      }

      "submit a valid Eori - eori exists" in forAll(
        claimGen.flatMap(j => j.submitBasisOfClaim(IncorrectEoriAndDan))
      ) { claim =>
        val eori = Eori("GB123456123456")
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockGetEoriDetails(eori)(
            Future.successful(
              Some(
                EoriDetailsConnector
                  .Response(
                    eoriGB = Eori("GB0123456789"),
                    eoriXI = Some(Eori("XI0123456789")),
                    fullName = "Foo Bar",
                    eoriEndDate = None
                  )
              )
            )
          )
          mockStoreSession(SessionData(claim.submitNewEori(eori)))(Right(()))
        }

        checkIsRedirect(
          performAction(controller.formKey -> eori.value),
          routes.EnterNewDanController.show
        )

      }
    }
  }
}
