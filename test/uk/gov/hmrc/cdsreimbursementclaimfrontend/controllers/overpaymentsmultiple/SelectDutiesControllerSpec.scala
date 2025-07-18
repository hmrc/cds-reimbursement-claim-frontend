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

import org.jsoup.nodes.Document
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsMultipleJourney

import scala.concurrent.Future

class SelectDutiesControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: SelectDutiesController = instanceOf[SelectDutiesController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "select-duties"

  def validateSelectDutiesPage(
    doc: Document,
    pageIndex: Int,
    mrn: MRN,
    taxCodes: Seq[TaxCode],
    selectedTaxCodes: Seq[TaxCode]
  ) = {
    checkboxes(doc)       should contain theSameElementsAs taxCodes.map(tc =>
      (s"${tc.value} - ${messages(s"$messagesKey.duty.${tc.value}")}", tc.value)
    )
    selectedCheckBox(doc) should contain theSameElementsAs selectedTaxCodes.map(_.value)
    val mrnElement = doc.select("span#MRN")
    mrnElement.text()        shouldBe mrn.value
    mrnElement.attr("class") shouldBe "govuk-!-font-weight-bold"
    hasContinueButton(doc)

    formAction(doc) shouldBe s"/claim-back-import-duty-vat/overpayments/multiple/select-duties/$pageIndex"
  }

  "SelectDutiesController" when {

    "Show duties selection for the first MRN" must {

      def performAction(): Future[Result] = controller.showFirst()(FakeRequest())

      "display the page with no duty selected" in {
        forAll(incompleteJourneyWithMrnsGen(2)) { case (journey, mrns) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          val displayedTaxCodes: Seq[TaxCode] =
            journey.getAvailableDuties(mrns.head).map(_._1)

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateSelectDutiesPage(doc, 1, mrns.head, displayedTaxCodes, Seq.empty)
          )
        }
      }

      "display the page with some duties selected" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(2)) { case (journey, _) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          val mrn: MRN = journey.getLeadMovementReferenceNumber.get

          val displayedTaxCodes: Seq[TaxCode] =
            journey.getAvailableDuties(mrn).map(_._1)

          val selectedTaxCodes: Seq[TaxCode] =
            journey.getSelectedDuties(mrn).get

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateSelectDutiesPage(doc, 1, mrn, displayedTaxCodes, selectedTaxCodes)
          )
        }
      }

      "display the page with some duties selected when in change mode" in {
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          val mrn: MRN = journey.getLeadMovementReferenceNumber.get

          val displayedTaxCodes: Seq[TaxCode] =
            journey.getAvailableDuties(mrn).map(_._1)

          val selectedTaxCodes: Seq[TaxCode] =
            journey.getSelectedDuties(mrn).get

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateSelectDutiesPage(doc, 1, mrn, displayedTaxCodes, selectedTaxCodes)
          )
        }
      }
    }

    "Show duties selection for the nth MRN" must {

      def performAction(pageIndex: Int): Future[Result] =
        controller.show(pageIndex)(FakeRequest())

      "display the page with no duty selected" in {
        forAll(incompleteJourneyWithMrnsGen(5)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
            }

            val displayedTaxCodes: Seq[TaxCode] =
              journey.getAvailableDuties(mrn).map(_._1)

            checkPageIsDisplayed(
              performAction(mrnIndex + 1),
              messageFromMessageKey(s"$messagesKey.title"),
              doc => validateSelectDutiesPage(doc, mrnIndex + 1, mrn, displayedTaxCodes, Seq.empty)
            )
          }
        }
      }

      "display the page with some duties selected" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(2)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
            }

            val displayedTaxCodes: Seq[TaxCode] =
              journey.getAvailableDuties(mrn).map(_._1)

            val selectedTaxCodes: Seq[TaxCode] =
              journey.getSelectedDuties(mrn).get

            checkPageIsDisplayed(
              performAction(mrnIndex + 1),
              messageFromMessageKey(s"$messagesKey.title"),
              doc => validateSelectDutiesPage(doc, mrnIndex + 1, mrn, displayedTaxCodes, selectedTaxCodes)
            )
          }
        }
      }

      "display the page with some duties selected when in change mode" in {
        forAll(completeJourneyGen) { journey =>
          journey.answers.movementReferenceNumbers.get.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
            }

            val displayedTaxCodes: Seq[TaxCode] =
              journey.getAvailableDuties(mrn).map(_._1)

            val selectedTaxCodes: Seq[TaxCode] =
              journey.getSelectedDuties(mrn).get

            checkPageIsDisplayed(
              performAction(mrnIndex + 1),
              messageFromMessageKey(s"$messagesKey.title"),
              doc => validateSelectDutiesPage(doc, mrnIndex + 1, mrn, displayedTaxCodes, selectedTaxCodes)
            )
          }
        }
      }

      "redirect to MRN not found page when page index is out of bounds" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(2)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
            }
            checkPageIsDisplayed(
              performAction(100),
              "This Movement Reference Number (MRN) does not exist",
              expectedStatus = BAD_REQUEST
            )
          }
        }
      }

      "redirect to ineligible when no duties are available" in {
        val displayResponseDetailWithoutDuties      =
          exampleDisplayDeclaration.displayResponseDetail.copy(ndrcDetails = None)
        val displayResponseDeclarationWithoutDuties = exampleDisplayDeclaration.copy(displayResponseDetailWithoutDuties)
        val journey                                 = OverpaymentsMultipleJourney
          .tryBuildFrom(
            OverpaymentsMultipleJourney.Answers(
              userEoriNumber = exampleEori,
              movementReferenceNumbers = Some(Seq(exampleMrn, anotherExampleMrn)),
              displayDeclarations = Some(
                Seq(
                  exampleDisplayDeclaration
                    .withDeclarationId(exampleMrn.value)
                    .withDeclarantEori(exampleEori),
                  displayResponseDeclarationWithoutDuties
                    .withDeclarationId(anotherExampleMrn.value)
                    .withDeclarantEori(exampleEori)
                )
              )
            )
          )
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }
        checkIsRedirect(
          performAction(2),
          baseRoutes.IneligibleController.ineligible.url
        )
      }

    }

    "Submit duties selection" must {

      def performAction(pageIndex: Int, selectedTaxCodes: Seq[TaxCode]): Future[Result] =
        controller.submit(pageIndex)(
          FakeRequest()
            .withFormUrlEncodedBody(selectedTaxCodes.map(taxCode => s"$messagesKey[]" -> taxCode.value)*)
        )

      "redirect to enter first claim for the MRN when no duty selected before" in {
        forAll(incompleteJourneyWithMrnsGen(5)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val displayedTaxCodes: Seq[TaxCode] =
              journey.getAvailableDuties(mrn).map(_._1)

            val selectedTaxCodes: Seq[TaxCode] =
              displayedTaxCodes.take(Math.max(1, displayedTaxCodes.size / 2))

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
              mockStoreSession(
                SessionData(
                  journey
                    .selectAndReplaceTaxCodeSetForReimbursement(mrn, selectedTaxCodes.sorted)
                    .getOrFail
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(mrnIndex + 1, selectedTaxCodes),
              s"/claim-back-import-duty-vat/overpayments/multiple/enter-claim/${mrnIndex + 1}/${selectedTaxCodes.head.value}"
            )
          }
        }
      }

      "redirect to enter first claim for the MRN when the same duties already selected" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(2)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
            }

            val selectedTaxCodes: Seq[TaxCode] = journey.getSelectedDuties(mrn).get

            checkIsRedirect(
              performAction(mrnIndex + 1, selectedTaxCodes),
              s"/claim-back-import-duty-vat/overpayments/multiple/enter-claim/${mrnIndex + 1}/${selectedTaxCodes.head.value}"
            )
          }
        }
      }

      "redirect to enter first claim for the MRN when some duties already selected" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(5)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            val selectedTaxCodes                  = journey.getSelectedDuties(mrn).get
            val newSelectedTaxCodes: Seq[TaxCode] = selectedTaxCodes.drop(1)
            if newSelectedTaxCodes.nonEmpty then {
              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(journey))
                mockStoreSession(
                  SessionData(
                    journey
                      .selectAndReplaceTaxCodeSetForReimbursement(mrn, newSelectedTaxCodes)
                      .getOrFail
                  )
                )(Right(()))
              }

              checkIsRedirect(
                performAction(mrnIndex + 1, newSelectedTaxCodes),
                s"/claim-back-import-duty-vat/overpayments/multiple/enter-claim/${mrnIndex + 1}/${newSelectedTaxCodes.head.value}"
              )
            }
          }
        }
      }
      "redirect to MRN not found page when page index is out of bounds" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(2)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, mrnIndex) =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
            }
            checkPageIsDisplayed(
              performAction(100, Seq.empty),
              "This Movement Reference Number (MRN) does not exist",
              expectedStatus = BAD_REQUEST
            )
          }
        }
      }

      "show form errors on invalid data" in {
        forAll(incompleteJourneyWithMrnsGen(1)) { case (journey, _) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }
          val result = performAction(1, Seq.empty)
          status(result) shouldBe BAD_REQUEST
        }
      }

      "redirect to ineligible when no duties are available" in {
        val displayResponseDetailWithoutDuties      =
          exampleDisplayDeclaration.displayResponseDetail.copy(ndrcDetails = None)
        val displayResponseDeclarationWithoutDuties = exampleDisplayDeclaration.copy(displayResponseDetailWithoutDuties)
        val journey                                 = OverpaymentsMultipleJourney
          .tryBuildFrom(
            OverpaymentsMultipleJourney.Answers(
              userEoriNumber = exampleEori,
              movementReferenceNumbers = Some(Seq(exampleMrn, anotherExampleMrn)),
              displayDeclarations = Some(
                Seq(
                  exampleDisplayDeclaration
                    .withDeclarationId(exampleMrn.value)
                    .withDeclarantEori(exampleEori),
                  displayResponseDeclarationWithoutDuties
                    .withDeclarationId(anotherExampleMrn.value)
                    .withDeclarantEori(exampleEori)
                )
              )
            )
          )
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction(2, Seq.empty),
          baseRoutes.IneligibleController.ineligible.url
        )
      }
    }
  }
}
