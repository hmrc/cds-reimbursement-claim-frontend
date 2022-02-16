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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

class SelectTaxCodesControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: SelectTaxCodesController = instanceOf[SelectTaxCodesController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = SelectTaxCodesController.selectTaxCodesKey

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  def validateSelectTaxCodesPage(
    doc: Document,
    index: Int,
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
    formAction(doc)          shouldBe s"/claim-for-reimbursement-of-import-duties/rejected-goods/multiple/select-duties/$index"
  }

  "SelectTaxCodesController" when {

    "Show duties selection for the first MRN" must {

      def performAction(): Future[Result] = controller.showFirst()(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page with no duty selected" in {
        forAll(incompleteJourneyWithMrnsGen(2)) { case (journey, mrns) =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          val displayedTaxCodes: Seq[TaxCode] =
            journey.getAvailableDuties(mrns.head).map(_._1)

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.multiple.title", OrdinalNumber.label(1)),
            doc => validateSelectTaxCodesPage(doc, 1, mrns.head, displayedTaxCodes, Seq.empty)
          )
        }
      }

      "display the page with some duties selected" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(2)) { case (journey, mrnsWithTaxCodesSelection) =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          val mrn: MRN = mrnsWithTaxCodesSelection.head._1

          val displayedTaxCodes: Seq[TaxCode] =
            journey.getAvailableDuties(mrn).map(_._1)

          val selectedTaxCodes: Seq[TaxCode] =
            mrnsWithTaxCodesSelection.head._2

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.multiple.title", OrdinalNumber.label(1)),
            doc => validateSelectTaxCodesPage(doc, 1, mrn, displayedTaxCodes, selectedTaxCodes)
          )
        }
      }

      "display the page with some duties selected when in change mode" in {
        forAll(completeJourneyGen) { journey =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          val mrn: MRN = journey.getLeadMovementReferenceNumber.get

          val displayedTaxCodes: Seq[TaxCode] =
            journey.getAvailableDuties(mrn).map(_._1)

          val selectedTaxCodes: Seq[TaxCode] =
            journey.getSelectedDuties(mrn).get

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.multiple.title", OrdinalNumber.label(1)),
            doc => validateSelectTaxCodesPage(doc, 1, mrn, displayedTaxCodes, selectedTaxCodes)
          )
        }
      }
    }

    "Show duties selection for the nth MRN" must {

      def performAction(index: Int): Future[Result] = controller.show(index)(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        for (index <- 1 to 100)
          status(performAction(index)) shouldBe NOT_FOUND
      }

      "display the page with no duty selected" in {
        forAll(incompleteJourneyWithMrnsGen(5)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, index) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(journey))
            }

            val displayedTaxCodes: Seq[TaxCode] =
              journey.getAvailableDuties(mrn).map(_._1)

            checkPageIsDisplayed(
              performAction(index + 1),
              messageFromMessageKey(s"$messagesKey.multiple.title", OrdinalNumber.label(index + 1)),
              doc => validateSelectTaxCodesPage(doc, index + 1, mrn, displayedTaxCodes, Seq.empty)
            )
          }
        }
      }

      "display the page with some duties selected" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(2)) { case (journey, mrnsWithTaxCodesSelection) =>
          mrnsWithTaxCodesSelection.zipWithIndex.foreach { case ((mrn, selectedTaxCodes), index) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(journey))
            }

            val displayedTaxCodes: Seq[TaxCode] =
              journey.getAvailableDuties(mrn).map(_._1)

            checkPageIsDisplayed(
              performAction(index + 1),
              messageFromMessageKey(s"$messagesKey.multiple.title", OrdinalNumber.label(index + 1)),
              doc => validateSelectTaxCodesPage(doc, index + 1, mrn, displayedTaxCodes, selectedTaxCodes)
            )
          }
        }
      }

      "display the page with some duties selected when in change mode" in {
        forAll(completeJourneyGen) { journey =>
          journey.answers.movementReferenceNumbers.get.zipWithIndex.foreach { case (mrn, index) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(journey))
            }

            val displayedTaxCodes: Seq[TaxCode] =
              journey.getAvailableDuties(mrn).map(_._1)

            val selectedTaxCodes: Seq[TaxCode] =
              journey.getSelectedDuties(mrn).get

            checkPageIsDisplayed(
              performAction(index + 1),
              messageFromMessageKey(s"$messagesKey.multiple.title", OrdinalNumber.label(index + 1)),
              doc => validateSelectTaxCodesPage(doc, index + 1, mrn, displayedTaxCodes, selectedTaxCodes)
            )
          }
        }
      }
    }

    "Submit duties selection" must {

      def performAction(index: Int, selectedTaxCodes: Seq[TaxCode]): Future[Result] =
        controller.submit(index)(
          FakeRequest()
            .withFormUrlEncodedBody(selectedTaxCodes.map(taxCode => s"$messagesKey[]" -> taxCode.value): _*)
        )

      "fail if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction(1, Seq.empty)) shouldBe NOT_FOUND
      }

      "redirect to enter first claim for the MRN when no duty selected before" in {
        forAll(incompleteJourneyWithMrnsGen(5)) { case (journey, mrns) =>
          mrns.zipWithIndex.foreach { case (mrn, index) =>
            val displayedTaxCodes: Seq[TaxCode] =
              journey.getAvailableDuties(mrn).map(_._1)

            val selectedTaxCodes: Seq[TaxCode] =
              displayedTaxCodes.take(Math.max(1, displayedTaxCodes.size / 2))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(journey))
              mockStoreSession(
                SessionData(
                  journey
                    .selectAndReplaceTaxCodeSetForReimbursement(mrn, selectedTaxCodes)
                    .getOrFail
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(index + 1, selectedTaxCodes),
              s"/claim-for-reimbursement-of-import-duties/rejected-goods/multiple/enter-claim/${index + 1}/${selectedTaxCodes.head.value}"
            )
          }
        }
      }

      "redirect to enter first claim for the MRN when the same duties already selected" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(2)) { case (journey, mrnsWithTaxCodesSelection) =>
          mrnsWithTaxCodesSelection.zipWithIndex.foreach { case ((_, selectedTaxCodes), index) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(journey))
            }

            checkIsRedirect(
              performAction(index + 1, selectedTaxCodes),
              s"/claim-for-reimbursement-of-import-duties/rejected-goods/multiple/enter-claim/${index + 1}/${selectedTaxCodes.head.value}"
            )
          }
        }
      }

      "redirect to enter first claim for the MRN when some duties already selected" in {
        forAll(incompleteJourneyWithSelectedDutiesGen(5)) { case (journey, mrnsWithTaxCodesSelection) =>
          mrnsWithTaxCodesSelection.zipWithIndex.foreach { case ((mrn, selectedTaxCodes), index) =>
            val newSelectedTaxCodes: Seq[TaxCode] = selectedTaxCodes.drop(1)
            if (newSelectedTaxCodes.nonEmpty) {
              inSequence {
                mockAuthWithNoRetrievals()
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
                performAction(index + 1, newSelectedTaxCodes),
                s"/claim-for-reimbursement-of-import-duties/rejected-goods/multiple/enter-claim/${index + 1}/${newSelectedTaxCodes.head.value}"
              )
            }
          }
        }
      }

    }
  }

}
