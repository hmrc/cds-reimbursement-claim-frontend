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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v2

import org.jsoup.nodes.Document
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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

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

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  def getHintText(document: Document, hintTextId: String) = {
    val hintTextElement = document.select(s"div#$hintTextId")

    if (hintTextElement.hasText) Some(hintTextElement.html()) else None
  }

  private val messagesKey: String = "select-duties"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  def assertPageContent(
    doc: Document,
    journey: OverpaymentsSingleJourney,
    selectedDuties: Seq[String]
  ): Unit = {
    selectedCheckBox(doc) should contain theSameElementsAs selectedDuties
    checkboxes(doc)       should containOnlyPairsOf(
      journey.getAvailableDuties.map { case (taxCode, _) =>
        (taxCode.value + " - " + messages(s"select-duties.duty.$taxCode"), taxCode.value)
      }
    )
  }

  val journeyGen: Gen[OverpaymentsSingleJourney] =
    buildJourneyGen(answersUpToBasisForClaimGen(forcedTaxCodes = Seq(TaxCode.NI411)))
      .flatMap(j =>
        Gen
          .oneOf(j.getAvailableClaimTypes)
          .map(b => j.submitBasisOfClaim(b))
      )

  val journeyWithNIExciseCodesGen: Gen[OverpaymentsSingleJourney] =
    buildJourneyGen(
      answersUpToBasisForClaimGen(taxCodes = TaxCodes.excise, forcedTaxCodes = Seq(TaxCode.A00, TaxCode.B05))
    )
      .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.IncorrectExciseValue))

  "Select Duties Controller" when {

    "Show select duties page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page the first time" in forAll(journeyGen) { journey =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          assertPageContent(_, journey, Seq.empty)
        )
      }

      "display the page the first time when NI excise code in ACC14 and IncorrectExciseValue" in forAll(
        journeyWithNIExciseCodesGen
      ) { journey =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          assertPageContent(_, journey, Seq.empty)
        )
      }

      "display the page when a tax code has already been selected before" in {
        forAll(completeJourneyGen) { journey =>
          val selectedDuties: Seq[String] =
            journey.getSelectedDuties.map(_.map(_.value)).getOrElse(Seq.empty)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            assertPageContent(_, journey, selectedDuties)
          )
        }
      }
    }

    "Submit Select Duties page" must {
      def performAction(data: Seq[(String, String)] = Seq.empty): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty tax code selection" in {

        val journey = OverpaymentsSingleJourney
          .empty(exampleDisplayDeclaration.getDeclarantEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
          .getOrFail

        val updatedSession = SessionData.empty.copy(overpaymentsSingleJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          performAction(Seq("select-duties" -> "")),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messagesKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "select valid tax codes when none have been selected before" in {
        forAll(displayDeclarationGen) { displayDeclaration =>
          val initialJourney = OverpaymentsSingleJourney
            .empty(displayDeclaration.getDeclarantEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
            .getOrFail

          val availableTaxCodes = displayDeclaration.getAvailableTaxCodes
          val selectedTaxCodes  =
            if (availableTaxCodes.size > 1) availableTaxCodes.drop(1)
            else availableTaxCodes

          val initialSession = SessionData.empty.copy(overpaymentsSingleJourney = Some(initialJourney))

          val updatedJourney = initialJourney.selectAndReplaceTaxCodeSetForReimbursement(selectedTaxCodes)
          val updatedSession = SessionData.empty.copy(overpaymentsSingleJourney = updatedJourney.toOption)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(initialSession)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(selectedTaxCodes.map(taxCode => s"select-duties[]" -> taxCode.value)),
            routes.EnterClaimController.showFirst
          )
        }
      }

    }

    "have CMA Eligible flag/Duties hint text" should {
      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "Acc14 excise code where the CMA eligible flag is true" in {

        val displayDeclaration = buildDisplayDeclaration(
          dutyDetails = Seq(
            (TaxCode.A80, BigDecimal("200.00"), true),
            (TaxCode.A95, BigDecimal("171.05"), false)
          )
        )

        val journey = OverpaymentsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, displayDeclaration)
          .getOrFail

        val updatedSession = SessionData.empty.copy(overpaymentsSingleJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        val hintText =
          Some("This duty is not eligible for CMA repayment.")

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            getHintText(doc, "select-duties-item-hint")   shouldBe None
            getHintText(doc, "select-duties-2-item-hint") shouldBe hintText
          }
        )
      }

    }
  }
}
