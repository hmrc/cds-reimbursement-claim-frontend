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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import cats.{Functor, Id}
import cats.implicits.catsSyntaxEq
import org.jsoup.nodes.Document
import org.scalacheck.Gen
import org.scalacheck.magnolia.gen
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectDutiesController.selectDutiesKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.buildCompleteJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{DisplayDeclaration, NdrcDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode.{A80, A85, A90, A95}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class SelectDutiesControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with RejectedGoodsSingleJourneyTestData {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: SelectDutiesController = instanceOf[SelectDutiesController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private def selectedValue(doc: Document): Option[String] = {
    val checkBoxes = doc.select("div.govuk-checkboxes input[checked]")
    if (checkBoxes.size() =!= 0)
      Some(checkBoxes.`val`())
    else
      None
  }

  def getHintText(document: Document, hintTextId: String) = {
    val hintTextElement = document.select(s"div#$hintTextId")

    if (hintTextElement.hasText) Some(hintTextElement.text()) else None
  }

  private val messagesKey: String = "select-duties"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session = SessionData.empty.copy(
    rejectedGoodsSingleJourney = Some(RejectedGoodsSingleJourney.empty(exampleEori))
  )

  "Select Duties Controller" when {

    "Show select duties page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page" in {
        val journey = buildCompleteJourneyGen(
          acc14DeclarantMatchesUserEori = false,
          acc14ConsigneeMatchesUserEori = false,
          hasConsigneeDetailsInACC14 = true
        ).sample.getOrElse(fail("Journey building has failed."))

        val updatedSession = session.copy(rejectedGoodsSingleJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            selectedValue(doc) shouldBe None
          }
        )
      }
    }


    "Submit Select Duties page" must {
      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty duty selection" in {
        val displayDeclaration = sample[DisplayDeclaration]
        val journey = session.rejectedGoodsSingleJourney.get.submitDisplayDeclaration(displayDeclaration)
        val updatedSession = session.copy(rejectedGoodsSingleJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          performAction(selectDutiesKey -> ""),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messagesKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      //    "select valid tax codes when none have been selected before" in {
      //      val journey = session.rejectedGoodsSingleJourney.getOrElse(fail("No rejected goods journey"))
      //      val displayDeclaration = sample[DisplayDeclaration]
      //     val taxCodes = ???
      //      val updatedJourney            = session.rejectedGoodsSingleJourney.get
      //        .submitDisplayDeclaration(displayDeclaration)
      //        .selectAndReplaceTaxCodeSetForReimbursement(taxCodes)
      //      val updatedSession     = session.copy(rejectedGoodsSingleJourney = Some(updatedJourney.getOrElse(fail("No rejected goods journey"))))
      //
      //      inSequence {
      //        mockAuthWithNoRetrievals()
      //        mockGetSession(session)
      //        mockStoreSession(updatedSession)(Right(()))
      //      }
      //
      //      checkIsRedirect(
      //        performAction(selectDutiesKey -> "true"),
      //        "enter-claim"
      //      )
      //
      //    }

    }

    "have CMA Eligible flag/Duties hint text" should {
      def performAction(): Future[Result] = controller.show()(FakeRequest())

      val taxCodes = List(A80, A85, A90, A95)
      val testNdrcDetail = sample[NdrcDetails]

      val ndrcs: List[NdrcDetails] = List(
        testNdrcDetail.copy(taxType = A80.value, cmaEligible = Some("1")),
        testNdrcDetail.copy(taxType = A85.value, cmaEligible = Some("1")),
        testNdrcDetail.copy(taxType = A90.value, cmaEligible = Some("0")),
        testNdrcDetail.copy(taxType = A95.value, cmaEligible = None)
      )

      val acc14 = Functor[Id].map(sample[DisplayDeclaration])(dd =>
        dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(ndrcDetails = Some(ndrcs)))
      )

      "Acc14 excise code where the CMA eligible flag is true" in {
        val journey = session.rejectedGoodsSingleJourney.getOrElse(fail("No rejected goods journey"))
        val displayDeclaration = ???
        val updatedJourney            = session.rejectedGoodsSingleJourney.get.submitDisplayDeclaration(displayDeclaration)

        val updatedSession = session.copy(rejectedGoodsSingleJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        val hintText = Some("This duty is not eligible for CMA reimbursement")

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            getHintText(doc, "select-duties-item-hint") shouldBe None
            getHintText(doc, "select-duties-2-item-hint") shouldBe None
            getHintText(doc, "select-duties-3-item-hint") shouldBe hintText
            getHintText(doc, "select-duties-4-item-hint") shouldBe hintText
          }
        )
      }


    }
  }
}
