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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterInspectionDateForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.buildCompleteJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.buildJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.displayDeclarationGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.exampleEori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.exampleMrn
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DateGen.genDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class EnterInspectionDateControllerSpec
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

  val controller: EnterInspectionDateController = instanceOf[EnterInspectionDateController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch  = instanceOf[FeatureSwitchService]
  private val messagesKey: String = "enter-inspection-date.rejected-goods"

  def performAction(): Future[Result] =
    controller.show()(FakeRequest())

  def test(
    sessionData: SessionData,
    expectedTitleKey: String,
    expectedPrepopulatedValue: Option[InspectionDate]
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(sessionData)
    }

    checkPageIsDisplayed(
      performAction(),
      messageFromMessageKey(expectedTitleKey),
      doc =>
        expectedPrepopulatedValue.foreach { inspectionDate =>
          doc
            .select("#enter-contact-details-rejected-goods.day")
            .attr("value") shouldBe inspectionDate.value.getDayOfMonth.toString
          doc
            .select("#enter-contact-details-rejected-goods.$inputValue.month")
            .attr("value") shouldBe inspectionDate.value.getMonthValue.toString
          doc
            .select("#enter-contact-details-rejected-goods.$inputValue.year")
            .attr("value") shouldBe inspectionDate.value.getYear.toString
        }
    )
  }

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session: SessionData = SessionData.empty.copy(
    rejectedGoodsSingleJourney = Some(RejectedGoodsSingleJourney.empty(exampleEori))
  )

  "Enter Inspection Date Controller" must {

    "not find the page if rejected goods feature is disabled" in {
      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      featureSwitch.disable(Feature.RejectedGoods)

      status(performAction()) shouldBe NOT_FOUND
    }

    "display the page" when {
      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      "the user has not answered this question before" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            doc
              .select("main p")
              .text()          shouldBe messageFromMessageKey(s"$messagesKey.help-text")
            selectedInput(doc) shouldBe empty
          }
        )
      }

      "the user has answered this question before" in forAll(buildCompleteJourneyGen()) { journey =>
        val inspectionDate = journey.answers.inspectionDate
        val updatedSession = session.copy(rejectedGoodsSingleJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => {
            selectedInputBox(doc, "day")   shouldBe Some(inspectionDate.get.value.getDayOfMonth.toString)
            selectedInputBox(doc, "month") shouldBe Some(inspectionDate.get.value.getMonthValue.toString)
            selectedInputBox(doc, "year")  shouldBe Some(inspectionDate.get.value.getYear.toString)
          }
        )
      }
    }

    "handle submit requests" when {
      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "the user enters a date for the first time and Acc14 has returned contact details for the importer or declarant" in forAll(
        displayDeclarationGen,
        genDate
      ) { (displayDeclaration, date) =>
        val initialJourney = RejectedGoodsSingleJourney
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitDisplayDeclaration(displayDeclaration)
        val initialSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(initialJourney))
        val updatedJourney = initialJourney.submitInspectionDate(date)
        val updatedSession = session.copy(rejectedGoodsSingleJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(initialSession)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(
            s"${controller.formKey}.day"   -> date.value.getDayOfMonth.toString,
            s"${controller.formKey}.month" -> date.value.getMonthValue.toString,
            s"${controller.formKey}.year"  -> date.value.getYear.toString
          ),
          "inspection-address/choose-type"
        )
      }

      "the user enters a date for the first time and Acc14 hasn't returned any contact details" in forAll(
        genDate
      ) { date =>
        val journey = RejectedGoodsSingleJourney.empty(exampleEori)

        val updatedJourney = journey.submitInspectionDate(date)
        val updatedSession = session.copy(rejectedGoodsSingleJourney = Some(updatedJourney))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(
            s"${controller.formKey}.day"   -> date.value.getDayOfMonth.toString,
            s"${controller.formKey}.month" -> date.value.getMonthValue.toString,
            s"${controller.formKey}.year"  -> date.value.getYear.toString
          ),
          "inspection-address/.../lookup"
        )
      }

    }

    "show an error summary" when {
      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "the user submits an empty date" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$messagesKey.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }
    }
  }

  "Form Validation" must {
    val form                  = enterInspectionDateForm
    val dateOfInspectionDay   = s"$messagesKey.day"
    val dateOfInspectionMonth = s"$messagesKey.month"
    val dateOfInspectionYear  = s"$messagesKey.year"
    val goodData              = Map(
      dateOfInspectionDay   -> "20",
      dateOfInspectionMonth -> "3",
      dateOfInspectionYear  -> "1987"
    )

    val emptyData = Map(
      dateOfInspectionDay   -> " ",
      dateOfInspectionMonth -> " ",
      dateOfInspectionYear  -> " "
    )

    "Day and month" should {
      "Reject empty day and month" in {
        val errors = form.bind(emptyData.updated(dateOfInspectionYear, "1987")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("dayAndMonth.error.required")
      }
    }

    "Day and year" should {
      "Reject empty day and year" in {
        val errors = form.bind(emptyData.updated(dateOfInspectionMonth, "3")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("dayAndYear.error.required")
      }
    }

    "Month and year" should {
      "Reject empty month and year" in {
        val errors = form.bind(emptyData.updated(dateOfInspectionDay, "20")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("monthAndYear.error.required")
      }
    }

    "inspection date day" should {
      "Reject empty day" in {
        val errors = form.bind(goodData.updated(dateOfInspectionDay, " ")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("day.error.required")
      }

      "Reject days too big" in {
        val errors = form.bind(goodData.updated(dateOfInspectionDay, "32")).errors
        errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
      }

      "Month of Inspection" should {

        "Reject empty month" in {
          val errors = form.bind(goodData.updated(dateOfInspectionMonth, " ")).errors
          errors.headOption.getOrElse(fail()).messages shouldBe List("month.error.required")
        }

        "Reject months too big" in {
          val errors = form.bind(goodData.updated(dateOfInspectionMonth, "13")).errors
          errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
        }

        "Year of Inspection" should {

          "Reject empty year" in {
            val errors = form.bind(goodData.updated(dateOfInspectionYear, " ")).errors
            errors.headOption.getOrElse(fail()).messages shouldBe List("year.error.required")
          }

          "Reject years too early" in {
            val errors = form.bind(goodData.updated(dateOfInspectionYear, "1899")).errors
            errors.headOption.getOrElse(fail()).messages shouldBe List("error.before1900")
          }
          "Reject 3 digits" in {
            val errors = form.bind(goodData.updated(dateOfInspectionYear, "202")).errors
            errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
          }
          "Reject years with chars" in {
            val errors = form.bind(goodData.updated(dateOfInspectionYear, "202a")).errors
            errors.headOption.getOrElse(fail()).messages shouldBe List("error.invalid")
          }
        }
      }
    }
  }

}
