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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

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
import play.api.test.Helpers.status
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.buildSecuritiesJourneyWithSomeSecuritiesSelected
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.buildSecuritiesJourneyWithSomeSecuritiesSelectedGeneratedMfd
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.mrnWithRfsTempAdmissionWithDisplayDeclarationWithMfdGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.mrnWithRfsWithDisplayDeclarationGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator

import scala.collection.JavaConverters._
import scala.concurrent.Future

class ChooseExportMethodControllerTests
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryMatchers
    with TestWithJourneyGenerator[SecuritiesJourney] {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: ChooseExportMethodController = instanceOf[ChooseExportMethodController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "choose-export-method"
  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  def validateChooseExportMethodPage(doc: Document, isError: Boolean = false) = {
    val header         = doc.select(".govuk-heading-xl").eachText().asScala
    val radioLabels    = doc.select(".govuk-radios__item label").eachText().asScala
    val radioInputs    = doc.select(".govuk-radios__item input").eachAttr("value").asScala
    val continueButton = doc.select("button.govuk-button").eachText().asScala.toList

    header             should ===(List(messages(s"$messagesKey.title")))
    radioInputs.length should ===(11)
    radioLabels.length should ===(11)
    continueButton     should ===(List(messages("button.continue")))

    if (isError) {
      val problemHeader  = doc.select("h2.govuk-error-summary__title").eachText().asScala.toList
      val linkToError    = doc.select("a[href=#choose-export-method]").eachText().asScala.toList
      val errorParagraph = doc.select("p#choose-export-method-error").eachText().asScala.toList

      problemHeader  should ===(List(messages("error.summary.title")))
      linkToError    should ===(List(messages(s"$messagesKey.error.required")))
      errorParagraph should ===(List("Error: " + messages(s"$messagesKey.error.required")))
    }
  }

  "ChooseExportMethodController" when {

    "show page" must {
      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) should ===(NOT_FOUND)
      }

      "show the page for temporary admissions RfS" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationGen(ReasonForSecurity.temporaryAdmissions.toList),
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelected
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messages(s"$messagesKey.title"),
          doc => validateChooseExportMethodPage(doc)
        )
      }

    }

    "submit page" must {
      def performAction(methodOfDisposal: Option[TemporaryAdmissionMethodOfDisposal]): Future[Result] =
        controller.submit()(
          FakeRequest()
            .withFormUrlEncodedBody(messagesKey -> methodOfDisposal.map(_.toString).getOrElse(""))
        )

      "show an error if no export method is selected" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationGen(ReasonForSecurity.temporaryAdmissions.toList),
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelected
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageWithErrorIsDisplayed(
          performAction(None),
          messages(s"$messagesKey.title"),
          messages(s"$messagesKey.error.required")
        )
      }

      "redirect to /enter-export-movement-reference-number-multiple when single shipment is selected" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsWithDisplayDeclarationGen(ReasonForSecurity.temporaryAdmissions.toList),
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelected
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(Some(TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment)),
          routes.EnterExportMovementReferenceNumberMultipleController.show()
        )
      }

      "redirect to /claimant-details if any option other than" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithRfsTempAdmissionWithDisplayDeclarationWithMfdGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelectedGeneratedMfd
        )
      ) { case (journey, (_, _, _, methodOfDisposal)) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(Some(methodOfDisposal)),
          routes.CheckClaimantDetailsController.show()
        )
      }
    }

  }
}
