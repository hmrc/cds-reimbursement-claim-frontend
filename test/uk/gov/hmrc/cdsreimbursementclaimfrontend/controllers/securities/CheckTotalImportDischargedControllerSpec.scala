package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import org.jsoup.nodes.Document
import org.scalatest.BeforeAndAfterEach
import play.api.http.Status.NOT_FOUND
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.buildSecuritiesJourneyWithSomeSecuritiesSelected
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.mrnWithNonExportIprOrErRfsWithDisplayDeclarationGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging

import scala.List
import scala.collection.JavaConverters._
import scala.concurrent.Future

class CheckTotalImportDischargedControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with TestWithJourneyGenerator[SecuritiesJourney]
    with SummaryMatchers
    with Logging {
  val mockClaimsService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService)
    )

  val controller: CheckTotalImportDischargedController = instanceOf[CheckTotalImportDischargedController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "check-total-import-discharged"
  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  def validateCheckTotalImportDischargedPage(
    doc: Document,
    journey: SecuritiesJourney,
    isError: Boolean = false
  ) = {
    val header      = doc.select("h1.govuk-heading-xl").eachText().asScala.toList
    val hint        = doc.select("#check-total-import-discharged-hint").eachText().asScala.toList
    val radioValues = doc.select("input.govuk-radios__input").eachAttr("value").asScala.toList
    val radioLabels = doc.select("label.govuk-radios__label").eachText().asScala.toList

    val continueButton = doc.select("button.govuk-button").eachText().asScala.toList

    header         should ===(List(messages(s"$messagesKey.title")))
    hint           should ===(
      List(messages(s"$messagesKey.help-text"))
    )
    radioValues    should ===(List("true", "false"))
    radioLabels    should ===(List(messages("generic.yes"), messages("generic.no")))
    continueButton should ===(List(messages("button.continue")))

    if (isError) {
      val problemHeader  = doc.select("h2.govuk-error-summary__title").eachText().asScala.toList
      val linkToError    = doc.select("a[href=#check-total-import-discharged]").eachText().asScala.toList
      val errorParagraph = doc.select("p#check-total-import-discharged-error").eachText().asScala.toList

      problemHeader  should ===(List(messages("error.summary.title")))
      linkToError    should ===(List(messages(s"$messagesKey.error.required")))
      errorParagraph should ===(List("Error: " + messages(s"$messagesKey.error.required")))
    }
  }

  "CheckTotalImportDischargedController" when {
    "show page" must {
      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) should ===(NOT_FOUND)
      }

      "display page" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithNonExportIprOrErRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelected
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          "Have you discharged 100% of the imported goods?",
          doc => validateCheckTotalImportDischargedPage(doc, journey)
        )
      }
    }

    "submit page" must {
      def performAction(discharged: Option[Boolean]): Future[Result] =
        controller.submit()(
          FakeRequest()
            .withFormUrlEncodedBody("check-total-import-discharged" -> discharged.map(_.toString).getOrElse(""))
        )

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction(Some(true))) shouldBe NOT_FOUND
      }

      "redirect to the next page when yes is selected" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithNonExportIprOrErRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelected
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction(Some(true)),
          routes.CheckClaimantDetailsController.show()
        )
      }

      "redirect to correct error page when no is selected" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithNonExportIprOrErRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelected
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction(Some(false)),
          routes.ClaimInvalidNotExportedAllController.show()
        )
      }

      "stay on the same page and display error message when no option selected" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithNonExportIprOrErRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelected
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(None),
          "Have you discharged 100% of the imported goods?",
          doc => validateCheckTotalImportDischargedPage(doc, journey, isError = true),
          BAD_REQUEST
        )
      }
    }
  }
}
