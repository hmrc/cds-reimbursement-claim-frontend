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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesSingleJourneyGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.MissingPreferenceCertificate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithJourneyGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class CheckExportMovementReferenceNumbersControllerSpec
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

  val controller: CheckExportMovementReferenceNumbersController =
    instanceOf[CheckExportMovementReferenceNumbersController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "check-export-movement-reference-numbers"

  def validateCheckExportMovementReferenceNumbersPage(
    doc: Document,
    isError: Boolean = false
  ) = {
    val header      = doc.select("h1").eachText().asScala.toList
    val hint        = doc.select("#check-export-movement-reference-numbers-hint").eachText().asScala.toList
    val radioValues = doc.select("input.govuk-radios__input").eachAttr("value").asScala.toList
    val radioLabels = doc.select("label.govuk-radios__label").eachText().asScala.toList

    val continueButton = doc.select("button.govuk-button").eachText().asScala.toList

    header         should ===(List(messages(s"$messagesKey.title")))
    hint           should ===(
      List(messages(s"$messagesKey.help-text"))
    )
    radioValues    should ===(List("true", "false"))
    radioLabels    should ===(List(messages("generic.yes"), messages("generic.no")))
    continueButton should contain(messages("button.continue"))

    if isError then {
      val problemHeader  = doc.select("h2.govuk-error-summary__title").eachText().asScala.toList
      val linkToError    = doc.select("a[href=#check-export-movement-reference-numbers]").eachText().asScala.toList
      val errorParagraph = doc.select("p#check-export-movement-reference-numbers-error").eachText().asScala.toList

      problemHeader  should ===(List(messages("error.summary.title")))
      linkToError    should ===(List(messages(s"$messagesKey.error.required")))
      errorParagraph should ===(List("Error: " + messages(s"$messagesKey.error.required")))
    }
  }

  "CheckExportMovementReferenceNumbersController" when {
    "show page" must {
      def performAction(): Future[Result] = controller.show(FakeRequest())

      "display page" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithTaRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
            Seq(MRN("19GB03I52858027001"), MRN("19GB03I52858027002"), MRN("19GB03I52858027003"))
          )
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          "Export Movement Reference Numbers (MRNs) added",
          doc => validateCheckExportMovementReferenceNumbersPage(doc)
        )
      }

      "redirect to enter export MRN when export MRNs are empty" in {
        val gen     = mrnWithTaRfsWithDisplayDeclarationGen.sample.getOrElse(fail("Failed to generate journey data"))
        val journey = buildSecuritiesJourneyWithSomeSecuritiesSelectedAndExportedMethodOfDisposal(gen)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterExportMovementReferenceNumberController.showFirst
        )
      }

      "redirect to next page when method of disposal is not exported in multiple or single shipments" in {
        val gen            = mrnWithTaRfsWithDisplayDeclarationGen.sample.getOrElse(fail("Failed to generate journey data"))
        val journey        = buildSecuritiesJourneyWithSomeSecuritiesSelectedAndExportedMethodOfDisposal(gen)
        val updatedJourney = SecuritiesJourney.unsafeModifyAnswers(
          journey,
          answers =>
            answers.copy(temporaryAdmissionMethodsOfDisposal =
              Some(List(TemporaryAdmissionMethodOfDisposal.DeclaredToAFreeZone))
            )
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedJourney))
        }

        checkIsRedirect(
          performAction(),
          routes.ConfirmFullRepaymentController.showFirst
        )
      }

      "redirect to next page when reason for security is not temporary admission" in {
        val gen            = mrnWithTaRfsWithDisplayDeclarationGen.sample.getOrElse(fail("Failed to generate journey data"))
        val journey        = buildSecuritiesJourneyWithSomeSecuritiesSelectedAndExportedMethodOfDisposal(gen)
        val updatedJourney = SecuritiesJourney.unsafeModifyAnswers(
          journey,
          answers => answers.copy(reasonForSecurity = Some(MissingPreferenceCertificate))
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedJourney))
        }

        checkIsRedirect(
          performAction(),
          routes.ConfirmFullRepaymentController.showFirst
        )
      }

      "redirect to choose export method page when method of disposal is not found" in {
        val gen            = mrnWithTaRfsWithDisplayDeclarationGen.sample.getOrElse(fail("Failed to generate journey data"))
        val journey        = buildSecuritiesJourneyWithSomeSecuritiesSelectedAndExportedMethodOfDisposal(gen)
        val updatedJourney = SecuritiesJourney.unsafeModifyAnswers(
          journey,
          answers => answers.copy(temporaryAdmissionMethodsOfDisposal = None)
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedJourney))
        }

        checkIsRedirect(
          performAction(),
          routes.ChooseExportMethodController.show
        )
      }
    }

    "submit page" must {
      def performAction(addAnotherMRN: Option[Boolean]): Future[Result] =
        controller.submit(
          FakeRequest()
            .withFormUrlEncodedBody(
              "check-export-movement-reference-numbers" -> addAnotherMRN.map(_.toString).getOrElse("")
            )
        )

      "redirect to enter next export MRN form when yes" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithTaRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
            Seq(MRN("19GB03I52858027001"), MRN("19GB03I52858027002"), MRN("19GB03I52858027003"))
          )
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction(Some(true)),
          routes.EnterExportMovementReferenceNumberController.showNext(4)
        )
      }

      "redirect to choose payee type page when no is selected and has single security" in {
        forAllWith(
          JourneyGenerator(
            testParamsGenerator = SecuritiesSingleJourneyGenerators.mrnWithTaRfsWithDisplayDeclarationGen,
            journeyBuilder = SecuritiesSingleJourneyGenerators
              .buildSecuritiesJourneyWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
                Seq(MRN("19GB03I52858027001"), MRN("19GB03I52858027002"), MRN("19GB03I52858027003"))
              )
          )
        ) { case (journey, _) =>
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            performAction(Some(false)),
            routes.ChoosePayeeTypeController.show
          )
        }
      }

      "redirect to confirm full repayment page when no is selected and has multiple securities" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithTaRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
            Seq(MRN("19GB03I52858027001"), MRN("19GB03I52858027002"), MRN("19GB03I52858027003"))
          )
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction(Some(false)),
          routes.ConfirmFullRepaymentController.showFirst
        )
      }

      "redirect to check your answers page when no is selected and has complete journey" in {
        val journey = buildCompleteJourneyGen(reasonsForSecurity = Set(ReasonForSecurity.ntas.head)).sample
          .getOrElse(fail("Failed to create journey"))
          .submitTemporaryAdmissionMethodsOfDisposal(
            TemporaryAdmissionMethodOfDisposal.exportedMethodsOfDisposal.toList
          )
          .flatMap(_.submitExportMovementReferenceNumber(0, MRN("19GB03I52858027001")))
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction(Some(false)),
          routes.CheckYourAnswersController.show
        )
      }

      "stay on the same page and display error message when no option selected" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithTaRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
            Seq(MRN("19GB03I52858027001"), MRN("19GB03I52858027002"), MRN("19GB03I52858027003"))
          )
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(None),
          "Export Movement Reference Numbers (MRNs) added",
          doc => validateCheckExportMovementReferenceNumbersPage(doc, isError = true),
          BAD_REQUEST
        )
      }
    }

    "delete" must {
      def performAction(mrn: MRN): Future[Result] =
        controller.delete(mrn)(
          FakeRequest()
        )

      "delete and redirect to check export MRNs when remaining export MRNs is not empty" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithTaRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
            Seq(MRN("19GB03I52858027001"), MRN("19GB03I52858027002"), MRN("19GB03I52858027003"))
          )
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(
            SessionData(journey.removeExportMovementReferenceNumber(MRN("19GB03I52858027001")).getOrFail)
          )(Right(()))
        }

        checkIsRedirect(
          performAction(MRN("19GB03I52858027001")),
          routes.CheckExportMovementReferenceNumbersController.show
        )
      }

      "delete and redirect to choose export method when there are no remaining export MRNs set" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithTaRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
            Seq(MRN("19GB03I52858027001"))
          )
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
          mockStoreSession(
            SessionData(journey.removeExportMovementReferenceNumber(MRN("19GB03I52858027001")).getOrFail)
          )(Right(()))
        }

        checkIsRedirect(
          performAction(MRN("19GB03I52858027001")),
          routes.ChooseExportMethodController.show
        )
      }

      "redirect to check export MRNs page when the export MRN to be deleted isn't in the export MRN list" in forAllWith(
        JourneyGenerator(
          testParamsGenerator = mrnWithTaRfsWithDisplayDeclarationGen,
          journeyBuilder = buildSecuritiesJourneyWithSomeSecuritiesSelectedAndExportedMethodOfDisposalAndSomeExportMRNs(
            Seq(MRN("19GB03I52858027001"), MRN("19GB03I52858027002"))
          )
        )
      ) { case (journey, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(
          performAction(MRN("19GB03I52858027003")),
          routes.CheckExportMovementReferenceNumbersController.show
        )
      }
    }
  }
}
