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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled_v2

import org.jsoup.nodes.Document
import org.scalamock.handlers.CallHandler1
import org.scalamock.handlers.CallHandler2
import org.scalatest.Assertion
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.OverpaymentsScheduledClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genCaseNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.jdk.CollectionConverters._
import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ClaimantInformationSummary
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayResponseDetail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo

class CheckYourAnswersControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryInspectionAddress {

  val mockConnector: OverpaymentsScheduledClaimConnector     = mock[OverpaymentsScheduledClaimConnector]
  val mockUploadDocumentsConnector: UploadDocumentsConnector = mock[UploadDocumentsConnector]

  def mockSubmitClaim(submitClaimRequest: OverpaymentsScheduledClaimConnector.Request)(
    response: Future[OverpaymentsScheduledClaimConnector.Response]
  ): CallHandler2[OverpaymentsScheduledClaimConnector.Request, HeaderCarrier, Future[
    OverpaymentsScheduledClaimConnector.Response
  ]] =
    (mockConnector
      .submitClaim(_: OverpaymentsScheduledClaimConnector.Request)(_: HeaderCarrier))
      .expects(submitClaimRequest, *)
      .returning(response)

  def mockWipeOutCall(): CallHandler1[HeaderCarrier, Future[Unit]] =
    (mockUploadDocumentsConnector
      .wipeOut(_: HeaderCarrier))
      .expects(*)
      .returning(Future.successful(()))

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[OverpaymentsScheduledClaimConnector].toInstance(mockConnector),
      bind[UploadDocumentsConnector].toInstance(mockUploadDocumentsConnector)
    )

  val controller: CheckYourAnswersController = instanceOf[CheckYourAnswersController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  def validateCheckYourAnswersPage(
    doc: Document,
    journey: OverpaymentsScheduledJourney,
    claim: OverpaymentsScheduledJourney.Output
  ) = {
    val headers       = doc.select("h2.govuk-heading-m").eachText().asScala.toSeq
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala).toSeq

    headers       should not be empty
    summaryKeys   should not be empty
    summaryValues should not be empty

    headers should containOnlyDefinedElementsOf(
      "First Movement Reference Number (MRN)".expectedAlways,
      "Scheduled document".expectedAlways,
      "Declaration details".expectedAlways,
      "Contact information for this claim".expectedAlways,
      "Were your goods imported into Northern Ireland?".expectedAlways,
      "Basis for claim".expectedAlways,
      "Reason for claim".expectedAlways,
      "Total repayment claim for all MRNs".expectedAlways,
      "Bank details".expectedWhen(claim.bankAccountDetails),
      "Supporting documents".expectedAlways,
      "Now send your claim".expectedAlways
    )

    val declaration: Option[DisplayDeclaration]           = journey.answers.displayDeclaration
    val declarationDetails: Option[DisplayResponseDetail] = declaration.map(_.displayResponseDetail)

    val expectedDocuments: Seq[String] =
      journey.answers.supportingEvidences.map { uploadDocument =>
        s"${uploadDocument.fileName} ${uploadDocument.documentType.fold("")(documentType =>
          messages(s"supporting-evidence.choose-document-type.document-type.${UploadDocumentType.keyOf(documentType)}")
        )}"
      }

    summaries should containOnlyDefinedPairsOf(
      Seq(
        "First MRN"                                       -> Some(claim.movementReferenceNumber.value),
        "Scheduled document"                              -> Some(claim.scheduledDocument.fileName),
        "Import date"                                     -> declarationDetails.map(_.acceptanceDate),
        "Duties paid"                                     -> declaration.map(_.totalDutiesPaidCharges.toPoundSterlingString),
        "VAT paid"                                        -> declaration.map(_.totalVatPaidCharges.toPoundSterlingString),
        "Importer name"                                   -> declaration.flatMap(_.consigneeName),
        "Importer email"                                  -> declaration.flatMap(_.consigneeEmail),
        "Importer telephone"                              -> declaration.flatMap(_.consigneeTelephone),
        "Importer address"                                -> declaration.flatMap(_.consigneeAddress).map(_.replace("<br />", " ")),
        "Declarant name"                                  -> declaration.map(_.declarantName),
        "Declarant address"                               -> declaration.flatMap(_.declarantContactAddress).map(_.replace("<br />", " ")),
        "Contact details"                                 -> Some(ClaimantInformationSummary.getContactDataString(claim.claimantInformation)),
        "Contact address"                                 -> Some(ClaimantInformationSummary.getAddressDataString(claim.claimantInformation)),
        "This is the basis behind the claim"              -> Some(
          m(s"select-basis-for-claim.reason.d${BasisOfOverpaymentClaimsList.indexOf(claim.basisOfClaim)}")
        ),
        "Were your goods imported into Northern Ireland?" -> Some(YesNo.of(claim.whetherNorthernIreland).toString),
        "This is the reason for the claim"                -> Some(claim.additionalDetails),
        "EU Duty"                                         -> journey.getEUDutyReimbursementTotal.map(_.toPoundSterlingString),
        "UK Duty"                                         -> journey.getUKDutyReimbursementTotal.map(_.toPoundSterlingString),
        "Excise Duty"                                     -> journey.getExciseDutyReimbursementTotal.map(_.toPoundSterlingString),
        "Total"                                           -> Some(journey.getTotalReimbursementAmount.toPoundSterlingString),
        "Uploaded"                                        -> (if (expectedDocuments.isEmpty) None else Some(expectedDocuments.mkString(" "))),
        "Name on the account"                             -> claim.bankAccountDetails.map(_.accountName.value),
        "Sort code"                                       -> claim.bankAccountDetails.map(_.sortCode.masked(messages)),
        "Account number"                                  -> claim.bankAccountDetails.map(_.accountNumber.masked(messages))
      )
    )
  }

  def validateConfirmationPage(doc: Document, caseNumber: String): Assertion =
    doc.select(".cds-wrap-content--forced").text shouldBe caseNumber

  "Check Your Answers Controller" when {

    "Show check your answers page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if journey has complete answers" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(overpaymentsScheduledJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"check-your-answers.title"),
            doc => validateCheckYourAnswersPage(doc, journey, claim)
          )
        }
      }

      "redirect to the proper page if any answer is missing" in {
        val journey =
          OverpaymentsScheduledJourney
            .empty(exampleEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
            .getOrFail

        val errors: Seq[String] = journey.toOutput.left.getOrElse(Seq.empty)

        val updatedSession = SessionData.empty.copy(overpaymentsScheduledJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the submission confirmation page if journey already finalized" in {
        forAll(completeJourneyGen) { journey =>
          val updatedSession =
            SessionData.empty.copy(overpaymentsScheduledJourney =
              journey.finalizeJourneyWith("dummy case reference").toOption
            )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.showConfirmation)
        }

      }
    }

    "Submitted the valid claim" must {

      def performAction(): Future[Result] = controller.submit()(FakeRequest())

      "redirect to the confirmation page if success" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(overpaymentsScheduledJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(OverpaymentsScheduledClaimConnector.Request(claim))(
              Future.successful(OverpaymentsScheduledClaimConnector.Response("foo-123-abc"))
            )
            mockWipeOutCall()
            mockStoreSession(
              updatedSession.copy(overpaymentsScheduledJourney =
                journey.finalizeJourneyWith("foo-123-abc").toOption.orElse(Some(journey))
              )
            )(Right(()))
          }
          val result         = performAction()
          checkIsRedirect(result, routes.CheckYourAnswersController.showConfirmation)
        }
      }

      "show failure page if submission fails" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(overpaymentsScheduledJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(OverpaymentsScheduledClaimConnector.Request(claim))(
              Future.failed(new Exception("blah"))
            )
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("submit-claim-error.title")
          )
        }
      }
    }

    "Show confirmation page" must {

      def performAction(): Future[Result] = controller.showConfirmation(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if journey has been finalized" in {
        forAll(completeJourneyGen, genCaseNumber) { (journey, caseNumber) =>
          val updatedSession =
            SessionData.empty.copy(overpaymentsScheduledJourney = journey.finalizeJourneyWith(caseNumber).toOption)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmation-of-submission.title"),
            doc => validateConfirmationPage(doc, caseNumber)
          )
        }
      }

      "redirect to the check your answers page if journey not yet finalized" in {
        forAll(completeJourneyGen) { journey =>
          val updatedSession = SessionData.empty.copy(overpaymentsScheduledJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.show)
        }

      }
    }
  }

}
