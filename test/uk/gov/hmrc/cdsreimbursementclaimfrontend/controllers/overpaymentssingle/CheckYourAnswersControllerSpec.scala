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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle

import org.jsoup.nodes.Document
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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.OverpaymentsSingleClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType.Consignee
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType.Declarant
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayResponseDetail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genCaseNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ClaimantInformationSummary
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.MethodOfPaymentSummary
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class CheckYourAnswersControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryInspectionAddress {

  val mockConnector: OverpaymentsSingleClaimConnector        = mock[OverpaymentsSingleClaimConnector]
  val mockUploadDocumentsConnector: UploadDocumentsConnector = mock[UploadDocumentsConnector]

  def mockSubmitClaim(submitClaimRequest: OverpaymentsSingleClaimConnector.Request)(
    response: Future[OverpaymentsSingleClaimConnector.Response]
  ) =
    (mockConnector
      .submitClaim(_: OverpaymentsSingleClaimConnector.Request)(_: HeaderCarrier))
      .expects(submitClaimRequest, *)
      .returning(response)

  def mockWipeOutCall() =
    (mockUploadDocumentsConnector
      .wipeOut(_: HeaderCarrier))
      .expects(*)
      .returning(Future.successful(()))

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[OverpaymentsSingleClaimConnector].toInstance(mockConnector),
      bind[UploadDocumentsConnector].toInstance(mockUploadDocumentsConnector)
    )

  val controller: CheckYourAnswersController = instanceOf[CheckYourAnswersController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "check-your-answers"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Overpayments_v2)

  def validateCheckYourAnswersPage(
    doc: Document,
    journey: OverpaymentsSingleJourney,
    claim: OverpaymentsSingleJourney.Output
  ) = {
    val headers       = doc.select("h2.govuk-heading-m").eachText().asScala
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

    headers       should not be empty
    summaryKeys   should not be empty
    summaryValues should not be empty

    headers.toSeq should containOnlyDefinedElementsOf(
      "Movement Reference Number (MRN)".expectedAlways,
      "Declaration details".expectedAlways,
      "Contact details for this claim".expectedAlways,
      "Claim details".expectedAlways,
      "Claim total".expectedAlways,
      "Repayment details".expectedAlways,
      "Bank details".expectedWhen(claim.bankAccountDetails),
      "Supporting documents".expectedAlways,
      "Now send your claim".expectedAlways
    )

    val declaration: Option[DisplayDeclaration]           = journey.answers.displayDeclaration
    val declarationDetails: Option[DisplayResponseDetail] = declaration.map(_.displayResponseDetail)

    val expectedDocuments: Seq[String] =
      journey.answers.supportingEvidences.map { uploadDocument =>
        s"${uploadDocument.fileName} ${uploadDocument.documentType
            .fold("")(documentType => messages(s"choose-file-type.file-type.${UploadDocumentType.keyOf(documentType)}"))}"
      }

    summaries.toSeq should containOnlyDefinedPairsOf(
      Seq(
        "MRN"               -> Some(claim.movementReferenceNumber.value),
        "Import date"       -> declarationDetails.map(_.acceptanceDate),
        "Method of payment" -> Some(
          MethodOfPaymentSummary(declaration.flatMap(_.getMethodsOfPayment).getOrElse(Set("")))
        ),
        "Duties paid"       -> declaration.map(_.totalDutiesPaidCharges.toPoundSterlingString)
      ) ++
        declaration.flatMap(_.totalVatPaidCharges).map(vat => "VAT paid" -> Some(vat.toPoundSterlingString)).toList ++
        Seq(
          "Importer name"                -> declaration.flatMap(_.consigneeName),
          "Importer email"               -> declaration.flatMap(_.consigneeEmail),
          "Importer telephone"           -> declaration.flatMap(_.consigneeTelephone),
          "Importer address"             -> declaration.flatMap(_.consigneeAddress).map(_.replace("<br>", " ")),
          "Declarant name"               -> declaration.map(_.declarantName),
          "Declarant address"            -> declaration.flatMap(_.declarantContactAddress).map(_.replace("<br>", " ")),
          "Contact details"              -> Some(ClaimantInformationSummary.getContactDataString(claim.claimantInformation)),
          "Contact address"              -> Some(ClaimantInformationSummary.getAddressDataString(claim.claimantInformation)),
          "Basis of claim"               -> Some(
            m(s"select-basis-for-claim.reason.${claim.basisOfClaim}")
          ),
          "Additional claim details"     -> Some(claim.additionalDetails),
          "Duplicate MRN"                -> claim.duplicateMovementReferenceNumber.map(_.value),
          "New EORI"                     -> claim.newEoriAndDan.map(_.eori.value),
          "New deferment account number" -> claim.newEoriAndDan.map(_.dan),
          "Total"                        -> Some(journey.getTotalReimbursementAmount.toPoundSterlingString),
          "Payee"                        ->
            Some(claim.displayPayeeType match {
              case PayeeType.Consignee      => m("choose-payee-type.radio.importer")
              case PayeeType.Declarant      => m("choose-payee-type.radio.declarant")
              case PayeeType.Representative => m("choose-payee-type.radio.representative")
            }),
          "Method"                       ->
            Some(claim.reimbursementMethod match {
              case ReimbursementMethod.CurrentMonthAdjustment => m("check-your-answers.repayment-method.cma")
              case ReimbursementMethod.Subsidy                => m("check-your-answers.repayment-method.subsidy")
              case _                                          => m("check-your-answers.repayment-method.bt")
            }),
          "Uploaded"                     -> (if expectedDocuments.isEmpty then None else Some(expectedDocuments.mkString(" "))),
          "Name on the account"          -> claim.bankAccountDetails.map(_.accountName.value),
          "Sort code"                    -> claim.bankAccountDetails.map(_.sortCode.value),
          "Account number"               -> claim.bankAccountDetails.map(_.accountNumber.value)
        )
        ++ claim.reimbursements.map { r =>
          m(s"tax-code.${r.taxCode}") -> Some(r.amount.toPoundSterlingString)
        }
    )

    claim.payeeType shouldBe getPayeeType(journey.answers.payeeType.get)
  }

  private def getPayeeType(payeeType: PayeeType): PayeeType = payeeType match {
    case Consignee => Consignee
    case _         => Declarant
  }

  def validateConfirmationPage(doc: Document, journey: OverpaymentsSingleJourney, caseNumber: String): Assertion = {

    val mrn = journey.getLeadMovementReferenceNumber.get.value
    mrn.isEmpty shouldBe false

    val claimAmount = journey.getTotalReimbursementAmount.toPoundSterlingString

    summaryKeyValueList(doc)                                          should containOnlyPairsOf(
      Seq(
        messages(s"confirmation-of-submission.reimbursement-amount") -> claimAmount,
        messages(s"confirmation-of-submission.mrn")                  -> mrn
      )
    )
    doc.select("div.govuk-panel__body").text().contains(caseNumber) shouldBe true
  }

  "Check Your Answers Controller" when {

    "Show check your answers page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if journey has complete answers" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(overpaymentsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateCheckYourAnswersPage(doc, journey, claim)
          )
        }
      }

      "display method of payment when declaration has only subsidy payments" in {
        forAll(
          buildCompleteJourneyGen(
            acc14DeclarantMatchesUserEori = false,
            acc14ConsigneeMatchesUserEori = false,
            generateSubsidyPayments = GenerateSubsidyPayments.All,
            features = Some(
              OverpaymentsSingleJourney
                .Features(
                  shouldBlockSubsidies = false,
                  shouldAllowSubsidyOnlyPayments = true,
                  shouldSkipDocumentTypeSelection = false
                )
            ),
            submitBankAccountDetails = false,
            submitBankAccountType = false
          )
        ) { j =>
          val journey        = j.resetReimbursementMethod().submitCheckYourAnswersChangeMode(true)
          val claim          = journey.toOutput.fold(error => fail(s"cannot get output of the journey: $error"), x => x)
          val updatedSession = SessionData.empty.copy(overpaymentsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateCheckYourAnswersPage(doc, journey, claim)
          )
        }
      }

      "redirect if any subsidy payment in the declaration when subsidies are blocked" in {
        val journey =
          buildCompleteJourneyGen(
            generateSubsidyPayments = GenerateSubsidyPayments.Some,
            features = Some(
              OverpaymentsSingleJourney.Features(
                shouldBlockSubsidies = true,
                shouldAllowSubsidyOnlyPayments = false,
                shouldSkipDocumentTypeSelection = false
              )
            )
          ).sample.getOrElse(fail())

        val errors: Seq[String] = journey.toOutput.left.getOrElse(Seq.empty)

        val updatedSession = SessionData.empty.copy(overpaymentsSingleJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the proper page if any answer is missing" in {
        val journey =
          OverpaymentsSingleJourney
            .empty(exampleEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
            .getOrFail

        val errors: Seq[String] = journey.toOutput.left.getOrElse(Seq.empty)

        val updatedSession = SessionData.empty.copy(overpaymentsSingleJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the submission confirmation page if journey already finalized" in {
        forAll(completeJourneyGen) { journey =>
          val updatedSession =
            SessionData.empty.copy(overpaymentsSingleJourney =
              journey.finalizeJourneyWith("dummy case reference").toOption
            )
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.showConfirmation)
        }

      }
    }

    "Submitted the valid claim" must {

      def performAction(): Future[Result] = controller.submit(FakeRequest())

      "redirect to the confirmation page if success" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(overpaymentsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(OverpaymentsSingleClaimConnector.Request(claim))(
              Future.successful(OverpaymentsSingleClaimConnector.Response("foo-123-abc"))
            )
            mockWipeOutCall()
            mockStoreSession(
              updatedSession.copy(overpaymentsSingleJourney =
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
          val updatedSession = SessionData.empty.copy(overpaymentsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(OverpaymentsSingleClaimConnector.Request(claim))(
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

      "not find the page if overpayments feature is disabled" in {
        featureSwitch.disable(Feature.Overpayments_v2)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if journey has been finalized" in {
        forAll(completeJourneyGen, genCaseNumber) { (journey, caseNumber) =>
          val updatedSession =
            SessionData.empty.copy(overpaymentsSingleJourney = journey.finalizeJourneyWith(caseNumber).toOption)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmation-of-submission.title"),
            doc => validateConfirmationPage(doc, journey, caseNumber)
          )
        }
      }

      "redirect to the check your answers page if journey not yet finalized" in {
        forAll(completeJourneyGen) { journey =>
          val updatedSession = SessionData.empty.copy(overpaymentsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.show)
        }

      }
    }
  }

}
