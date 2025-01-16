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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.RejectedGoodsSingleClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
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

  val mockConnector: RejectedGoodsSingleClaimConnector       = mock[RejectedGoodsSingleClaimConnector]
  val mockUploadDocumentsConnector: UploadDocumentsConnector = mock[UploadDocumentsConnector]

  def mockSubmitClaim(submitClaimRequest: RejectedGoodsSingleClaimConnector.Request)(
    response: Future[RejectedGoodsSingleClaimConnector.Response]
  ): CallHandler2[RejectedGoodsSingleClaimConnector.Request, HeaderCarrier, Future[
    RejectedGoodsSingleClaimConnector.Response
  ]] =
    (mockConnector
      .submitClaim(_: RejectedGoodsSingleClaimConnector.Request)(_: HeaderCarrier))
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
      bind[RejectedGoodsSingleClaimConnector].toInstance(mockConnector),
      bind[UploadDocumentsConnector].toInstance(mockUploadDocumentsConnector)
    )

  val controller: CheckYourAnswersController = instanceOf[CheckYourAnswersController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "check-your-answers"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  def validateCheckYourAnswersPage(
    doc: Document,
    journey: RejectedGoodsSingleJourney,
    claim: RejectedGoodsSingleJourney.Output,
    isSubsidy: Boolean = false
  ) = {
    val headers       = doc.select("h2.govuk-heading-m").eachText().asScala
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

    headers       should not be empty
    summaryKeys   should not be empty
    summaryValues should not be empty

    headers.toSeq should containOnlyDefinedElementsOf(
      "Movement Reference Number (MRN)".expectedWhen(!isSubsidy),
      "Movement Reference Number (MRN) - Subsidy".expectedWhen(isSubsidy),
      "Declaration details".expectedAlways,
      "Contact details for this claim".expectedAlways,
      "Claim details".expectedAlways,
      "Claim total".expectedAlways,
      "Details of inspection".expectedAlways,
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
          "Importer name"            -> declaration.flatMap(_.consigneeName),
          "Importer email"           -> declaration.flatMap(_.consigneeEmail),
          "Importer telephone"       -> declaration.flatMap(_.consigneeTelephone),
          "Importer address"         -> declaration.flatMap(_.consigneeAddress).map(_.replace("<br>", " ")),
          "Declarant name"           -> declaration.map(_.declarantName),
          "Declarant address"        -> declaration.flatMap(_.declarantContactAddress).map(_.replace("<br>", " ")),
          "Contact details"          -> Some(ClaimantInformationSummary.getContactDataString(claim.claimantInformation)),
          "Contact address"          -> Some(ClaimantInformationSummary.getAddressDataString(claim.claimantInformation)),
          "Basis of claim"           -> Some(
            m(s"select-basis-for-claim.rejected-goods.reason.${claim.basisOfClaim}")
          ),
          "Special circumstances"    -> claim.basisOfClaimSpecialCircumstances,
          "Disposal method"          -> Some(
            messages(s"select-method-of-disposal.rejected-goods.method.${claim.methodOfDisposal}")
          ),
          "Additional claim details" -> Some(claim.detailsOfRejectedGoods),
          "Inspection date"          -> Some(claim.inspectionDate.checkYourDetailsDisplayFormat),
          "Inspection address type"  -> Some(
            messages(s"inspection-address.type.${claim.inspectionAddress.addressType}")
          ),
          "Inspection address"       -> Some(summaryAddress(claim.inspectionAddress, " ")),
          "Total"                    -> Some(journey.getTotalReimbursementAmount.toPoundSterlingString),
          "Payee"                    ->
            Some(claim.payeeType match {
              case PayeeType.Consignee => m("check-your-answers.payee-type.importer")
              case PayeeType.Declarant => m("check-your-answers.payee-type.declarant")
            }),
          "Method"                   ->
            Some(claim.reimbursementMethod match {
              case ReimbursementMethod.CurrentMonthAdjustment => m("check-your-answers.repayment-method.cma")
              case ReimbursementMethod.Subsidy                => m("check-your-answers.repayment-method.subsidy")
              case _                                          => m("check-your-answers.repayment-method.bt")
            }),
          "Uploaded"                 -> (if expectedDocuments.isEmpty then None else Some(expectedDocuments.mkString(" "))),
          "Name on the account"      -> claim.bankAccountDetails.map(_.accountName.value),
          "Sort code"                -> claim.bankAccountDetails.map(_.sortCode.value),
          "Account number"           -> claim.bankAccountDetails.map(_.accountNumber.value)
        )
        ++ claim.reimbursements.map(r => messages(s"tax-code.${r.taxCode}") -> Some(r.amount.toPoundSterlingString))
    )
  }

  def validateConfirmationPage(doc: Document, journey: RejectedGoodsSingleJourney, caseNumber: String): Assertion = {

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

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if journey has complete answers" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateCheckYourAnswersPage(doc, journey, claim, journey.isSubsidyOnlyJourney)
          )
        }
      }

      "display subsidy status when declaration has only subsidy payments" in {
        forAll(
          buildCompleteJourneyGen(
            acc14DeclarantMatchesUserEori = false,
            acc14ConsigneeMatchesUserEori = false,
            generateSubsidyPayments = GenerateSubsidyPayments.All,
            features = Some(
              RejectedGoodsSingleJourney.Features(shouldBlockSubsidies = false, shouldAllowSubsidyOnlyPayments = true)
            ),
            submitBankAccountDetails = false,
            submitBankAccountType = false
          )
        ) { j =>
          val journey        = j.resetReimbursementMethod().submitCheckYourAnswersChangeMode(true)
          val claim          = journey.toOutput.fold(error => fail(s"cannot get output of the journey: $error"), x => x)
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => {
              validateCheckYourAnswersPage(doc, journey, claim, isSubsidy = true)
              doc
                .select(".govuk-summary-list__row dt.govuk-summary-list__key")
                .get(2)
                .text() shouldBe "Method of payment"
            }
          )
        }
      }

      "redirect if any subsidy payment in the declaration when subsidies are blocked" in {
        val journey =
          buildCompleteJourneyGen(
            generateSubsidyPayments = GenerateSubsidyPayments.Some,
            features = Some(
              RejectedGoodsSingleJourney.Features(shouldBlockSubsidies = true, shouldAllowSubsidyOnlyPayments = false)
            )
          ).sample.getOrElse(fail())

        val errors: Seq[String] = journey.toOutput.left.getOrElse(Seq.empty)

        val updatedSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the proper page if any answer is missing" in {
        val journey =
          RejectedGoodsSingleJourney
            .empty(exampleEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
            .getOrFail

        val errors: Seq[String] = journey.toOutput.left.getOrElse(Seq.empty)

        val updatedSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the submission confirmation page if journey already finalized" in {
        forAll(completeJourneyGen) { journey =>
          val updatedSession =
            SessionData.empty.copy(rejectedGoodsSingleJourney =
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
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(RejectedGoodsSingleClaimConnector.Request(claim))(
              Future.successful(RejectedGoodsSingleClaimConnector.Response("foo-123-abc"))
            )
            mockWipeOutCall()
            mockStoreSession(
              updatedSession.copy(rejectedGoodsSingleJourney =
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
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(RejectedGoodsSingleClaimConnector.Request(claim))(
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
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if journey has been finalized" in {
        forAll(completeJourneyGen, genCaseNumber) { (journey, caseNumber) =>
          val updatedSession =
            SessionData.empty.copy(rejectedGoodsSingleJourney = journey.finalizeJourneyWith(caseNumber).toOption)
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
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))
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
