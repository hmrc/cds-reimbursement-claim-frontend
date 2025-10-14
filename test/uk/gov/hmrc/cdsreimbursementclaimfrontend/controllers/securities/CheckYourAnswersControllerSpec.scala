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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.SecuritiesClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.InwardProcessingRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType.Consignee
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType.Declarant
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genCaseNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ClaimantInformationSummary
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesSingleClaimGenerators

class CheckYourAnswersControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryInspectionAddress
    with SummaryMatchers
    with Logging {

  val mockConnector: SecuritiesClaimConnector                = mock[SecuritiesClaimConnector]
  val mockUploadDocumentsConnector: UploadDocumentsConnector = mock[UploadDocumentsConnector]

  def mockSubmitClaim(submitClaimRequest: SecuritiesClaimConnector.Request)(
    response: Future[SecuritiesClaimConnector.Response]
  ) =
    (mockConnector
      .submitClaim(_: SecuritiesClaimConnector.Request, _: Boolean)(_: HeaderCarrier))
      .expects(submitClaimRequest, *, *)
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
      bind[SecuritiesClaimConnector].toInstance(mockConnector),
      bind[UploadDocumentsConnector].toInstance(mockUploadDocumentsConnector)
    )

  val controller: CheckYourAnswersController = instanceOf[CheckYourAnswersController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "check-your-answers"

  def validateCheckYourAnswersPage(
    doc: Document,
    claim: SecuritiesClaim,
    output: SecuritiesClaim.Output,
    isPrintView: Boolean = false
  ) = {

    val numberOfSecurities: Int = claim.getLeadImportDeclaration.map(_.getNumberOfSecurityDeposits).getOrElse(0)

    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

    summaryKeys   should not be empty
    summaryValues should not be empty

    if numberOfSecurities > 1
    then {

      val headers = doc.select("h2.govuk-heading-m").eachText().asScala

      headers should not be empty

      val commonHeaders = Seq(
        "Declaration details".expectedAlways,
        "Claim details".expectedWhen(claim.answers.temporaryAdmissionMethodsOfDisposal),
        "Contact details for this claim".expectedAlways,
        "Bank details".expectedWhen(output.bankAccountDetails),
        "Supporting documents".expectedAlways,
        "Additional details".expectedAlways,
        "Now send your claim".expectedWhen(!isPrintView)
      )
      val claimsHeaders =
        output.securitiesReclaims.keys.map(sid =>
          s"Claim details for security deposit or guarantee ${claim.getLeadImportDeclaration
              .map(d => d.getSecurityDepositIdIndex(sid) + 1)
              .getOrElse(0)} of $numberOfSecurities".expectedAlways
        )

      headers.toSeq should containOnlyDefinedElementsOf(
        (if output.reasonForSecurity == InwardProcessingRelief then commonHeaders else commonHeaders ++ claimsHeaders)*
      )

      val expectedDocuments: Seq[String] =
        claim.answers.supportingEvidences
          .map { uploadDocument =>
            s"${uploadDocument.fileName} ${uploadDocument.documentType
                .fold("")(documentType => messages(s"choose-file-type.file-type.${UploadDocumentType.keyOf(documentType)}"))}"
          }

      val expectedBillOfDischarge: Seq[String] =
        claim.answers.billOfDischargeDocuments
          .map(uploadDocument => s"${uploadDocument.fileName}")

      val expectedProofOfOrigin: Seq[String] =
        claim.answers.proofOfOriginDocuments
          .map(uploadDocument => s"${uploadDocument.fileName}")

      val validateSecurityReclaims = claim.answers.importDeclaration
        .flatMap(_.getSecurityDepositIds)
        .getOrElse(Seq.empty)
        .zipWithIndex
        .map { (sid, securityIndex) =>
          s"Claim for security deposit or guarantee ${securityIndex + 1} of $numberOfSecurities" -> Some(
            if output.securitiesReclaims.contains(sid)
            then "Yes"
            else "No"
          )
        } ++
        output.securitiesReclaims.flatMap { case (sid, reclaims) =>
          Seq(
            "Claim full amount" -> Some(
              if claim.answers.importDeclaration
                  .map(_.isFullSecurityAmount(sid, reclaims.values.sum))
                  .getOrElse(false)
              then "Yes"
              else "No"
            ),
            "Duties selected"   -> Some(
              reclaims.keys.toList.sorted
                .map(taxCode => messages(s"tax-code.$taxCode"))
                .mkString(" ")
            ),
            "Total"             -> Some(
              reclaims.values.sum.toPoundSterlingString
            )
          ) ++
            reclaims.map { case (taxCode, amount) =>
              messages(s"tax-code.$taxCode") -> Some(amount.toPoundSterlingString)
            }
        }

      summaries.toSeq should containAllDefinedPairsOf(
        Seq(
          "Import Movement Reference Number (MRN)" -> Some(output.movementReferenceNumber.value),
          // ("Export MRN"                   -> claim.answers.exportMovementReferenceNumber.map(_.map(_.value))),
          "Contact details"                        -> Some(ClaimantInformationSummary.getContactDataString(output.claimantInformation)),
          "Contact address"                        -> Some(ClaimantInformationSummary.getAddressDataString(output.claimantInformation)),
          "Uploaded"                               ->
            (if claim.needsAddOtherDocuments then None
             else Some(expectedDocuments.mkString(" "))),
          "Bill of discharge 3"                    ->
            (if output.reasonForSecurity == ReasonForSecurity.InwardProcessingRelief
             then Some(expectedBillOfDischarge.mkString(" "))
             else None),
          "Bill of discharge 4"                    ->
            (if output.reasonForSecurity == ReasonForSecurity.EndUseRelief
             then Some(expectedBillOfDischarge.mkString(" "))
             else None),
          "Proof of origin"                        ->
            (if ReasonForSecurity.nidac.contains(output.reasonForSecurity)
             then Some(expectedProofOfOrigin.mkString(" "))
             else None),
          "Other supporting documents"             ->
            (if claim.needsAddOtherDocuments
             then Some(expectedDocuments.mkString(" "))
             else None),
          "Do you want to provide more detail?"    -> output.additionalDetails.map(_.value),
          "Name on the account"                    -> output.bankAccountDetails.map(_.accountName.value),
          "Sort code"                              -> output.bankAccountDetails.map(_.sortCode.value),
          "Account number"                         -> output.bankAccountDetails.map(_.accountNumber.value),
          "Reason for security deposit"            -> claim.answers.reasonForSecurity.map(rfs =>
            messages(s"choose-reason-for-security.securities.${ReasonForSecurity.keyOf(rfs)}")
          ),
          "Date security deposit made"             -> claim.answers.importDeclaration
            .flatMap(_.displayResponseDetail.btaDueDate)
            .flatMap(DateUtils.displayFormat),
          "Total security deposit value"           -> claim.answers.importDeclaration
            .map(_.getTotalSecuritiesAmountFor(output.securitiesReclaims.keySet).toPoundSterlingString),
          "Payee"                                  -> output.displayPayeeType.map {
            case PayeeType.Consignee      => m("choose-payee-type.radio.importer")
            case PayeeType.Declarant      => m("choose-payee-type.radio.declarant")
            case PayeeType.Representative => m("choose-payee-type.radio.representative")
          },
          "Payment method"                         -> Some(
            if claim.answers.importDeclaration
                .map(_.isAllSelectedSecuritiesEligibleForGuaranteePayment(output.securitiesReclaims.keySet))
                .getOrElse(false)
            then "Guarantee"
            else "Bank account transfer"
          )
        ) ++ (if output.reasonForSecurity == InwardProcessingRelief then Seq.empty else validateSecurityReclaims)
      )

      output.payeeType shouldBe claim.answers.payeeType.map(getPayeeType)
    } else {

      val cardTitles = doc.select("h2.govuk-summary-card__title").eachText().asScala
      cardTitles       should not be empty
      cardTitles.toSeq should containOnlyDefinedElementsOf(
        "Submission details".expectedWhen(isPrintView),
        "Claim details".expectedAlways,
        "Claim amount".expectedWhen(claim.answers.correctedAmounts.isDefined),
        "Repayment details".expectedAlways,
        "Disposal details".expectedWhen(claim.answers.temporaryAdmissionMethodsOfDisposal.isDefined),
        "Additional details".expectedAlways,
        "Supporting documents".expectedAlways,
        "Contact details for this claim".expectedAlways
      )

    }
  }

  private def getPayeeType(payeeType: PayeeType): PayeeType = payeeType match {
    case Consignee => Consignee
    case _         => Declarant
  }

  def validateConfirmationPage(doc: Document, claim: SecuritiesClaim, caseNumber: String): Assertion = {

    val mrn = claim.getLeadMovementReferenceNumber.get.value
    mrn.isEmpty shouldBe false

    val claimAmount = claim.getTotalClaimAmount.toPoundSterlingString

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

      "display the page if claim has complete answers" in {
        forAll(completeClaimGen) { claim =>
          val output = claim.toOutput.getOrElse(fail("cannot get output of the claim"))

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateCheckYourAnswersPage(doc, claim, output)
          )
        }
      }

      "display the page if claim has a single security and complete answers" in {
        forAll(SecuritiesSingleClaimGenerators.completeClaimGen) { claim =>
          val output = claim.toOutput.getOrElse(fail("cannot get output of the claim"))

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateCheckYourAnswersPage(doc, claim, output)
          )
        }
      }

      "redirect to the proper page if any answer is missing" in {
        val claim =
          SecuritiesClaim
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(
              exampleSecuritiesImportDeclaration.getReasonForSecurity.get,
              exampleSecuritiesImportDeclaration
            )
            .getOrFail

        val errors: Seq[String] = claim.toOutput.left.getOrElse(Seq.empty)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the submission confirmation page if claim already finalized" in {
        forAll(completeClaimGen, genCaseNumber) { (claim, caseNumber) =>

          val updatedClaim = claim.finalizeClaimWith(caseNumber).getOrElse(fail("Failed to finalize claim"))

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(updatedClaim))
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.showConfirmation)
        }
      }
    }

    "Submitted the valid claim" must {

      def performAction(): Future[Result] = controller.submit(FakeRequest())

      "redirect to the confirmation page if success" in {
        forAll(completeClaimGen, genCaseNumber) { (claim, caseNumber) =>
          val output = claim.toOutput.getOrElse(fail("cannot get output of the claim"))

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockSubmitClaim(SecuritiesClaimConnector.Request(output))(
              Future.successful(SecuritiesClaimConnector.Response(caseNumber))
            )
            mockWipeOutCall()
            mockStoreSession(
              SessionData(claim.finalizeClaimWith(caseNumber).getOrElse(fail("Failed to finalize claim")))
            )(Right(()))
          }
          val result = performAction()
          checkIsRedirect(result, routes.CheckYourAnswersController.showConfirmation)
        }
      }

      "show failure page if submission fails" in {
        forAll(completeClaimGen) { claim =>
          val output = claim.toOutput.getOrElse(fail("cannot get output of the claim"))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockSubmitClaim(SecuritiesClaimConnector.Request(output))(
              Future.failed(new Exception("blah"))
            )
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("submit-claim-error.title")
          )
        }
      }

      "redirect to the proper page if any answer is missing" in {
        val claim =
          SecuritiesClaim
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(
              exampleSecuritiesImportDeclaration.getReasonForSecurity.get,
              exampleSecuritiesImportDeclaration
            )
            .getOrFail

        val errors: Seq[String] = claim.toOutput.left.getOrElse(Seq.empty)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }
    }

    "Show confirmation page" must {

      def performAction(): Future[Result] = controller.showConfirmation(FakeRequest())

      "display the page if claim has been finalized" in {
        forAll(completeClaimGen, genCaseNumber) { (claim, caseNumber) =>
          val updatedClaim = claim.finalizeClaimWith(caseNumber).getOrElse(fail("Failed to finalize claim"))

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(updatedClaim))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmation-of-submission.title"),
            doc => validateConfirmationPage(doc, claim, caseNumber)
          )
        }
      }

      "redirect to the check your answers page if claim not yet finalized" in {
        forAll(completeClaimGen) { claim =>

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.show)
        }
      }
    }

    "Show print page" must {

      def performAction(): Future[Result] = controller.showPrintView(FakeRequest())

      "display the page if claim has complete answers" in {
        forAll(completeClaimGen, genCaseNumber) { (claim, caseNumber) =>
          val output       = claim.toOutput.getOrElse(fail("cannot get output of the claim"))
          val updatedClaim = claim.finalizeClaimWith(caseNumber).getOrElse(fail("cannot submit case number"))

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(updatedClaim))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.print-view.title"),
            doc => validateCheckYourAnswersPage(doc, claim, output, true)
          )
        }
      }

      "display the page if claim has a single security and complete answers" in {
        forAll(SecuritiesSingleClaimGenerators.completeClaimGen, genCaseNumber) { (claim, caseNumber) =>
          val output       = claim.toOutput.getOrElse(fail("cannot get output of the claim"))
          val updatedClaim = claim.finalizeClaimWith(caseNumber).getOrElse(fail("cannot submit case number"))

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(updatedClaim))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.print-view.title"),
            doc => validateCheckYourAnswersPage(doc, claim, output, true)
          )
        }
      }

      "display error page when claim is not finalized with a case number" in {
        val claim = completeClaimGen.sample.getOrElse(fail("Failed to create claim"))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsTechnicalErrorPage(performAction())
      }

      "redirect to the proper page if any answer is missing" in {
        val claim =
          SecuritiesClaim
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(
              exampleSecuritiesImportDeclaration.getReasonForSecurity.get,
              exampleSecuritiesImportDeclaration
            )
            .getOrFail

        val errors: Seq[String] = claim.toOutput.left.getOrElse(Seq.empty)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }
    }
  }
}
