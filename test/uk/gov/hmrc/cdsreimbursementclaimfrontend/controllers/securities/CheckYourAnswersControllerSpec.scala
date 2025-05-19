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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.InwardProcessingRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType.Consignee
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.PayeeType.Declarant
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genCaseNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ClaimantInformationSummary
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

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
      .submitClaim(_: SecuritiesClaimConnector.Request)(_: HeaderCarrier))
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
      bind[SecuritiesClaimConnector].toInstance(mockConnector),
      bind[UploadDocumentsConnector].toInstance(mockUploadDocumentsConnector)
    )

  val controller: CheckYourAnswersController = instanceOf[CheckYourAnswersController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "check-your-answers"

  override def beforeEach(): Unit = {
    featureSwitch.enable(Feature.Securities)
    featureSwitch.disable(Feature.LimitedAccessSecurities)
  }

  def validateCheckYourAnswersPage(
    doc: Document,
    journey: SecuritiesJourney,
    claim: SecuritiesJourney.Output
  ) = {

    val numberOfSecurities: Int = journey.getLeadDisplayDeclaration.map(_.getNumberOfSecurityDeposits).getOrElse(0)

    val headers       = doc.select("h2.govuk-heading-m").eachText().asScala
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

    headers       should not be empty
    summaryKeys   should not be empty
    summaryValues should not be empty

    val commonHeaders = Seq(
      "Declaration details".expectedAlways,
      "Claim details".expectedWhen(journey.answers.temporaryAdmissionMethodsOfDisposal),
      "Contact details for this claim".expectedAlways,
      "Bank details".expectedWhen(claim.bankAccountDetails),
      "Supporting documents".expectedAlways,
      "Additional details".expectedAlways,
      "Now send your claim".expectedAlways
    )
    val claimsHeaders =
      claim.securitiesReclaims.keys.map(sid =>
        s"Claim details for security deposit or guarantee ${journey.getLeadDisplayDeclaration
            .map(d => d.getSecurityDepositIdIndex(sid) + 1)
            .getOrElse(0)} of $numberOfSecurities".expectedAlways
      )

    headers.toSeq should containOnlyDefinedElementsOf(
      (if claim.reasonForSecurity == InwardProcessingRelief then commonHeaders else commonHeaders ++ claimsHeaders)*
    )

    val expectedDocuments: Seq[String] =
      journey.answers.supportingEvidences
        .map { uploadDocument =>
          s"${uploadDocument.fileName} ${uploadDocument.documentType
              .fold("")(documentType => messages(s"choose-file-type.file-type.${UploadDocumentType.keyOf(documentType)}"))}"
        }

    val expectedBillOfDischarge: Seq[String] =
      journey.answers.billOfDischargeDocuments
        .map(uploadDocument => s"${uploadDocument.fileName}")

    val expectedProofOfOrigin: Seq[String] =
      journey.answers.proofOfOriginDocuments
        .map(uploadDocument => s"${uploadDocument.fileName}")

    val validateSecurityReclaims = journey.answers.displayDeclaration
      .flatMap(_.getSecurityDepositIds)
      .getOrElse(Seq.empty)
      .zipWithIndex
      .map { (sid, securityIndex) =>
        s"Claim for security deposit or guarantee ${securityIndex + 1} of $numberOfSecurities" -> Some(
          if claim.securitiesReclaims.contains(sid)
          then "Yes"
          else "No"
        )
      } ++
      claim.securitiesReclaims.flatMap { case (sid, reclaims) =>
        Seq(
          "Claim full amount" -> Some(
            if journey.answers.displayDeclaration
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
        "Import Movement Reference Number (MRN)"     -> Some(claim.movementReferenceNumber.value),
        // ("Export MRN"                   -> journey.answers.exportMovementReferenceNumber.map(_.map(_.value))),
        "Contact details"                            -> Some(ClaimantInformationSummary.getContactDataString(claim.claimantInformation)),
        "Contact address"                            -> Some(ClaimantInformationSummary.getAddressDataString(claim.claimantInformation)),
        "Uploaded"                                   ->
          (if journey.needsAddOtherDocuments then None
           else Some(expectedDocuments.mkString(" "))),
        "Bill of discharge 3"                        ->
          (if claim.reasonForSecurity == ReasonForSecurity.InwardProcessingRelief
           then Some(expectedBillOfDischarge.mkString(" "))
           else None),
        "Bill of discharge 4"                        ->
          (if claim.reasonForSecurity == ReasonForSecurity.EndUseRelief
           then Some(expectedBillOfDischarge.mkString(" "))
           else None),
        "Proof of origin"                            ->
          (if ReasonForSecurity.nidac.contains(claim.reasonForSecurity)
           then Some(expectedProofOfOrigin.mkString(" "))
           else None),
        "Other supporting documents"                 ->
          (if journey.needsAddOtherDocuments
           then Some(expectedDocuments.mkString(" "))
           else None),
        "Any information that may support the claim" -> claim.additionalDetails.map(_.value),
        "Name on the account"                        -> claim.bankAccountDetails.map(_.accountName.value),
        "Sort code"                                  -> claim.bankAccountDetails.map(_.sortCode.value),
        "Account number"                             -> claim.bankAccountDetails.map(_.accountNumber.value),
        "Reason for security deposit"                -> journey.answers.reasonForSecurity.map(rfs =>
          messages(s"choose-reason-for-security.securities.${ReasonForSecurity.keyOf(rfs)}")
        ),
        "Date security deposit made"                 -> journey.answers.displayDeclaration
          .flatMap(_.displayResponseDetail.btaDueDate)
          .flatMap(DateUtils.displayFormat),
        "Total security deposit value"               -> journey.answers.displayDeclaration
          .map(_.getTotalSecuritiesAmountFor(claim.securitiesReclaims.keySet).toPoundSterlingString),
        "Payee"                                      -> claim.displayPayeeType.map {
          case PayeeType.Consignee      => m("choose-payee-type.radio.importer")
          case PayeeType.Declarant      => m("choose-payee-type.radio.declarant")
          case PayeeType.Representative => m("choose-payee-type.radio.representative")
        },
        "Payment method"                             -> Some(
          if journey.answers.displayDeclaration
              .map(_.isAllSelectedSecuritiesEligibleForGuaranteePayment(claim.securitiesReclaims.keySet))
              .getOrElse(false)
          then "Guarantee"
          else "Bank account transfer"
        )
      ) ++ (if claim.reasonForSecurity == InwardProcessingRelief then Seq.empty else validateSecurityReclaims)
    )

    claim.payeeType shouldBe journey.answers.payeeType.map(getPayeeType)
  }

  private def getPayeeType(payeeType: PayeeType): PayeeType = payeeType match {
    case Consignee => Consignee
    case _         => Declarant
  }

  def validateConfirmationPage(doc: Document, journey: SecuritiesJourney, caseNumber: String): Assertion = {

    val mrn = journey.getLeadMovementReferenceNumber.get.value
    mrn.isEmpty shouldBe false

    val claimAmount = journey.getTotalClaimAmount.toPoundSterlingString

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

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if journey has complete answers" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
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

      "redirect to the proper page if any answer is missing" in {
        val journey =
          SecuritiesJourney
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(
              exampleSecuritiesDisplayDeclaration.getReasonForSecurity.get,
              exampleSecuritiesDisplayDeclaration
            )
            .getOrFail

        val errors: Seq[String] = journey.toOutput.left.getOrElse(Seq.empty)

        val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the submission confirmation page if journey already finalized" in {
        forAll(completeJourneyGen) { journey =>
          val updatedSession =
            SessionData.empty.copy(securitiesJourney = journey.finalizeJourneyWith("dummy case reference").toOption)
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
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(SecuritiesClaimConnector.Request(claim))(
              Future.successful(SecuritiesClaimConnector.Response("foo-123-abc"))
            )
            mockWipeOutCall()
            mockStoreSession(
              updatedSession.copy(securitiesJourney =
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
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(SecuritiesClaimConnector.Request(claim))(
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

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if journey has been finalized" in {
        forAll(completeJourneyGen, genCaseNumber) { (journey, caseNumber) =>
          val updatedSession =
            SessionData.empty.copy(securitiesJourney = journey.finalizeJourneyWith(caseNumber).toOption)
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
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
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
