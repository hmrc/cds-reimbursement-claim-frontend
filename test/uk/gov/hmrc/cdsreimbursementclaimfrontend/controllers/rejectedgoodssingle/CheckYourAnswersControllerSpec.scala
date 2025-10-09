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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.RejectedGoodsSingleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genCaseNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.CheckYourAnswersContactDetailsCardSummary
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.DateFormatter.toDisplayDate
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
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
  ) =
    (mockConnector
      .submitClaim(_: RejectedGoodsSingleClaimConnector.Request, _: Boolean)(_: HeaderCarrier))
      .expects(submitClaimRequest, *, *)
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

  private val messagesKey: String = "check-your-answers"

  def validateCheckYourAnswersPage(
    doc: Document,
    claim: RejectedGoodsSingleClaim,
    output: RejectedGoodsSingleClaim.Output,
    isPrintView: Boolean
  ) = {
    val cardTitles    = doc.select("h2.govuk-summary-card__title").eachText().asScala
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

    cardTitles    should not be empty
    summaryKeys   should not be empty
    summaryValues should not be empty

    cardTitles.toSeq should containOnlyDefinedElementsOf(
      "Submission details".expectedWhen(isPrintView),
      "Claim details".expectedAlways,
      "Claim amount".expectedAlways,
      "Repayment details".expectedAlways,
      "Inspection details".expectedAlways,
      "Supporting documents".expectedAlways,
      "Contact details for this claim".expectedAlways
    )

    val expectedDocuments: Seq[String] =
      claim.answers.supportingEvidences
        .groupBy(_.documentType)
        .flatMap { case (docType, docs) =>
          Seq(s"${m(s"choose-file-type.file-type.${docType.get}")}:") ++ docs.map(doc => s"${doc.fileName}")
        }
        .toSeq

    val submissionDate: LocalDateTime = claim.submissionDateTime.getOrElse(LocalDateTime.now())

    summaries.toSeq should containOnlyDefinedPairsOf(
      Seq(
        "Movement Reference Number (MRN)" -> Some(output.movementReferenceNumber.value),
        "Personal details"                -> Some(
          CheckYourAnswersContactDetailsCardSummary.getContactDataString(output.claimantInformation)
        ),
        "Address"                         -> Some(CheckYourAnswersContactDetailsCardSummary.getAddressDataString(output.claimantInformation)),
        "Reason for claim"                -> Some(
          m(s"select-basis-for-claim.rejected-goods.reason.${output.basisOfClaim}")
        ),
        "Disposal method"                 -> Some(
          messages(s"select-method-of-disposal.rejected-goods.method.${output.methodOfDisposal}")
        ),
        "Additional claim information"    -> Some(output.detailsOfRejectedGoods),
        "Inspection date"                 -> Some(output.inspectionDate.checkYourDetailsDisplayFormat),
        "Total"                           -> Some(claim.getTotalReimbursementAmount.toPoundSterlingString),
        "What do you want to claim?"      -> Some(
          output.reimbursements
            .map(reimbursement => m(s"tax-code.${reimbursement.taxCode}"))
            .mkString(" ")
        ),
        "Payee"                           ->
          Some(output.displayPayeeType match {
            case PayeeType.Consignee      => m("choose-payee-type.radio.importer")
            case PayeeType.Declarant      => m("choose-payee-type.radio.declarant")
            case PayeeType.Representative => m("choose-payee-type.radio.representative")
          }),
        "Method"                          ->
          Some(output.reimbursementMethod match {
            case ReimbursementMethod.CurrentMonthAdjustment => m("check-your-answers.repayment-method.cma")
            case _                                          => m("check-your-answers.repayment-method.bt")
          }),
        "Uploaded files"                  -> (if expectedDocuments.isEmpty then None else Some(expectedDocuments.mkString(" "))),
        "Bank details"                    -> output.bankAccountDetails.map(details =>
          Seq(details.accountName.value, details.sortCode.value, details.accountNumber.value).mkString(" ")
        )
      )
        ++ output.reimbursements.map(r => messages(s"tax-code.${r.taxCode}") -> Some(r.amount.toPoundSterlingString))
        ++ Seq(
          "Details of the special circumstances" -> output.basisOfClaimSpecialCircumstances.map(_.value)
        ).filter(_ => output.basisOfClaim.equals(BasisOfRejectedGoodsClaim.SpecialCircumstances))
        ++
        Seq(
          "Claim reference number" -> claim.caseNumber.map(_.value),
          "Submitted"              -> Some(
            s"${submissionDate.format(DateTimeFormatter.ofPattern("h:mm a"))}, ${messages(s"day-of-week.${submissionDate.getDayOfWeek.getValue}")}" ++
              " " ++ toDisplayDate(submissionDate.toLocalDate)
          )
        ).filter(_ => isPrintView)
        ++ (output.inspectionAddress.addressType match {
          case InspectionAddressType.Other =>
            Seq(
              "Inspection address" -> Some(
                messages(s"inspection-address.type.${output.inspectionAddress.addressType}")
              ),
              "Address"            -> Some(summaryAddress(output.inspectionAddress, " "))
            )
          case _                           =>
            Seq(
              "Inspection address" -> Some(
                Seq(
                  messages(s"inspection-address.type.${output.inspectionAddress.addressType}"),
                  summaryAddress(output.inspectionAddress, " ")
                ).mkString(" ")
              )
            )
        })
    )

    output.payeeType shouldBe claim.answers.payeeType.get
  }

  def validateConfirmationPage(doc: Document, claim: RejectedGoodsSingleClaim, caseNumber: String): Assertion = {

    val mrn = claim.getLeadMovementReferenceNumber.get.value
    mrn.isEmpty shouldBe false

    val claimAmount = claim.getTotalReimbursementAmount.toPoundSterlingString

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
          val output         = claim.toOutput.getOrElse(fail("cannot get output of the claim"))
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleClaim = Some(claim))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateCheckYourAnswersPage(doc, claim, output, false)
          )
        }
      }

      "redirect if any subsidy payment in the declaration when subsidies are blocked" in {
        val claim =
          buildCompleteClaimGen(
            generateSubsidyPayments = GenerateSubsidyPayments.Some
          ).sample.getOrElse(fail())

        val errors: Seq[String] = claim.toOutput.left.getOrElse(Seq.empty)

        val updatedSession = SessionData.empty.copy(rejectedGoodsSingleClaim = Some(claim))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the proper page if any answer is missing" in {
        val claim = RejectedGoodsSingleClaim
          .empty(exampleDisplayDeclaration.getDeclarantEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
          .getOrFail

        val errors: Seq[String] = claim.toOutput.left.getOrElse(Seq.empty)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the submission confirmation page if claim already finalized" in {
        forAll(completeClaimGen) { claim =>
          val updatedSession =
            SessionData.empty.copy(rejectedGoodsSingleClaim = claim.finalizeClaimWith("dummy case reference").toOption)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.showConfirmation)
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
            doc => validateCheckYourAnswersPage(doc, updatedClaim, output, true)
          )
        }
      }

      "redirect if any subsidy payment in the declaration when subsidies are blocked" in {
        val claim =
          buildCompleteClaimGen(
            generateSubsidyPayments = GenerateSubsidyPayments.Some
          ).sample.getOrElse(fail())

        val errors: Seq[String] = claim.toOutput.left.getOrElse(Seq.empty)

        val updatedSession = SessionData.empty.copy(rejectedGoodsSingleClaim = Some(claim))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the proper page if any answer is missing" in {
        val claim = RejectedGoodsSingleClaim
          .empty(exampleDisplayDeclaration.getDeclarantEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
          .getOrFail

        val errors: Seq[String] = claim.toOutput.left.getOrElse(Seq.empty)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }
    }

    "Submitted the valid claim" must {

      def performAction(): Future[Result] = controller.submit(FakeRequest())

      "redirect to the confirmation page if success" in {
        forAll(completeClaimGen) { claim =>
          val output         = claim.toOutput.getOrElse(fail("cannot get output of the claim"))
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleClaim = Some(claim))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(RejectedGoodsSingleClaimConnector.Request(output))(
              Future.successful(RejectedGoodsSingleClaimConnector.Response("foo-123-abc"))
            )
            mockWipeOutCall()
            mockStoreSession(
              updatedSession.copy(rejectedGoodsSingleClaim =
                claim.finalizeClaimWith("foo-123-abc").toOption.orElse(Some(claim))
              )
            )(Right(()))
          }
          val result         = performAction()
          checkIsRedirect(result, routes.CheckYourAnswersController.showConfirmation)
        }
      }

      "show failure page if submission fails" in {
        forAll(completeClaimGen) { claim =>
          val output         = claim.toOutput.getOrElse(fail("cannot get output of the claim"))
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleClaim = Some(claim))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(RejectedGoodsSingleClaimConnector.Request(output))(
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
        val claim = RejectedGoodsSingleClaim
          .empty(exampleDisplayDeclaration.getDeclarantEori)
          .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
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
          val updatedSession =
            SessionData.empty.copy(rejectedGoodsSingleClaim = claim.finalizeClaimWith(caseNumber).toOption)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
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
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleClaim = Some(claim))
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
