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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

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
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.OverpaymentsMultipleClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsMultipleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genCaseNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.CheckYourAnswersContactDetailsCardSummary
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.DateFormatter.toDisplayDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.OrdinalNumberMrnHelper
import uk.gov.hmrc.http.HeaderCarrier

import java.time.format.DateTimeFormatter
import java.time.LocalDateTime
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class CheckYourAnswersControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryInspectionAddress {

  val mockConnector: OverpaymentsMultipleClaimConnector      = mock[OverpaymentsMultipleClaimConnector]
  val mockUploadDocumentsConnector: UploadDocumentsConnector = mock[UploadDocumentsConnector]

  def mockSubmitClaim(submitClaimRequest: OverpaymentsMultipleClaimConnector.Request)(
    response: Future[OverpaymentsMultipleClaimConnector.Response]
  ) =
    (mockConnector
      .submitClaim(_: OverpaymentsMultipleClaimConnector.Request, _: Boolean)(_: HeaderCarrier))
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
      bind[OverpaymentsMultipleClaimConnector].toInstance(mockConnector),
      bind[UploadDocumentsConnector].toInstance(mockUploadDocumentsConnector)
    )

  val controller: CheckYourAnswersController = instanceOf[CheckYourAnswersController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "check-your-answers"

  def validateCheckYourAnswersPage(
    doc: Document,
    claim: OverpaymentsMultipleClaim,
    output: OverpaymentsMultipleClaim.Output,
    isPrintView: Boolean
  ) = {
    val cardTitles    = doc.select("h2.govuk-summary-card__title").eachText().asScala
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala).toSeq

    cardTitles                                                    should not be empty
    summaryKeys                                                   should not be empty
    summaryValues                                                 should not be empty
    if output.supportingEvidences.isEmpty then summaryKeys.size shouldBe (summaryValues.size - 1)
    else summaryKeys.size                                       shouldBe summaryValues.size

    cardTitles.toSeq should containOnlyDefinedElementsOf(
      "Submission details".expectedWhen(isPrintView),
      "Claim details".expectedAlways,
      "Claim amount".expectedAlways,
      "Repayment details".expectedAlways,
      "Supporting documents".expectedAlways,
      "Contact details for this claim".expectedAlways
    )

    val mrnKeys: Seq[(String, Option[String])] =
      output.movementReferenceNumbers.zipWithIndex.map { case (mrn, i) =>
        (OrdinalNumberMrnHelper(i + 1, i == 0), Some(mrn.value))
      }

    val expectedDocuments: Seq[String] =
      claim.answers.supportingEvidences
        .groupBy(_.documentType)
        .flatMap { case (docType, docs) =>
          Seq(s"${m(s"choose-file-type.file-type.${docType.get}")}:") ++ docs.map(doc => s"${doc.fileName}")
        }
        .toSeq

    val submissionDate: LocalDateTime = claim.submissionDateTime.getOrElse(LocalDateTime.now())

    summaries should containOnlyDefinedPairsOf(
      mrnKeys ++
        Seq(
          "Personal details"                 -> Some(
            CheckYourAnswersContactDetailsCardSummary.getContactDataString(output.claimantInformation)
          ),
          "Address"                          -> Some(CheckYourAnswersContactDetailsCardSummary.getAddressDataString(output.claimantInformation)),
          "Reason for claim"                 -> Some(
            m(s"select-basis-for-claim.reason.${output.basisOfClaim}")
          ),
          "Additional claim information"     -> Some(output.additionalDetails),
          "Correct EORI number"              -> output.newEoriAndDan.map(_.eori.value),
          "Correct deferment account number" -> output.newEoriAndDan.map(_.dan),
          "Total"                            -> Some(claim.getTotalReimbursementAmount.toPoundSterlingString),
          "Uploaded files"                   -> (if expectedDocuments.isEmpty then None else Some(expectedDocuments.mkString(" "))),
          "Payee"                            ->
            Some(output.displayPayeeType match {
              case PayeeType.Consignee      => m("choose-payee-type.radio.importer")
              case PayeeType.Declarant      => m("choose-payee-type.radio.declarant")
              case PayeeType.Representative => m("choose-payee-type.radio.representative")
            }),
          "Bank details"                     -> output.bankAccountDetails.map(details =>
            Seq(details.accountName.value, details.sortCode.value, details.accountNumber.value).mkString(" ")
          )
        ) ++
        output.reimbursementClaims.zipWithIndex.map { case ((mrn, claims), index) =>
          s"${OrdinalNumberMrnHelper(index + 1)} ${mrn.value}" -> Some(claims.values.sum.toPoundSterlingString)
        } ++
        Seq(
          "Claim reference number" -> claim.caseNumber.map(_.value),
          "Submitted"              -> Some(
            s"${submissionDate.format(DateTimeFormatter.ofPattern("h:mm a"))}, ${messages(s"day-of-week.${submissionDate.getDayOfWeek.getValue}")}" ++
              " " ++ toDisplayDate(submissionDate.toLocalDate)
          )
        ).filter(_ => isPrintView)
    )

  }

  def validateConfirmationPage(doc: Document, claim: OverpaymentsMultipleClaim, caseNumber: String): Assertion = {

    val mrn = claim.getLeadMovementReferenceNumber.get.value
    mrn.isEmpty shouldBe false

    val claimAmount = claim.getTotalReimbursementAmount.toPoundSterlingString

    summaryKeyValueList(doc)                                          should containOnlyPairsOf(
      Seq(
        messages(s"confirmation-of-submission.reimbursement-amount") -> claimAmount,
        messages(s"confirmation-of-submission.multiple.mrn")         -> mrn
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
          val updatedSession = SessionData.empty.copy(overpaymentsMultipleClaim = Some(claim))
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

        val updatedSession = SessionData.empty.copy(overpaymentsMultipleClaim = Some(claim))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the proper page if any answer is missing" in {
        val claim =
          OverpaymentsMultipleClaim
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
            SessionData.empty.copy(overpaymentsMultipleClaim = claim.finalizeClaimWith("dummy case reference").toOption)
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.showConfirmation)
        }

      }

      "redirect to the enter MRN page when missing MRNs answer" in {
        val claim = OverpaymentsMultipleClaim.unsafeModifyAnswers(
          completeClaimGen.sample.get,
          _.copy(movementReferenceNumbers = None)
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(performAction(), routes.EnterMovementReferenceNumberController.showFirst)
      }

      "redirect to the enter MRN page when missing declarations answer" in {
        val claim = OverpaymentsMultipleClaim.unsafeModifyAnswers(
          completeClaimGen.sample.get,
          _.copy(displayDeclarations = None)
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(performAction(), routes.EnterMovementReferenceNumberController.showFirst)
      }

      "redirect to the select basis of claim page when missing one" in {
        val claim = OverpaymentsMultipleClaim.unsafeModifyAnswers(
          completeClaimGen.sample.get,
          _.copy(basisOfClaim = None)
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(performAction(), routes.BasisForClaimController.show)
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

        val updatedSession = SessionData.empty.copy(overpaymentsMultipleClaim = Some(claim))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the proper page if any answer is missing" in {
        val claim =
          OverpaymentsMultipleClaim
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
          val updatedSession = SessionData.empty.copy(overpaymentsMultipleClaim = Some(claim))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(OverpaymentsMultipleClaimConnector.Request(output))(
              Future.successful(OverpaymentsMultipleClaimConnector.Response("foo-123-abc"))
            )
            mockWipeOutCall()
            mockStoreSession(
              updatedSession.copy(overpaymentsMultipleClaim =
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
          val updatedSession = SessionData.empty.copy(overpaymentsMultipleClaim = Some(claim))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(OverpaymentsMultipleClaimConnector.Request(output))(
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
          OverpaymentsMultipleClaim
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
            SessionData.empty.copy(overpaymentsMultipleClaim = claim.finalizeClaimWith(caseNumber).toOption)
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
          val updatedSession = SessionData.empty.copy(overpaymentsMultipleClaim = Some(claim))
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
