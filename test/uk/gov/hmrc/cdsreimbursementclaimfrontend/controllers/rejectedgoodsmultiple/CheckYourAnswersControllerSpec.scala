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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.RejectedGoodsMultipleClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genCaseNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.DateFormatter.toDisplayDate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.CheckYourAnswersContactDetailsCardSummary
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.OrdinalNumberMrnHelper
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

  val mockConnector: RejectedGoodsMultipleClaimConnector     = mock[RejectedGoodsMultipleClaimConnector]
  val mockUploadDocumentsConnector: UploadDocumentsConnector = mock[UploadDocumentsConnector]

  def mockSubmitClaim(submitClaimRequest: RejectedGoodsMultipleClaimConnector.Request)(
    response: Future[RejectedGoodsMultipleClaimConnector.Response]
  ) =
    (mockConnector
      .submitClaim(_: RejectedGoodsMultipleClaimConnector.Request, _: Boolean)(_: HeaderCarrier))
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
      bind[RejectedGoodsMultipleClaimConnector].toInstance(mockConnector),
      bind[UploadDocumentsConnector].toInstance(mockUploadDocumentsConnector)
    )

  val controller: CheckYourAnswersController = instanceOf[CheckYourAnswersController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "check-your-answers"

  def validateCheckYourAnswersPage(
    doc: Document,
    claim: RejectedGoodsMultipleJourney.Output,
    journey: RejectedGoodsMultipleJourney,
    isPrintView: Boolean
  ) = {
    val cardTitles    = doc.select("h2.govuk-summary-card__title").eachText().asScala
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

    cardTitles                                                   should not be empty
    summaryKeys                                                  should not be empty
    summaryValues                                                should not be empty
    if claim.supportingEvidences.isEmpty then summaryKeys.size shouldBe (summaryValues.size - 1)
    else summaryKeys.size                                      shouldBe summaryValues.size

    cardTitles.toSeq should containOnlyDefinedElementsOf(
      "Submission details".expectedWhen(isPrintView),
      "Claim details".expectedAlways,
      "Claim amount".expectedAlways,
      "Inspection details".expectedAlways,
      "Repayment details".expectedAlways,
      "Supporting documents".expectedAlways,
      "Contact details for this claim".expectedAlways
    )

    val mrnKeys: Seq[(String, Option[String])] =
      claim.movementReferenceNumbers.zipWithIndex.map { case (mrn, i) =>
        (OrdinalNumberMrnHelper(i + 1, i == 0), Some(mrn.value))
      }

    val expectedDocuments: Seq[String] =
      journey.answers.supportingEvidences
        .groupBy(_.documentType)
        .flatMap { case (docType, docs) =>
          Seq(s"${m(s"choose-file-type.file-type.${docType.get}")}:") ++ docs.map(doc => s"${doc.fileName}")
        }
        .toSeq

    val submissionDate: LocalDateTime = journey.submissionDateTime.getOrElse(LocalDateTime.now())

    summaries.toSeq should containOnlyDefinedPairsOf(
      mrnKeys ++
        Seq(
          "Personal details"             -> Some(
            CheckYourAnswersContactDetailsCardSummary.getContactDataString(claim.claimantInformation)
          ),
          "Address"                      -> Some(CheckYourAnswersContactDetailsCardSummary.getAddressDataString(claim.claimantInformation)),
          "Reason for claim"             -> Some(
            m(s"select-basis-for-claim.rejected-goods.reason.${claim.basisOfClaim}")
          ),
          "Disposal method"              -> Some(
            messages(s"select-method-of-disposal.rejected-goods.method.${claim.methodOfDisposal}")
          ),
          "Additional claim information" -> Some(claim.detailsOfRejectedGoods),
          "Inspection date"              -> Some(claim.inspectionDate.checkYourDetailsDisplayFormat),
          "Total"                        -> Some(journey.getTotalReimbursementAmount.toPoundSterlingString),
          "Uploaded files"               -> (if expectedDocuments.isEmpty then None else Some(expectedDocuments.mkString(" "))),
          "Payee"                        ->
            Some(claim.displayPayeeType match {
              case PayeeType.Consignee      => m("choose-payee-type.radio.importer")
              case PayeeType.Declarant      => m("choose-payee-type.radio.declarant")
              case PayeeType.Representative => m("choose-payee-type.radio.representative")
            }),
          "Bank details"                 -> claim.bankAccountDetails.map(details =>
            Seq(details.accountName.value, details.sortCode.value, details.accountNumber.value).mkString(" ")
          )
        ) ++
        claim.reimbursementClaims.zipWithIndex.map { case ((mrn, claims), index) =>
          s"${OrdinalNumberMrnHelper(index + 1)} ${mrn.value}" -> Some(claims.values.sum.toPoundSterlingString)
        } ++ Seq(
          "Details of the special circumstances" -> claim.basisOfClaimSpecialCircumstances.map(_.value)
        ).filter(_ => claim.basisOfClaim.equals(BasisOfRejectedGoodsClaim.SpecialCircumstances))
        ++
        Seq(
          "Claim reference number" -> journey.caseNumber.map(_.value),
          "Submitted"              -> Some(
            s"${submissionDate.format(DateTimeFormatter.ofPattern("h:mm a"))}, ${messages(s"day-of-week.${submissionDate.getDayOfWeek.getValue}")}" ++
              " " ++ toDisplayDate(submissionDate.toLocalDate)
          )
        ).filter(_ => isPrintView)
        ++ (claim.inspectionAddress.addressType match {
          case InspectionAddressType.Other =>
            Seq(
              "Inspection address" -> Some(
                messages(s"inspection-address.type.${claim.inspectionAddress.addressType}")
              ),
              "Address"            -> Some(summaryAddress(claim.inspectionAddress, " "))
            )
          case _                           =>
            Seq(
              "Inspection address" -> Some(
                Seq(
                  messages(s"inspection-address.type.${claim.inspectionAddress.addressType}"),
                  summaryAddress(claim.inspectionAddress, " ")
                ).mkString(" ")
              )
            )
        })
    )
  }

  def validateConfirmationPage(doc: Document, journey: RejectedGoodsMultipleJourney, caseNumber: String): Assertion = {

    val mrn = journey.getLeadMovementReferenceNumber.get.value
    mrn.isEmpty shouldBe false

    val claimAmount = journey.getTotalReimbursementAmount.toPoundSterlingString

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

      "display the page if journey has complete answers" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(rejectedGoodsMultipleJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateCheckYourAnswersPage(doc, claim, journey, false)
          )
        }
      }

      "redirect if any subsidy payment in the declaration when subsidies are blocked" in {
        val journey =
          buildCompleteJourneyGen(
            generateSubsidyPayments = GenerateSubsidyPayments.Some,
            features = Some(
              RejectedGoodsMultipleJourney.Features(shouldBlockSubsidies = true, shouldAllowSubsidyOnlyPayments = false)
            )
          ).sample.getOrElse(fail())

        val errors: Seq[String] = journey.toOutput.left.getOrElse(Seq.empty)

        val updatedSession = SessionData.empty.copy(rejectedGoodsMultipleJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the proper page if any answer is missing" in {
        val journey =
          RejectedGoodsMultipleJourney
            .empty(exampleDisplayDeclaration.getDeclarantEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
            .getOrFail

        val errors: Seq[String] = journey.toOutput.left.getOrElse(Seq.empty)

        val updatedSession = SessionData.empty.copy(rejectedGoodsMultipleJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the submission confirmation page if journey already finalized" in {
        forAll(completeJourneyGen) { journey =>
          val updatedSession =
            SessionData.empty.copy(rejectedGoodsMultipleJourney =
              journey.finalizeJourneyWith("dummy case reference").toOption
            )
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.showConfirmation)
        }

      }

      "redirect to the enter MRN page when missing MRNs answer" in {
        val journey = RejectedGoodsMultipleJourney.unsafeModifyAnswers(
          completeJourneyGen.sample.get,
          _.copy(movementReferenceNumbers = None)
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(performAction(), routes.EnterMovementReferenceNumberController.showFirst())
      }

      "redirect to the enter MRN page when missing declarations answer" in {
        val journey = RejectedGoodsMultipleJourney.unsafeModifyAnswers(
          completeJourneyGen.sample.get,
          _.copy(displayDeclarations = None)
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(performAction(), routes.EnterMovementReferenceNumberController.showFirst())
      }

      "redirect to the select basis of claim page when missing one" in {
        val journey = RejectedGoodsMultipleJourney.unsafeModifyAnswers(
          completeJourneyGen.sample.get,
          _.copy(basisOfClaim = None)
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkIsRedirect(performAction(), routes.BasisForClaimController.show)
      }
    }

    "Show print page" must {

      def performAction(): Future[Result] = controller.showPrintView(FakeRequest())

      "display the page if journey has complete answers" in {
        forAll(completeJourneyGen, genCaseNumber) { (journey, caseNumber) =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedJourney = journey.finalizeJourneyWith(caseNumber).getOrElse(fail("cannot submit case number"))

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(updatedJourney))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.print-view.title"),
            doc => validateCheckYourAnswersPage(doc, claim, updatedJourney, true)
          )
        }
      }

      "redirect if any subsidy payment in the declaration when subsidies are blocked" in {
        val journey =
          buildCompleteJourneyGen(
            generateSubsidyPayments = GenerateSubsidyPayments.Some,
            features = Some(
              RejectedGoodsMultipleJourney.Features(shouldBlockSubsidies = true, shouldAllowSubsidyOnlyPayments = false)
            )
          ).sample.getOrElse(fail())

        val errors: Seq[String] = journey.toOutput.left.getOrElse(Seq.empty)

        val updatedSession = SessionData.empty.copy(rejectedGoodsMultipleJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the proper page if any answer is missing" in {
        val journey =
          RejectedGoodsMultipleJourney
            .empty(exampleDisplayDeclaration.getDeclarantEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
            .getOrFail

        val errors: Seq[String] = journey.toOutput.left.getOrElse(Seq.empty)

        val updatedSession = SessionData.empty.copy(rejectedGoodsMultipleJourney = Some(journey))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }
    }

    "Submitted the valid claim" must {

      def performAction(): Future[Result] = controller.submit(FakeRequest())

      "redirect to the confirmation page if success" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(rejectedGoodsMultipleJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(RejectedGoodsMultipleClaimConnector.Request(claim))(
              Future.successful(RejectedGoodsMultipleClaimConnector.Response("foo-123-abc"))
            )
            mockWipeOutCall()
            mockStoreSession(
              updatedSession.copy(rejectedGoodsMultipleJourney =
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
          val updatedSession = SessionData.empty.copy(rejectedGoodsMultipleJourney = Some(journey))
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(RejectedGoodsMultipleClaimConnector.Request(claim))(
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

      "display the page if journey has been finalized" in {
        forAll(completeJourneyGen, genCaseNumber) { (journey, caseNumber) =>
          val updatedSession =
            SessionData.empty.copy(rejectedGoodsMultipleJourney = journey.finalizeJourneyWith(caseNumber).toOption)
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
          val updatedSession = SessionData.empty.copy(rejectedGoodsMultipleJourney = Some(journey))
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
