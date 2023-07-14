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
import org.scalamock.handlers.{CallHandler1, CallHandler2}
import org.scalatest.{Assertion, BeforeAndAfterEach}
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.{RejectedGoodsSingleClaimConnector, UploadDocumentsConnector}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genCaseNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ClaimantInformationSummary
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ReimbursementMethodSummary
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import scala.jdk.CollectionConverters._

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
  ): CallHandler2[RejectedGoodsSingleClaimConnector.Request, HeaderCarrier, Future[RejectedGoodsSingleClaimConnector.Response]] =
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

  @annotation.nowarn
  def validateCheckYourAnswersPage(
    doc: Document,
    claim: RejectedGoodsSingleJourney.Output,
    whetherShowRepaymentMethod: Boolean
  ): Unit = {
    val headers       = doc.select("h2.govuk-heading-m").eachText()
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summary       = summaryKeys.asScala.zip(summaryValues.asScala).toMap

    headers              should not be empty
    summaryKeys          should not be empty
    summaryValues        should not be empty
    if (claim.supportingEvidences.isEmpty)
      summaryKeys.size shouldBe (summaryValues.size - 1)
    else
      summaryKeys.size shouldBe summaryValues.size

    headers     should contain allElementsOf Seq(
      "Movement Reference Number (MRN)",
      "Declaration details",
      "Contact information for this claim",
      "Basis for claim",
      "Disposal method",
      "Details of rejected goods",
      "Claim total",
      "Details of inspection",
      "Supporting documents",
      "Now send your claim"
    ) ++ (if (whetherShowRepaymentMethod) Seq("Repayment method") else Nil)

    summaryKeys should contain allElementsOf Seq(
      "MRN",
      "Contact details",
      "Contact address",
      "This is the basis behind the claim",
      "This is how the goods will be disposed of",
      "These are the details of the rejected goods",
      "Total",
      "Inspection date",
      "Inspection address type",
      "Inspection address"
    ) ++ (
      if (whetherShowRepaymentMethod) Seq("Method") else Nil
    ) ++ (
      if (claim.supportingEvidences.nonEmpty) Seq("Uploaded") else Nil
    )

    summary("MRN")                                         shouldBe claim.movementReferenceNumber.value
    summary("Contact details")                             shouldBe ClaimantInformationSummary.getContactDataString(claim.claimantInformation)
    summary("Contact address")                             shouldBe ClaimantInformationSummary.getAddressDataString(claim.claimantInformation)
    summary("This is the basis behind the claim")          shouldBe messages(
      s"select-basis-for-claim.rejected-goods.reason.${claim.basisOfClaim}"
    )
    summary("This is how the goods will be disposed of")   shouldBe messages(
      s"select-method-of-disposal.rejected-goods.method.${claim.methodOfDisposal}"
    )
    summary("These are the details of the rejected goods") shouldBe claim.detailsOfRejectedGoods

    if (whetherShowRepaymentMethod) {
      summary("Method") shouldBe messages(
        ReimbursementMethodSummary.answerKey(messagesKey + ".repayment-method", claim.reimbursementMethod)
      )
    }

    summary("Inspection date")         shouldBe claim.inspectionDate.checkYourDetailsDisplayFormat
    summary("Inspection address type") shouldBe messages(
      s"inspection-address.type.${claim.inspectionAddress.addressType}"
    )
    summary("Inspection address")      shouldBe summaryAddress(claim.inspectionAddress, " ")

    claim.reimbursementClaims.foreachEntry { case (taxCode, amount) =>
      summary(messages(s"tax-code.$taxCode")) shouldBe amount.toPoundSterlingString
    }

    summary("Total") shouldBe claim.reimbursementClaims.values.sum.toPoundSterlingString

    claim.bankAccountDetails.foreach { value =>
      headers                          should contain("Bank details")
      summaryKeys                      should contain allOf ("Name on the account", "Sort code", "Account number")
      summary("Name on the account") shouldBe value.accountName.value
      summary("Sort code")           shouldBe value.sortCode.masked
      summary("Account number")      shouldBe value.accountNumber.masked
    }

    claim.basisOfClaimSpecialCircumstances.foreach { value =>
      headers                                                       should contain("Special circumstances")
      summaryKeys                                                   should contain("Any special circumstances relating to your claim")
      summary("Any special circumstances relating to your claim") shouldBe value
    }
  }

  def validateConfirmationPage(doc: Document, caseNumber: String): Assertion =
    doc.select(".cds-wrap-content--forced").text shouldBe caseNumber

  "Check Your Answers Controller" when {

    "Show check your answers page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if journey has complete answers" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateCheckYourAnswersPage(doc, claim, journey.isAllSelectedDutiesAreCMAEligible)
          )
        }
      }

      "display subsidy status when declaration has only subsidy payments" in {
        forAll(
          buildCompleteJourneyGen(
            acc14DeclarantMatchesUserEori = false,
            acc14ConsigneeMatchesUserEori = false,
            generateSubsidyPayments = GenerateSubsidyPayments.All
          )
        ) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => {
              validateCheckYourAnswersPage(doc, claim, journey.isSubsidyOnlyJourney)
              doc
                .select(".govuk-summary-list__row dt.govuk-summary-list__key")
                .get(2)
                .text() shouldBe "Subsidy status"
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
          mockAuthWithNoRetrievals()
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
          mockAuthWithNoRetrievals()
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
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.showConfirmation())
        }

      }
    }

    "Submitted the valid claim" must {

      def performAction(): Future[Result] = controller.submit()(FakeRequest())

      "redirect to the confirmation page if success" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
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
          checkIsRedirect(result, routes.CheckYourAnswersController.showConfirmation())
        }
      }

      "show failure page if submission fails" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
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

      def performAction(): Future[Result] = controller.showConfirmation()(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if journey has been finalized" in {
        forAll(completeJourneyGen, genCaseNumber) { (journey, caseNumber) =>
          val updatedSession =
            SessionData.empty.copy(rejectedGoodsSingleJourney = journey.finalizeJourneyWith(caseNumber).toOption)
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
          val updatedSession = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.show())
        }

      }
    }
  }

}
