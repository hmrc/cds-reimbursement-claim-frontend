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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.RejectedGoodsMultipleClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genCaseNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.ClaimantInformationSummary
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import scala.jdk.CollectionConverters._

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
      .submitClaim(_: RejectedGoodsMultipleClaimConnector.Request)(_: HeaderCarrier))
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
      bind[RejectedGoodsMultipleClaimConnector].toInstance(mockConnector),
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
  def validateCheckYourAnswersPage(doc: Document, claim: RejectedGoodsMultipleJourney.Output) = {
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

    headers.asScala.toSeq should contain allOf (
      "Movement Reference Numbers (MRNs)",
      "Declaration details",
      "Contact information for this claim",
      "Basis for claim",
      "Disposal method",
      "Details of rejected goods",
      "Claim total",
      "Details of inspection",
      "Supporting documents",
      "Now send your claim"
    )

    val mrnKeys: Seq[String] =
      (1 to claim.movementReferenceNumbers.size).map(i => s"${OrdinalNumber.label(i).capitalize} MRN")

    summaryKeys should contain allOf (
      "Contact details",
      "Contact address",
      (mrnKeys ++ Seq(
        "This is the basis behind the claim",
        "This is how the goods will be disposed of",
        "These are the details of the rejected goods",
        "Total",
        "Inspection date",
        "Inspection address type",
        "Inspection address"
      ) ++ (if (claim.supportingEvidences.isEmpty) Seq.empty else Seq("Uploaded"))): _*
    )

    mrnKeys.zip(claim.movementReferenceNumbers).foreach { case (key, mrn) =>
      summary(key) shouldBe mrn.value
    }

    summary("Contact details")                             shouldBe ClaimantInformationSummary.getContactDataString(claim.claimantInformation)
    summary("Contact address")                             shouldBe ClaimantInformationSummary.getAddressDataString(claim.claimantInformation)
    summary("This is the basis behind the claim")          shouldBe messages(
      s"select-basis-for-claim.rejected-goods.reason.${claim.basisOfClaim}"
    )
    summary("This is how the goods will be disposed of")   shouldBe messages(
      s"select-method-of-disposal.rejected-goods.method.${claim.methodOfDisposal}"
    )
    summary("These are the details of the rejected goods") shouldBe claim.detailsOfRejectedGoods
    summary("Inspection date")                             shouldBe claim.inspectionDate.checkYourDetailsDisplayFormat
    summary("Inspection address type")                     shouldBe messages(
      s"inspection-address.type.${claim.inspectionAddress.addressType}"
    )
    summary("Inspection address")                          shouldBe summaryAddress(claim.inspectionAddress, " ")

    claim.reimbursementClaims.foreachEntry { case (mrn, claims) =>
      summary(mrn.value) shouldBe claims.values.sum.toPoundSterlingString
    }

    summary("Total") shouldBe claim.reimbursementClaims.values.map(_.values.sum).sum.toPoundSterlingString

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

  def validateConfirmationPage(doc: Document, journey: RejectedGoodsMultipleJourney, caseNumber: String): Assertion = {

    val mrn = journey.getLeadMovementReferenceNumber.get.value
    mrn.isEmpty shouldBe false

    val claimAmount = journey.getTotalReimbursementAmount.toPoundSterlingString

    summaryKeyValueList(doc) should containOnlyPairsOf(
      Seq(
        messages(s"confirmation-of-submission.reimbursement-amount") -> claimAmount,
        messages(s"confirmation-of-submission.multiple.mrn")         -> mrn,
        messages(s"confirmation-of-submission.claim-reference")      -> caseNumber
      )
    )
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
          val updatedSession = SessionData.empty.copy(rejectedGoodsMultipleJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateCheckYourAnswersPage(doc, claim)
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
          mockAuthWithNoRetrievals()
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
          mockAuthWithNoRetrievals()
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
            mockAuthWithNoRetrievals()
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
          val updatedSession = SessionData.empty.copy(rejectedGoodsMultipleJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
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
            mockAuthWithNoRetrievals()
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

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if journey has been finalized" in {
        forAll(completeJourneyGen, genCaseNumber) { (journey, caseNumber) =>
          val updatedSession =
            SessionData.empty.copy(rejectedGoodsMultipleJourney = journey.finalizeJourneyWith(caseNumber).toOption)
          inSequence {
            mockAuthWithNoRetrievals()
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
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkIsRedirect(performAction(), routes.CheckYourAnswersController.show)
        }

      }
    }
  }

}
