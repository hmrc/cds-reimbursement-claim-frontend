/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import org.jsoup.nodes.Document
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.RejectedGoodsScheduledClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genCaseNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.collection.JavaConverters._
import scala.concurrent.Future

class CheckYourAnswersControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  val mockConnector: RejectedGoodsScheduledClaimConnector    = mock[RejectedGoodsScheduledClaimConnector]
  val mockUploadDocumentsConnector: UploadDocumentsConnector = mock[UploadDocumentsConnector]

  def mockSubmitClaim(submitClaimRequest: RejectedGoodsScheduledClaimConnector.Request)(
    response: Future[RejectedGoodsScheduledClaimConnector.Response]
  ) =
    (mockConnector
      .submitClaim(_: RejectedGoodsScheduledClaimConnector.Request)(_: HeaderCarrier))
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
      bind[RejectedGoodsScheduledClaimConnector].toInstance(mockConnector),
      bind[UploadDocumentsConnector].toInstance(mockUploadDocumentsConnector)
    )

  val controller: CheckYourAnswersController = instanceOf[CheckYourAnswersController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "check-your-answers"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  def validateCheckYourAnswersPage(doc: Document, claim: RejectedGoodsScheduledJourney.Output) = {
    val headers       = doc.select("h2.govuk-heading-m").eachText()
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summary       = summaryKeys.asScala.zip(summaryValues.asScala).toMap

    headers       should not be empty
    summaryKeys   should not be empty
    summaryValues should not be empty

    if (claim.supportingEvidences.isEmpty)
      summaryKeys.size shouldBe (summaryValues.size - 1)
    else
      summaryKeys.size shouldBe summaryValues.size

    headers should contain allOf ("Movement Reference Number (MRN)", "Declaration details", "Contact information for this claim", "Basis for claim", "Disposal method", "Details of rejected goods", "Claim total", "Details of inspection", "Supporting documents", "Now send your application")

    summaryKeys should contain allOf ("Contact details", "Contact address", (Seq(
      "MRN",
      "Scheduled document",
      "This is the basis behind the claim",
      "This is how the goods will be disposed of",
      "These are the details of the rejected goods",
      "Total",
      "Inspection date",
      "Inspection address type",
      "Inspection address"
    ) ++ (if (claim.supportingEvidences.isEmpty) Seq.empty else Seq("Uploaded"))): _*)

    summary("MRN")                                         shouldBe claim.movementReferenceNumber.value
    summary("Scheduled document")                          shouldBe claim.scheduledDocument.fileName
    summary("Contact details")                             shouldBe s"${claim.claimantInformation.summaryContact(" ")}"
    summary("Contact address")                             shouldBe s"${claim.claimantInformation.summaryAddress(" ")}"
    summary("This is the basis behind the claim")          shouldBe messages(
      s"select-basis-for-claim.rejected-goods.reason.${claim.basisOfClaim}"
    )
    summary("This is how the goods will be disposed of")   shouldBe messages(
      s"select-method-of-disposal.rejected-goods.method.${claim.methodOfDisposal}"
    )
    summary("These are the details of the rejected goods") shouldBe claim.detailsOfRejectedGoods
    summary("Inspection date")                             shouldBe claim.inspectionDate.value.toString
    summary("Inspection address type")                     shouldBe messages(
      s"inspection-address.type.${claim.inspectionAddress.addressType}"
    )
    summary("Inspection address")                          shouldBe claim.inspectionAddress.summaryAddress(" ")

    claim.reimbursementClaims.foreach { case (dutyType, claims) =>
      summary(messages(s"duty-type.${dutyType.repr}")) shouldBe claims.values
        .map(_.shouldOfPaid)
        .sum
        .toPoundSterlingString
    }

    summary("Total") shouldBe claim.reimbursementClaims.values
      .map(_.values.map(_.shouldOfPaid).sum)
      .sum
      .toPoundSterlingString

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

  def validateConfirmationPage(doc: Document, caseNumber: String) =
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
          val updatedSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = Some(journey))
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

      "redirect to the proper page if any answer is missing" in {
        val journey =
          RejectedGoodsScheduledJourney
            .empty(exampleEori)
            .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
            .getOrFail

        val errors: Seq[String] = journey.toOutput.left.getOrElse(Seq.empty)

        val updatedSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the submission confirmation page if journey already finalized" in {
        forAll(completeJourneyGen) { journey =>
          val updatedSession =
            SessionData.empty.copy(rejectedGoodsScheduledJourney =
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
          val updatedSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(RejectedGoodsScheduledClaimConnector.Request(claim))(
              Future.successful(RejectedGoodsScheduledClaimConnector.Response("foo-123-abc"))
            )
            mockWipeOutCall()
            mockStoreSession(
              updatedSession.copy(rejectedGoodsScheduledJourney =
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
          val updatedSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockSubmitClaim(RejectedGoodsScheduledClaimConnector.Request(claim))(
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
            SessionData.empty.copy(rejectedGoodsScheduledJourney = journey.finalizeJourneyWith(caseNumber).toOption)
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
          val updatedSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = Some(journey))
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