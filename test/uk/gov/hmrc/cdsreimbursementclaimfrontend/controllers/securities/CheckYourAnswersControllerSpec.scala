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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.SecuritiesClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.genCaseNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary.ClaimantInformationSummary
import uk.gov.hmrc.http.HeaderCarrier

import scala.collection.JavaConverters._
import scala.concurrent.Future

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

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.Securities)

  def validateCheckYourAnswersPage(
    doc: Document,
    journey: SecuritiesJourney,
    claim: SecuritiesJourney.Output
  ) = {
    val headers       = doc.select("h2.govuk-heading-m").eachText().asScala
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

    headers       should not be empty
    summaryKeys   should not be empty
    summaryValues should not be empty

    headers should containOnlyDefinedElementsOf(
      (Seq(
        "Declaration details".expectedAlways,
        "Export movement reference number (MRN)".expectedWhen(journey.needsExportMRNSubmission),
        "Contact information for this claim".expectedAlways,
        "Bank details".expectedWhen(claim.bankAccountDetails),
        "Documents".expectedAlways,
        "Now send your application".expectedAlways
      ) ++
        claim.securitiesReclaims.keys.map(sid => s"Claim details for: $sid".expectedAlways)): _*
    )

    summaries should containOnlyDefinedPairsOf(
      Seq(
        ("Import MRN"                   -> Some(claim.movementReferenceNumber.value)),
        ("Export MRN"                   -> journey.answers.exportMovementReferenceNumber.map(_.value)),
        ("Contact details"              -> Some(ClaimantInformationSummary.getContactDataString(claim.claimantInformation))),
        ("Contact address"              -> Some(ClaimantInformationSummary.getAddressDataString(claim.claimantInformation))),
        ("Name on the account"          -> claim.bankAccountDetails.map(_.accountName.value)),
        ("Sort code"                    -> claim.bankAccountDetails.map(_.sortCode.masked)),
        ("Account number"               -> claim.bankAccountDetails.map(_.accountNumber.masked)),
        ("Importer name"                -> journey.answers.displayDeclaration.flatMap(_.consigneeName)),
        ("Importer email"               -> journey.answers.displayDeclaration.flatMap(_.consigneeEmail)),
        ("Importer address"             -> journey.answers.displayDeclaration.flatMap(d =>
          d.displayResponseDetail.consigneeDetails.map(details =>
            d.establishmentAddress(details.establishmentAddress).mkString(" ")
          )
        )),
        ("Importer telephone"           -> journey.answers.displayDeclaration.flatMap(_.consigneeTelephone)),
        ("Declarant name"               -> journey.answers.displayDeclaration.map(_.declarantName)),
        ("Declarant address"            -> journey.answers.displayDeclaration.map(d =>
          d.establishmentAddress(d.displayResponseDetail.declarantDetails.establishmentAddress).mkString(" ")
        )),
        ("Reason for security deposit"  -> journey.answers.reasonForSecurity.map(rfs =>
          messages(s"choose-reason-for-security.securities.${ReasonForSecurity.keyOf(rfs)}")
        )),
        ("Date duty to be collected"    -> journey.answers.displayDeclaration
          .map(_.displayResponseDetail.acceptanceDate)
          .flatMap(DateUtils.displayFormat)),
        ("Date security deposit made"   -> journey.answers.displayDeclaration
          .flatMap(_.displayResponseDetail.btaDueDate)
          .flatMap(DateUtils.displayFormat)),
        ("Total security deposit value" -> journey.answers.displayDeclaration
          .map(_.getTotalSecuritiesAmountFor(claim.securitiesReclaims.keySet).toPoundSterlingString)),
        ("Payment method"               -> Some(
          if (
            journey.answers.displayDeclaration
              .map(_.isAllSelectedSecuritiesEligibleForGuaranteePayment(claim.securitiesReclaims.keySet))
              .getOrElse(false)
          )
            "Guarantee"
          else
            "Bank account transfer"
        ))
      ) ++
        journey.answers.displayDeclaration
          .flatMap(_.getSecurityDepositIds)
          .getOrElse(Seq.empty)
          .map { sid =>
            (s"Claim for $sid" -> Some(
              if (claim.securitiesReclaims.contains(sid))
                "Yes"
              else
                "No"
            ))
          } ++
        claim.securitiesReclaims.flatMap { case (sid, reclaims) =>
          Seq(
            ("Claim full amount" -> Some(
              if (
                journey.answers.displayDeclaration
                  .map(_.isFullSecurityAmount(sid, reclaims.values.sum))
                  .getOrElse(false)
              )
                "Yes"
              else
                "No"
            )),
            ("Duties selected"   -> Some(
              reclaims.keys.toList
                .sortBy(x => messages(s"select-duties.duty.${x.value}"))
                .map(taxCode => messages(s"tax-code.${taxCode.value}"))
                .mkString(" ")
            )),
            ("Total"             -> Some(
              reclaims.values.sum.toPoundSterlingString
            ))
          ) ++
            reclaims.map { case (taxCode, amount) =>
              (messages(s"tax-code.${taxCode.value}") -> Some(amount.toPoundSterlingString))
            }
        } ++
        claim.supportingEvidences
          .map(document =>
            (messages(s"choose-file-type.file-type.${document.documentType}") -> Some(
              document.fileName + " " +
                messages(s"choose-file-type.file-type.${document.documentType}")
            ))
          )
    )
  }

  def validateConfirmationPage(doc: Document, caseNumber: String) =
    doc.select(".cds-wrap-content--forced").text shouldBe caseNumber

  "Check Your Answers Controller" when {

    "Show check your answers page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if journey has complete answers" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
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
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(performAction(), controller.routeForValidationErrors(errors))
      }

      "redirect to the submission confirmation page if journey already finalized" in {
        forAll(completeJourneyGen) { journey =>
          val updatedSession =
            SessionData.empty.copy(securitiesJourney = journey.finalizeJourneyWith("dummy case reference").toOption)
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
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
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
          checkIsRedirect(result, routes.CheckYourAnswersController.showConfirmation())
        }
      }

      "show failure page if submission fails" in {
        forAll(completeJourneyGen) { journey =>
          val claim          = journey.toOutput.getOrElse(fail("cannot get output of the journey"))
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          inSequence {
            mockAuthWithNoRetrievals()
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

      def performAction(): Future[Result] = controller.showConfirmation()(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page if journey has been finalized" in {
        forAll(completeJourneyGen, genCaseNumber) { (journey, caseNumber) =>
          val updatedSession =
            SessionData.empty.copy(securitiesJourney = journey.finalizeJourneyWith(caseNumber).toOption)
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
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
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
