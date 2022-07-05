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

import cats.data.EitherT
import org.jsoup.nodes.Document
import org.scalatest.Assertion
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.http.Status.NOT_FOUND
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.CDSReimbursementClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExistingClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ChooseReasonForSecurityControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks {

  val mockClaimsService: ClaimService                                    = mock[ClaimService]
  val mockCDSReimbursementClaimConnector: CDSReimbursementClaimConnector = mock[CDSReimbursementClaimConnector]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService),
      bind[CDSReimbursementClaimConnector].toInstance(mockCDSReimbursementClaimConnector)
    )

  val controller: ChooseReasonForSecurityController = instanceOf[ChooseReasonForSecurityController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "choose-reason-for-security.securities"
  val journey: SecuritiesJourney  = SecuritiesJourney.empty(exampleEori).submitMovementReferenceNumber(exampleMrn)

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  private def mockGetDisplayDeclaration(response: Either[Error, Option[DisplayDeclaration]]) =
    (mockClaimsService
      .getDisplayDeclaration(_: MRN, _: ReasonForSecurity)(_: HeaderCarrier))
      .expects(*, *, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockGetIsDuplicateClaim(response: Either[Error, ExistingClaim]) =
    (mockCDSReimbursementClaimConnector
      .getIsDuplicate(_: MRN, _: ReasonForSecurity)(_: HeaderCarrier))
      .expects(*, *, *)
      .returning(EitherT.fromEither[Future](response))

  def validateChooseReasonForSecurityPage(doc: Document): Assertion = {
    radioItems(doc) should contain theSameElementsAs Seq(
      ("Account Sales", "AccountSales"),
      ("Community Systems of Duty Relief (CSDR)", "CommunitySystemsOfDutyRelief"),
      ("End-use Relief", "EndUseRelief"),
      ("Inward Processing Relief (IPR)", "InwardProcessingRelief"),
      ("Manual Override Deposit*", "ManualOverrideDeposit"),
      ("Missing License Quota*", "MissingLicenseQuota"),
      ("Missing Preference Certificate", "MissingPreferenceCertificate"),
      ("Outward Processing Relief (OPR)", "OutwardProcessingRelief"),
      ("Revenue Dispute/Inward Pre-Clearance (IPC)", "RevenueDispute"),
      ("Temporary Admission (2 years Expiration)", "TemporaryAdmission2Y"),
      ("Temporary Admission (6 months Expiration)", "TemporaryAdmission6M"),
      ("Temporary Admission (3 months Expiration)", "TemporaryAdmission3M"),
      ("Temporary Admission (2 months Expiration)", "TemporaryAdmission2M"),
      ("UKAP Entry Price*", "UKAPEntryPrice"),
      ("UKAP Safeguard Duties*", "UKAPSafeguardDuties")
    )
    hasContinueButton(doc)
  }

  "ChooseReasonForSecuritiesController" when {

    "show page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page for the first time" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateChooseReasonForSecurityPage(doc)
        )
      }

    }

    "submit page" must {
      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not succeed if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction(Seq.empty)) shouldBe NOT_FOUND
      }

      "reject an empty reason selection" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
        }

        checkPageIsDisplayed(
          performAction(Seq.empty),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messagesKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      val displayDeclaration        = sample[DisplayDeclaration]
      val updatedDisplayDeclaration = displayDeclaration
        .copy(displayResponseDetail =
          displayDeclaration.displayResponseDetail
            .copy(
              consigneeDetails = displayDeclaration.displayResponseDetail.consigneeDetails
                .map((cd: ConsigneeDetails) => cd.copy(consigneeEORI = journey.getClaimantEori.value)),
              declarantDetails = displayDeclaration.displayResponseDetail.declarantDetails
                .copy(declarantEORI = journey.getClaimantEori.value),
              declarationId = journey.getLeadMovementReferenceNumber.get.value,
              securityReason = Some(ReasonForSecurity.AccountSales.acc14Code)
            )
        )

      "Retrieve declaration details from Acc14 and EORIs don't match" in {
        val updatedDisplayDeclarationEorisUnmatched = displayDeclaration
          .copy(displayResponseDetail =
            displayDeclaration.displayResponseDetail
              .copy(
                declarationId = journey.getLeadMovementReferenceNumber.get.value,
                securityReason = Some(ReasonForSecurity.AccountSales.acc14Code)
              )
          )

        val updatedJourney = SessionData(
          journey
            .submitReasonForSecurityAndDeclaration(
              ReasonForSecurity.AccountSales,
              updatedDisplayDeclarationEorisUnmatched
            )
            .right
            .toOption
            .get
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
          mockGetDisplayDeclaration(Right(Some(updatedDisplayDeclarationEorisUnmatched)))
          mockGetIsDuplicateClaim(Right(ExistingClaim(false)))
          mockStoreSession(updatedJourney)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq("choose-reason-for-security.securities" -> "AccountSales")),
          routes.EnterImporterEoriNumberController.show()
        )
      }

      "HAPPY PATH - Retrieve declaration details from Acc14 and claim is not duplicate" in {
        val updatedJourney = SessionData(
          journey
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.AccountSales, updatedDisplayDeclaration)
            .right
            .toOption
            .get
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
          mockGetDisplayDeclaration(Right(Some(updatedDisplayDeclaration)))
          mockGetIsDuplicateClaim(Right(ExistingClaim(false)))
          mockStoreSession(updatedJourney)(Right(()))
        }

        checkIsRedirect(
          performAction(Seq("choose-reason-for-security.securities" -> "AccountSales")),
          routes.SelectDutiesController.show("")
        )
      }

      "UNHAPPY PATH - Retrieve declaration details from Acc14 and claim is duplicate" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(journey))
          mockGetDisplayDeclaration(Right(Some(updatedDisplayDeclaration)))
          mockGetIsDuplicateClaim(Right(ExistingClaim(true)))
        }

        checkIsRedirect(
          performAction(Seq("choose-reason-for-security.securities" -> "AccountSales")),
          controllers.routes.IneligibleController.ineligible()
        )
      }
    }

  }

}