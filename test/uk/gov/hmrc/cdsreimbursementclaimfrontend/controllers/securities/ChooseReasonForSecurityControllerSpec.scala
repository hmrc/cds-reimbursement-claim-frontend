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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExistingClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.EndUseRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.InwardProcessingRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ChooseReasonForSecurityControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

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

  val initialJourney: SecuritiesJourney = SecuritiesJourney
    .empty(exampleEori)
    .submitMovementReferenceNumber(exampleMrn)

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
      ("Missing document: Community System of Duty Relief (CSDR)", "CommunitySystemsOfDutyRelief"),
      ("End Use Relief", "EndUseRelief"),
      ("Inward Processing Relief (IPR)", "InwardProcessingRelief"),
      ("Manual override of duty amount", "ManualOverrideDeposit"),
      ("Missing document: quota license", "MissingLicenseQuota"),
      ("Missing document: preference", "MissingPreferenceCertificate"),
      ("Outward Processing Relief (OPR)", "OutwardProcessingRelief"),
      ("Revenue Dispute or Inland Pre-Clearance (IPC)", "RevenueDispute"),
      ("Temporary Admissions (2 years Expiration)", "TemporaryAdmission2Y"),
      ("Temporary Admissions (6 months Expiration)", "TemporaryAdmission6M"),
      ("Temporary Admissions (3 months Expiration)", "TemporaryAdmission3M"),
      ("Temporary Admissions (2 months Expiration)", "TemporaryAdmission2M"),
      ("UKAP Entry Price", "UKAPEntryPrice"),
      ("UKAP Safeguard Duties", "UKAPSafeguardDuties")
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
          mockGetSession(SessionData(initialJourney))
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
          mockGetSession(SessionData(initialJourney))
        }

        checkPageIsDisplayed(
          performAction(Seq.empty),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messagesKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "retrieve the ACC14 declaration, make a TPI04 check and redirect to the select first security deposit page" in {
        forAll(securitiesDisplayDeclarationGen) { declaration: DisplayDeclaration =>
          val rfs: ReasonForSecurity                = declaration.getReasonForSecurity.get
          val bodRfsList: Set[ReasonForSecurity]    = Set(InwardProcessingRelief, EndUseRelief)
          val reasonForSecurityIsDischarge: Boolean = bodRfsList.contains(rfs)

          whenever(!reasonForSecurityIsDischarge) {
            val initialJourney =
              SecuritiesJourney
                .empty(declaration.getDeclarantEori)
                .submitMovementReferenceNumber(declaration.getMRN)

            val updatedJourney = SessionData(
              initialJourney
                .submitReasonForSecurityAndDeclaration(rfs, declaration)
                .flatMap(_.submitClaimDuplicateCheckStatus(false))
                .getOrFail
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(initialJourney))
              mockGetDisplayDeclaration(Right(Some(declaration)))
              mockGetIsDuplicateClaim(Right(ExistingClaim(false)))
              mockStoreSession(updatedJourney)(Right(()))
            }

            checkIsRedirect(
              performAction(
                Seq("choose-reason-for-security.securities" -> rfs.toString)
              ),
              routes.SelectSecuritiesController.showFirst()
            )
          }
        }
      }

      "redirect to the first select security page when reason for security didn't change and NOT in a change mode" in {
        forAll(securitiesDisplayDeclarationGen) { declaration: DisplayDeclaration =>
          val rfs = declaration.getReasonForSecurity

          whenever(rfs.exists(_ !== ReasonForSecurity.InwardProcessingRelief)) {
            val initialJourney =
              SecuritiesJourney
                .empty(declaration.getDeclarantEori)
                .submitMovementReferenceNumber(declaration.getMRN)
                .submitReasonForSecurityAndDeclaration(rfs.get, declaration)
                .getOrFail

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(initialJourney))
            }

            checkIsRedirect(
              performAction(
                Seq("choose-reason-for-security.securities" -> rfs.get.toString)
              ),
              routes.SelectSecuritiesController.showFirst()
            )
          }
        }
      }

      "redirect to the check declaration details page when reason for security didn't change and in a change mode" in {
        forAll(securitiesDisplayDeclarationGen) { declaration: DisplayDeclaration =>
          val rfs = declaration.getReasonForSecurity

          whenever(rfs.exists(_ !== ReasonForSecurity.InwardProcessingRelief)) {
            val initialJourney =
              SecuritiesJourney
                .empty(declaration.getDeclarantEori)
                .submitMovementReferenceNumber(declaration.getMRN)
                .submitReasonForSecurityAndDeclaration(rfs.get, declaration)
                .map(_.submitCheckDeclarationDetailsChangeMode(true))
                .getOrFail

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(initialJourney))
            }

            checkIsRedirect(
              performAction(
                Seq("choose-reason-for-security.securities" -> rfs.get.toString)
              ),
              routes.CheckDeclarationDetailsController.show()
            )
          }
        }
      }

      "redirect to the Bill of Discharge page when reason for security is InwardProcessingRelief" in {
        forAll(securitiesDisplayDeclarationGen) { declaration: DisplayDeclaration =>
          val rfs = ReasonForSecurity.InwardProcessingRelief

          val updatedDeclaration = declaration
            .copy(displayResponseDetail =
              declaration.displayResponseDetail
                .copy(securityReason = Some(rfs.acc14Code))
            )

          val initialJourney =
            SecuritiesJourney
              .empty(updatedDeclaration.getDeclarantEori)
              .submitMovementReferenceNumber(updatedDeclaration.getMRN)

          val updatedJourney = SessionData(
            initialJourney
              .submitReasonForSecurityAndDeclaration(rfs, updatedDeclaration)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .getOrFail
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockGetDisplayDeclaration(Right(Some(updatedDeclaration)))
            mockGetIsDuplicateClaim(Right(ExistingClaim(false)))
            mockStoreSession(updatedJourney)(Right(()))
          }

          checkIsRedirect(
            performAction(
              Seq("choose-reason-for-security.securities" -> rfs.toString)
            ),
            routes.BillOfDischargeController.showBOD3()
          )
        }
      }

      "redirect to the Bill of Discharge page when reason for security is EndUseRelief" in {
        forAll(securitiesDisplayDeclarationGen) { declaration: DisplayDeclaration =>
          val rfs = ReasonForSecurity.EndUseRelief

          val updatedDeclaration = declaration
            .copy(displayResponseDetail =
              declaration.displayResponseDetail
                .copy(securityReason = Some(rfs.acc14Code))
            )

          val initialJourney =
            SecuritiesJourney
              .empty(updatedDeclaration.getDeclarantEori)
              .submitMovementReferenceNumber(updatedDeclaration.getMRN)

          val updatedJourney = SessionData(
            initialJourney
              .submitReasonForSecurityAndDeclaration(rfs, updatedDeclaration)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .getOrFail
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockGetDisplayDeclaration(Right(Some(updatedDeclaration)))
            mockGetIsDuplicateClaim(Right(ExistingClaim(false)))
            mockStoreSession(updatedJourney)(Right(()))
          }

          checkIsRedirect(
            performAction(
              Seq("choose-reason-for-security.securities" -> rfs.toString)
            ),
            routes.BillOfDischargeController.showBOD4()
          )
        }
      }

      "retrieve the ACC14 declaration and redirect to the enter importer EORI page when user's EORI don't match those of ACC14" in {
        forAll(securitiesDisplayDeclarationGen) { declaration: DisplayDeclaration =>
          val initialJourney =
            SecuritiesJourney
              .empty(exampleEori)
              .submitMovementReferenceNumber(declaration.getMRN)

          val updatedJourney = SessionData(
            initialJourney
              .submitReasonForSecurityAndDeclaration(declaration.getReasonForSecurity.get, declaration)
              .getOrFail
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockGetDisplayDeclaration(Right(Some(declaration)))
            mockStoreSession(updatedJourney)(Right(()))
          }

          checkIsRedirect(
            performAction(
              Seq("choose-reason-for-security.securities" -> declaration.getReasonForSecurity.get.toString)
            ),
            routes.EnterImporterEoriNumberController.show()
          )
        }

      }

      "retrieve the ACC14 declaration and redirect to inelligible page when TPI04 says that the claim is duplicated" in {
        forAll(securitiesDisplayDeclarationGen) { declaration: DisplayDeclaration =>
          val initialJourney =
            SecuritiesJourney
              .empty(declaration.getDeclarantEori)
              .submitMovementReferenceNumber(declaration.getMRN)

          val updatedJourney = SessionData(
            initialJourney
              .submitReasonForSecurityAndDeclaration(declaration.getReasonForSecurity.get, declaration)
              .flatMap(_.submitClaimDuplicateCheckStatus(true))
              .getOrFail
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockGetDisplayDeclaration(Right(Some(declaration)))
            mockGetIsDuplicateClaim(Right(ExistingClaim(true)))
            mockStoreSession(updatedJourney)(Right(()))
          }

          checkIsRedirect(
            performAction(
              Seq("choose-reason-for-security.securities" -> declaration.getReasonForSecurity.get.toString)
            ),
            routes.ClaimInvalidTPI04Controller.show()
          )
        }
      }
    }

  }

}
