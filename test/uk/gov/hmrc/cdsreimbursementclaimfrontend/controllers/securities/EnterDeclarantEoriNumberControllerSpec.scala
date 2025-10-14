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

import cats.data.EitherT
import org.scalatest.BeforeAndAfterEach
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.DeclarationConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExistingClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class EnterDeclarantEoriNumberControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  val enterDeclarantEoriNumberKey: String = "enter-declarant-eori-number"

  val mockDeclarationConnector: DeclarationConnector = mock[DeclarationConnector]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[DeclarationConnector].toInstance(mockDeclarationConnector)
    )

  val controller: EnterDeclarantEoriNumberController = instanceOf[EnterDeclarantEoriNumberController]

  val declaration: DisplayDeclaration =
    buildSecuritiesDisplayDeclaration(
      exampleMrnAsString,
      ReasonForSecurity.MissingLicenseQuota.acc14Code,
      declarantEORI = anotherExampleEori,
      consigneeEORI = Some(yetAnotherExampleEori)
    )

  lazy val initialSession: SessionData = SessionData(
    SecuritiesClaim
      .empty(exampleEori)
      .submitMovementReferenceNumber(exampleMrn)
      .submitReasonForSecurityAndDeclaration(ReasonForSecurity.MissingLicenseQuota, declaration)
      .flatMap(_.submitClaimDuplicateCheckStatus(false))
      .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))
      .getOrFail
  )

  "Movement Reference Number Controller" when {
    "Enter Declarant EORI number page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "display the page on a new claim" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(initialSession)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc => {
            doc.select(s"#$enterDeclarantEoriNumberKey").`val`() shouldBe ""
            doc.select("form").attr("action")                    shouldBe routes.EnterDeclarantEoriNumberController.submit.url
          }
        )
      }

      "redirect to the select securities page when eori submission not required" in {
        val declaration: DisplayDeclaration =
          buildSecuritiesDisplayDeclaration(
            exampleMrnAsString,
            ReasonForSecurity.MissingLicenseQuota.acc14Code
          )

        val session = SessionData(
          SecuritiesClaim
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.MissingLicenseQuota, declaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.SelectSecuritiesController.showFirst()
        )
      }

      "redirect to the bill of discharge page when eori submission not required and rfs is InwardProcessingRelief" in {
        val declaration: DisplayDeclaration =
          buildSecuritiesDisplayDeclaration(
            exampleMrnAsString,
            ReasonForSecurity.InwardProcessingRelief.acc14Code
          )

        val session = SessionData(
          SecuritiesClaim
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.InwardProcessingRelief, declaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.CheckTotalImportDischargedController.show
        )
      }

      "redirect to the bill of discharge page when eori submission not required and rfs is EndUseRelief" in {
        val declaration: DisplayDeclaration =
          buildSecuritiesDisplayDeclaration(
            exampleMrnAsString,
            ReasonForSecurity.EndUseRelief.acc14Code
          )

        val session = SessionData(
          SecuritiesClaim
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.EndUseRelief, declaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.CheckTotalImportDischargedController.show
        )
      }

      "redirect to the enter importer eori page when required but missing" in {
        val declaration: DisplayDeclaration =
          buildSecuritiesDisplayDeclaration(
            exampleMrnAsString,
            ReasonForSecurity.MissingLicenseQuota.acc14Code,
            declarantEORI = anotherExampleEori,
            consigneeEORI = Some(yetAnotherExampleEori)
          )

        lazy val session = SessionData(
          SecuritiesClaim
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.MissingLicenseQuota, declaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.EnterImporterEoriNumberController.show
        )
      }
    }

    "Submit MRN page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "reject an invalid EORI" in {
        val invalidEori = Eori("INVALID_EORI")

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(initialSession)
        }

        checkPageIsDisplayed(
          performAction(enterDeclarantEoriNumberKey -> invalidEori.value),
          messageFromMessageKey(s"$enterDeclarantEoriNumberKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$enterDeclarantEoriNumberKey.invalid.number"),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject an empty EORI" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(initialSession)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$enterDeclarantEoriNumberKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$enterDeclarantEoriNumberKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "submit a valid EORI and user is not the declarant" in forAll { (eori: Eori) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(initialSession)
        }

        checkPageIsDisplayed(
          performAction(controller.formKey -> exampleEori.value),
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc => {
            getErrorSummary(doc)                               shouldBe messageFromMessageKey(
              "enter-declarant-eori-number.eori-should-match-declarant"
            )
            doc.select("#enter-declarant-eori-number").`val`() shouldBe exampleEori.value
          },
          expectedStatus = BAD_REQUEST
        )
      }

      "redirect to select securities and submit a valid EORI" in {
        val declaration: DisplayDeclaration =
          buildSecuritiesDisplayDeclaration(
            id = exampleMrnAsString,
            securityReason = ReasonForSecurity.MissingLicenseQuota.acc14Code,
            declarantEORI = anotherExampleEori,
            consigneeEORI = Some(yetAnotherExampleEori)
          )

        val claim = SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.MissingLicenseQuota, declaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(SessionData(claim.submitDeclarantEoriNumber(anotherExampleEori).getOrFail))(Right(()))
          mockGetIsDuplicateClaim(Right(ExistingClaim(claimFound = false)))
          mockStoreSession(
            SessionData(
              claim
                .submitDeclarantEoriNumber(anotherExampleEori)
                .flatMap(_.submitClaimDuplicateCheckStatus(false))
                .getOrFail
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction(controller.formKey -> anotherExampleEori.value),
          routes.SelectSecuritiesController.showFirst()
        )
      }

      "redirect to ineligible page when claim already exists" in {
        val declaration: DisplayDeclaration =
          buildSecuritiesDisplayDeclaration(
            id = exampleMrnAsString,
            securityReason = ReasonForSecurity.MissingLicenseQuota.acc14Code,
            declarantEORI = anotherExampleEori,
            consigneeEORI = Some(yetAnotherExampleEori)
          )

        val claim = SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.MissingLicenseQuota, declaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(SessionData(claim.submitDeclarantEoriNumber(anotherExampleEori).getOrFail))(Right(()))
          mockGetIsDuplicateClaim(Right(ExistingClaim(claimFound = true)))
          mockStoreSession(
            SessionData(
              claim
                .submitDeclarantEoriNumber(anotherExampleEori)
                .flatMap(_.submitClaimDuplicateCheckStatus(true))
                .getOrFail
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction(controller.formKey -> anotherExampleEori.value),
          baseRoutes.IneligibleController.ineligible
        )
      }

      "display error page when isDuplicateClaim returns left" in {
        val declaration: DisplayDeclaration =
          buildSecuritiesDisplayDeclaration(
            id = exampleMrnAsString,
            securityReason = ReasonForSecurity.MissingLicenseQuota.acc14Code,
            declarantEORI = anotherExampleEori,
            consigneeEORI = Some(yetAnotherExampleEori)
          )

        val claim = SecuritiesClaim
          .empty(exampleEori)
          .submitMovementReferenceNumber(exampleMrn)
          .submitReasonForSecurityAndDeclaration(ReasonForSecurity.MissingLicenseQuota, declaration)
          .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))
          .getOrFail

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(SessionData(claim.submitDeclarantEoriNumber(anotherExampleEori).getOrFail))(Right(()))
          mockGetIsDuplicateClaim(Left(Error("foo")))
        }

        checkIsTechnicalErrorPage(
          performAction(controller.formKey -> anotherExampleEori.value)
        )
      }

      "on submit redirect to the bill of discharge page when rfs is InwardProcessingRelief" in {
        val declaration: DisplayDeclaration =
          buildSecuritiesDisplayDeclaration(
            exampleMrnAsString,
            ReasonForSecurity.InwardProcessingRelief.acc14Code,
            exampleEori,
            Some(exampleEori)
          )

        val session = SessionData(
          SecuritiesClaim
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.InwardProcessingRelief, declaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(enterDeclarantEoriNumberKey -> exampleEori.value),
          routes.CheckTotalImportDischargedController.show
        )
      }

      "on submit redirect to the bill of discharge page when rfs is EndUseRelief" in {
        val declaration: DisplayDeclaration =
          buildSecuritiesDisplayDeclaration(
            exampleMrnAsString,
            ReasonForSecurity.EndUseRelief.acc14Code,
            exampleEori,
            Some(exampleEori)
          )

        val session = SessionData(
          SecuritiesClaim
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.EndUseRelief, declaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(enterDeclarantEoriNumberKey -> exampleEori.value),
          routes.CheckTotalImportDischargedController.show
        )
      }

      "redirect to the enter importer EORI page when required but missing" in {
        val declaration: DisplayDeclaration =
          buildSecuritiesDisplayDeclaration(
            exampleMrnAsString,
            ReasonForSecurity.MissingLicenseQuota.acc14Code,
            declarantEORI = anotherExampleEori,
            consigneeEORI = Some(yetAnotherExampleEori)
          )

        lazy val session = SessionData(
          SecuritiesClaim
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.MissingLicenseQuota, declaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(enterDeclarantEoriNumberKey -> exampleEori.value),
          routes.EnterImporterEoriNumberController.show
        )
      }
    }
  }

  private def mockGetIsDuplicateClaim(response: Either[Error, ExistingClaim]) =
    (mockDeclarationConnector
      .getIsDuplicate(_: MRN, _: ReasonForSecurity)(_: HeaderCarrier))
      .expects(*, *, *)
      .returning(EitherT.fromEither[Future](response))
}
