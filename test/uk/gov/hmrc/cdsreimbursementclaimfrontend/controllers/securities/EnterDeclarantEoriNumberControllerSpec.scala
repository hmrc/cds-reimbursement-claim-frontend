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

import org.scalatest.BeforeAndAfterEach
import play.api.http.Status.BAD_REQUEST
import play.api.http.Status.NOT_FOUND
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration

class EnterDeclarantEoriNumberControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  val enterDeclarantEoriNumberKey: String = "enter-declarant-eori-number"

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterDeclarantEoriNumberController = instanceOf[EnterDeclarantEoriNumberController]

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  val declaration: DisplayDeclaration =
    buildSecuritiesDisplayDeclaration(
      exampleMrnAsString,
      ReasonForSecurity.AccountSales.acc14Code,
      declarantEORI = anotherExampleEori,
      consigneeEORI = Some(yetAnotherExampleEori)
    )

  lazy val initialSession: SessionData = SessionData(
    SecuritiesJourney
      .empty(exampleEori)
      .submitMovementReferenceNumber(exampleMrn)
      .submitReasonForSecurityAndDeclaration(ReasonForSecurity.AccountSales, declaration)
      .flatMap(_.submitClaimDuplicateCheckStatus(false))
      .flatMap(_.submitConsigneeEoriNumber(yetAnotherExampleEori))
      .getOrFail
  )

  "Movement Reference Number Controller" when {
    "Enter Declarant EORI number page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page on a new journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(initialSession)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-declarant-eori-number.title"),
          doc => {
            doc.select(s"#$enterDeclarantEoriNumberKey").`val`() shouldBe ""
            doc.select("form").attr("action")                    shouldBe routes.EnterDeclarantEoriNumberController.submit().url
          }
        )
      }

      "redirect to the select securities page when eori submission not required" in {
        val declaration: DisplayDeclaration =
          buildSecuritiesDisplayDeclaration(
            exampleMrnAsString,
            ReasonForSecurity.AccountSales.acc14Code
          )

        val session = SessionData(
          SecuritiesJourney
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.AccountSales, declaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail
        )

        inSequence {
          mockAuthWithNoRetrievals()
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
          SecuritiesJourney
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.InwardProcessingRelief, declaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.BillOfDischarge3Controller.show()
        )
      }

      "redirect to the bill of discharge page when eori submission not required and rfs is EndUseRelief" in {
        val declaration: DisplayDeclaration =
          buildSecuritiesDisplayDeclaration(
            exampleMrnAsString,
            ReasonForSecurity.EndUseRelief.acc14Code
          )

        val session = SessionData(
          SecuritiesJourney
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.EndUseRelief, declaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.BillOfDischarge4Controller.show()
        )
      }

      "redirect to the enter importer eori page when required but missing" in {
        val declaration: DisplayDeclaration =
          buildSecuritiesDisplayDeclaration(
            exampleMrnAsString,
            ReasonForSecurity.AccountSales.acc14Code,
            declarantEORI = anotherExampleEori,
            consigneeEORI = Some(yetAnotherExampleEori)
          )

        lazy val session = SessionData(
          SecuritiesJourney
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.AccountSales, declaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.EnterImporterEoriNumberController.show()
        )
      }
    }

    "Submit MRN page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an invalid EORI" in {
        val invalidEori = Eori("INVALID_EORI")

        inSequence {
          mockAuthWithNoRetrievals()
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
          mockAuthWithNoRetrievals()
          mockGetSession(initialSession)
        }

        checkPageIsDisplayed(
          performAction(enterDeclarantEoriNumberKey -> ""),
          messageFromMessageKey(s"$enterDeclarantEoriNumberKey.title"),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$enterDeclarantEoriNumberKey.error.required"),
          expectedStatus = BAD_REQUEST
        )
      }

      "submit a valid EORI and user is not the declarant" in forAll { (eori: Eori) =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(initialSession)
        }

        checkIsRedirect(
          performAction(enterDeclarantEoriNumberKey -> eori.value),
          baseRoutes.IneligibleController.ineligible()
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
          SecuritiesJourney
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.InwardProcessingRelief, declaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(enterDeclarantEoriNumberKey -> exampleEori.value),
          routes.BillOfDischarge3Controller.show()
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
          SecuritiesJourney
            .empty(exampleEori)
            .submitMovementReferenceNumber(exampleMrn)
            .submitReasonForSecurityAndDeclaration(ReasonForSecurity.EndUseRelief, declaration)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(enterDeclarantEoriNumberKey -> exampleEori.value),
          routes.BillOfDischarge4Controller.show()
        )
      }
    }
  }
}
