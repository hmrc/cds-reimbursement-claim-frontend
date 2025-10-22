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
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesSingleClaimGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TemporaryAdmissionMethodOfDisposal
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithClaimGenerator

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class ChooseExportMethodControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryMatchers
    with TestWithClaimGenerator[SecuritiesClaim] {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: ChooseExportMethodController = instanceOf[ChooseExportMethodController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "choose-export-method"

  def validateChooseExportMethodPage(doc: Document, isError: Boolean = false) = {
    val header           = doc.select(".govuk-fieldset__legend--l").eachText().asScala
    val checkBoxesLabels = doc.select(".govuk-checkboxes__item label").eachText().asScala
    val checkBoxItems    = doc.select(".govuk-checkboxes__item input").eachAttr("value").asScala
    val continueButton   = doc.select("button.govuk-button").eachText().asScala.toList

    header                  should ===(List(messages(s"$messagesKey.title")))
    checkBoxItems.length    should ===(9)
    checkBoxesLabels.length should ===(9)
    continueButton          should contain(messages("button.continue"))

    if isError then {
      val problemHeader  = doc.select("h2.govuk-error-summary__title").eachText().asScala.toList
      val linkToError    = doc.select("a[href=#choose-export-method]").eachText().asScala.toList
      val errorParagraph = doc.select("p#choose-export-method-error").eachText().asScala.toList

      problemHeader  should ===(List(messages("error.summary.title")))
      linkToError    should ===(List(messages(s"$messagesKey.error.required")))
      errorParagraph should ===(List("Error: " + messages(s"$messagesKey.error.required")))
    }
  }

  "ChooseExportMethodController" when {

    "show page" must {
      def performAction(): Future[Result] = controller.show(FakeRequest())

      "show the page for temporary admissions RfS" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationGen(ReasonForSecurity.ntas),
          claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelected
        )
      ) { case (claim, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkPageIsDisplayed(
          performAction(),
          messages(s"$messagesKey.title"),
          doc => validateChooseExportMethodPage(doc)
        )
      }

    }

    "submit page" must {
      def performAction(
        methodOfDisposal: Option[List[TemporaryAdmissionMethodOfDisposal]],
        formKey: String = "choose-export-method[]"
      ): Future[Result] =
        controller.submit(
          FakeRequest()
            .withFormUrlEncodedBody(formKey -> methodOfDisposal.map(_.mkString(",")).getOrElse(""))
        )

      "show an error if no export method is selected" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationGen(ReasonForSecurity.ntas),
          claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelected
        )
      ) { case (claim, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkPageWithErrorIsDisplayed(
          performAction(None, messagesKey),
          messages(s"$messagesKey.title"),
          messages(s"$messagesKey.error.required")
        )
      }

      "redirect to /enter-export-movement-reference-number when single shipment is selected" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationGen(ReasonForSecurity.ntas),
          claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelected
        )
      ) { case (claim, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(Some(List(TemporaryAdmissionMethodOfDisposal.ExportedInSingleShipment))),
          routes.EnterExportMovementReferenceNumberController.showFirst
        )
      }

      "redirect to /enter-export-movement-reference-number when multiple shipment is selected" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationGen(ReasonForSecurity.ntas),
          claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelected
        )
      ) { case (claim, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(Some(List(TemporaryAdmissionMethodOfDisposal.ExportedInMultipleShipments))),
          routes.EnterExportMovementReferenceNumberController.showFirst
        )
      }

      "redirect to /confirm-full-repayment if any option other than" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsTempAdmissionWithImportDeclarationWithMfdGen,
          claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelectedGeneratedMfd
        )
      ) { case (claim, (_, _, _, methodOfDisposal)) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(Some(List(methodOfDisposal))),
          routes.ConfirmFullRepaymentController.showFirst
        )
      }

      "redirect to choose payee type if any option other than exported methods of disposal and has single security" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = SecuritiesSingleClaimGenerators.mrnWithRfsTempAdmissionWithImportDeclarationWithMfdGen,
          claimBuilder = SecuritiesSingleClaimGenerators.buildSecuritiesClaimWithSomeSecuritiesSelectedGeneratedMfd
        )
      ) { case (claim, (_, _, _, methodOfDisposal)) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(Some(List(methodOfDisposal))),
          routes.ChoosePayeeTypeController.show
        )
      }

      "redirect to choose payee type if rfs is not ntas for single security" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = SecuritiesSingleClaimGenerators.mrnWithRfsWithImportDeclarationGen(
            ReasonForSecurity.values -- ReasonForSecurity.ntas
          ),
          claimBuilder = SecuritiesSingleClaimGenerators.buildSecuritiesClaimWithSomeSecuritiesSelected
        )
      ) { case (claim, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(Some(List(TemporaryAdmissionMethodOfDisposal.DeclaredToAFreeZone))),
          routes.ChoosePayeeTypeController.show
        )
      }

      "redirect to confirm full repayment if rfs is not ntas" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithRfsWithImportDeclarationGen(ReasonForSecurity.values -- ReasonForSecurity.ntas),
          claimBuilder = buildSecuritiesClaimWithSomeSecuritiesSelected
        )
      ) { case (claim, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performAction(Some(List(TemporaryAdmissionMethodOfDisposal.DeclaredToAFreeZone))),
          routes.ConfirmFullRepaymentController.showFirst
        )
      }
    }
  }
}
