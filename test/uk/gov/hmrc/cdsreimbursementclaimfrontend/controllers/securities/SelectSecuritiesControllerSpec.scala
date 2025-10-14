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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesSingleClaimGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithClaimGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class SelectSecuritiesControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryMatchers
    with TestWithClaimGenerator[SecuritiesClaim] {

  val mockClaimsService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService)
    )

  val controller: SelectSecuritiesController = instanceOf[SelectSecuritiesController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "select-securities"

  def validateSelectSecuritiesPage(
    doc: Document,
    claim: SecuritiesClaim,
    securityDepositId: String
  ) = {
    val headers       = doc.select("h2.govuk-heading-m").eachText().asScala
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

    headers       should not be empty
    summaryKeys   should not be empty
    summaryValues should not be empty

    claim.getSecurityDetailsFor(securityDepositId).foreach { securityDetails =>
      summaries.toSeq should containOnlyDefinedPairsOf(
        Seq(
          "Movement Reference Number (MRN)" -> claim.getLeadMovementReferenceNumber
            .map(_.value),
          "Reason for security deposit"     -> claim.answers.reasonForSecurity
            .map(rfs => messages(s"choose-reason-for-security.securities.${ReasonForSecurity.keyOf(rfs)}")),
          "Total security deposit value"    -> Some(securityDetails.getTotalAmount.toPoundSterlingString),
          "Security deposit paid"           -> Some(securityDetails.getPaidAmount.toPoundSterlingString),
          "Security deposit payment date"   -> claim.getLeadImportDeclaration
            .flatMap(d => DateUtils.displayFormat(d.displayResponseDetail.acceptanceDate)),
          "Security deposit expiry date"    -> claim.getLeadImportDeclaration
            .flatMap(d => DateUtils.displayFormat(d.displayResponseDetail.btaDueDate))
        )
      )
    }
  }

  "SelectSecuritiesController" when {

    "show page" must {

      def performAction(id: String): Future[Result] = controller.show(id)(FakeRequest())

      def performActionShowFirst(): Future[Result] = controller.showFirst()(FakeRequest())

      "redirect to the ineligible page if an invalid security deposit ID" in forSomeWith(
        ClaimGenerator(
          testParamsGenerator = SecuritiesClaimGenerators.mrnWithtRfsWithImportDeclarationWithoutIPROrENUGen,
          claimBuilder = SecuritiesClaimGenerators.buildSecuritiesClaimReadyForSelectingSecurities
        )
      ) { case (initialClaim, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialClaim))
        }

        checkIsRedirect(
          performAction("foo-123-xyz"),
          baseRoutes.IneligibleController.ineligible
        )
      }

      "display the page if a valid security deposit ID" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = SecuritiesClaimGenerators.mrnWithtRfsWithImportDeclarationWithoutIPROrENUGen,
          claimBuilder = SecuritiesClaimGenerators.buildSecuritiesClaimReadyForSelectingSecurities
        )
      ) { case (initialClaim, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        for depositId <- depositIds do {
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkPageIsDisplayed(
            performAction(depositId),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateSelectSecuritiesPage(doc, initialClaim, depositId)
          )
        }
      }

      "redirect to show by deposit ID if a valid first security deposit ID" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = SecuritiesClaimGenerators.mrnWithtRfsWithImportDeclarationWithoutIPROrENUGen,
          claimBuilder = SecuritiesClaimGenerators.buildSecuritiesClaimReadyForSelectingSecurities
        )
      ) { case (initialClaim, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        for depositId <- depositIds do {
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkIsRedirect(
            performActionShowFirst(),
            routes.SelectSecuritiesController.show(depositIds.head)
          )
        }
      }

      "select security deposit and redirect to check declaration details if a single security deposit ID" in {
        forAllWith(
          ClaimGenerator(
            testParamsGenerator = SecuritiesSingleClaimGenerators.mrnWithtRfsWithImportDeclarationWithoutIPROrENUGen,
            claimBuilder = SecuritiesSingleClaimGenerators.buildSecuritiesClaimReadyForSelectingSecurities
          )
        ) { case (initialClaim, (_, _, decl)) =>
          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
            mockStoreSession(SessionData(initialClaim.selectSecurityDepositId(depositIds.head).getOrFail))(Right(()))
          }
          checkIsRedirect(
            performActionShowFirst(),
            routes.CheckDeclarationDetailsSingleSecurityController.show
          )
        }
      }

      "redirect to choose reason for security when there are no security deposits" in {
        val gen = mrnWithRfsWithImportDeclarationGen.sample
          .map { case (mrn, reasonForSecurity, importDeclaration) =>
            val updatedDisplayResponseDetail = importDeclaration.displayResponseDetail.copy(securityDetails = None)
            val updatedImportDeclaration     =
              importDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetail)
            (mrn, reasonForSecurity, updatedImportDeclaration)
          }
          .getOrElse(fail("Failed to generate claim data"))

        val claim = SecuritiesClaimGenerators.buildSecuritiesClaimReadyForSelectingSecurities(gen)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performActionShowFirst(),
          routes.ChooseReasonForSecurityController.show
        )
      }

      "display error page when there is a problem selecting a security deposit for single security" in {
        val gen = SecuritiesSingleClaimGenerators.mrnWithRfsWithImportDeclarationGen.sample
          .map { case (mrn, reasonForSecurity, importDeclaration) =>
            val securityDetails              =
              importDeclaration.displayResponseDetail.securityDetails.get.map(_.copy(securityDepositId = ""))
            val updatedDisplayResponseDetail =
              importDeclaration.displayResponseDetail.copy(securityDetails = Some(securityDetails))
            val updatedImportDeclaration     =
              importDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetail)
            (mrn, reasonForSecurity, updatedImportDeclaration)
          }
          .getOrElse(fail("Failed to generate claim data"))

        val claim = SecuritiesSingleClaimGenerators.buildSecuritiesClaimReadyForSelectingSecurities(gen)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsTechnicalErrorPage(
          performActionShowFirst()
        )
      }
    }

    "submitting securities selection" must {

      def performAction(id: String, data: (String, String)*): Future[Result] =
        controller.submit(id)(FakeRequest().withFormUrlEncodedBody(data*))

      "select the first security deposit and move to the next security" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = SecuritiesClaimGenerators.mrnWithtRfsWithImportDeclarationWithoutIPROrENUGen,
          claimBuilder = SecuritiesClaimGenerators.buildSecuritiesClaimReadyForSelectingSecurities
        )
      ) { case (initialClaim, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        whenever(depositIds.size >= 2) {
          val firstDepositId  = depositIds(0)
          val secondDepositId = depositIds(1)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
            mockStoreSession(SessionData(initialClaim.selectSecurityDepositId(firstDepositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(firstDepositId, "select-securities" -> "true"),
            routes.SelectSecuritiesController.show(secondDepositId)
          )
        }

      }

      "skip the first security deposit and move to the next security" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = SecuritiesClaimGenerators.mrnWithtRfsWithImportDeclarationWithoutIPROrENUGen,
          claimBuilder = SecuritiesClaimGenerators.buildSecuritiesClaimReadyForSelectingSecurities
        )
      ) { case (initialClaim, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        whenever(depositIds.size >= 1) {
          val firstDepositId  = depositIds(0)
          val secondDepositId = depositIds(1)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkIsRedirect(
            performAction(firstDepositId, "select-securities" -> "false"),
            routes.SelectSecuritiesController.show(secondDepositId)
          )
        }

      }

      "select the last security deposit and move to the check declaration details page" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = SecuritiesClaimGenerators.mrnWithtRfsWithImportDeclarationWithoutIPROrENUGen,
          claimBuilder = SecuritiesClaimGenerators.buildSecuritiesClaimReadyForSelectingSecurities
        )
      ) { case (initialClaim, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        whenever(depositIds.size >= 2) {
          val lastDepositId = depositIds.last

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
            mockStoreSession(SessionData(initialClaim.selectSecurityDepositId(lastDepositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(lastDepositId, "select-securities" -> "true"),
            routes.CheckDeclarationDetailsController.show
          )
        }
      }

      "skip the last security deposit and move to the check declaration details page" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = SecuritiesClaimGenerators.mrnWithtRfsWithImportDeclarationWithoutIPROrENUGen,
          claimBuilder = SecuritiesClaimGenerators.buildSecuritiesClaimWithSomeSecuritiesSelected
        )
      ) { case (initialClaim, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        whenever(depositIds.size >= 1) {
          val lastDepositId = depositIds.last

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
            mockStoreSession(SessionData(initialClaim.removeSecurityDepositId(lastDepositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(lastDepositId, "select-securities" -> "false"),
            routes.CheckDeclarationDetailsController.show
          )
        }
      }

      "select the first security deposit and return to the check details page when in change mode" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = SecuritiesClaimGenerators.mrnWithtRfsWithImportDeclarationWithoutIPROrENUGen,
          claimBuilder = SecuritiesClaimGenerators.buildSecuritiesClaimInChangeDeclarationDetailsMode
        )
      ) { case (initialClaim, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        whenever(depositIds.size >= 2) {
          val firstDepositId = depositIds(0)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
            mockStoreSession(SessionData(initialClaim.selectSecurityDepositId(firstDepositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(firstDepositId, "select-securities" -> "true"),
            routes.CheckDeclarationDetailsController.show
          )
        }

      }

      "de-select the last security deposit and return to the check details page when in change mode" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = SecuritiesClaimGenerators.mrnWithtRfsWithImportDeclarationWithoutIPROrENUGen,
          claimBuilder = SecuritiesClaimGenerators.buildSecuritiesClaimInChangeDeclarationDetailsMode
        )
      ) { case (initialClaim, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        whenever(depositIds.nonEmpty) {
          val depositId = depositIds.last

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
            mockStoreSession(SessionData(initialClaim.removeSecurityDepositId(depositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(depositId, "select-securities" -> "false"),
            routes.CheckDeclarationDetailsController.show
          )
        }

      }

      "after de-selecting any security redirect back to the CYA page when in change your answers mode" in forAll(
        SecuritiesClaimGenerators.completeClaimGen
      ) { initialClaim =>
        val selected = initialClaim.getSelectedDepositIds
        for depositId <- selected do {

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
            mockStoreSession(SessionData(initialClaim.removeSecurityDepositId(depositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(depositId, "select-securities" -> "false"),
            routes.CheckYourAnswersController.show
          )
        }
      }

      "after selecting any missing security redirect back to the CYA page when in change your answers mode" in forAll(
        SecuritiesClaimGenerators.completeClaimGen
      ) { initialClaim =>
        val unselected = initialClaim.getSecurityDepositIds.toSet -- initialClaim.getSelectedDepositIds.toSet
        for depositId <- unselected do {

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
            mockStoreSession(SessionData(initialClaim.selectSecurityDepositId(depositId).getOrFail))(Right(()))
          }

          checkIsRedirect(
            performAction(depositId, "select-securities" -> "true"),
            routes.CheckYourAnswersController.show
          )
        }
      }

      "after selecting any missing security ignore change declaration details mode and redirect back to the CYA page when in change your answers mode" in forAll(
        SecuritiesClaimGenerators.completeClaimGen
      ) { initialClaim =>
        val unselected = initialClaim.getSecurityDepositIds.toSet -- initialClaim.getSelectedDepositIds.toSet
        for depositId <- unselected do {

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim.submitCheckDeclarationDetailsChangeMode(true)))
            mockStoreSession(
              SessionData(
                initialClaim
                  .submitCheckDeclarationDetailsChangeMode(true)
                  .selectSecurityDepositId(depositId)
                  .getOrFail
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(depositId, "select-securities" -> "true"),
            routes.CheckYourAnswersController.show
          )
        }
      }

      "reject empty selection and display error message" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = SecuritiesClaimGenerators.mrnWithtRfsWithImportDeclarationWithoutIPROrENUGen,
          claimBuilder = SecuritiesClaimGenerators.buildSecuritiesClaimReadyForSelectingSecurities
        )
      ) { case (initialClaim, (_, _, decl)) =>
        val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)

        whenever(depositIds.size >= 2) {
          val firstDepositId = depositIds.head

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
          }

          checkPageWithErrorIsDisplayed(
            performAction(firstDepositId, "select-securities" -> ""),
            messageFromMessageKey("select-securities.title"),
            messageFromMessageKey("select-securities.error.required")
          )
        }
      }
    }
  }
}
