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
import org.jsoup.nodes.Element
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
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.MissingPreferenceCertificate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.TestWithClaimGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class CheckDeclarationDetailsSingleSecurityControllerSpec
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

  val controller: CheckDeclarationDetailsSingleSecurityController =
    instanceOf[CheckDeclarationDetailsSingleSecurityController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val messagesKey: String = "check-import-declaration-details"

  def getSummaryCardByTitle(doc: Document, title: String): Option[Element] =
    doc.select(".govuk-summary-card").asScala.find { card =>
      card.select(".govuk-summary-card__title").text() == title
    }

  def getSummaryList(card: Element): Seq[(String, String)] = {
    val rows = card.select(".govuk-summary-list__row").asScala
    rows.map { row =>
      val key   = row.select(".govuk-summary-list__key").text
      val value = row.select(".govuk-summary-list__value").text
      key -> value
    }.toSeq
  }

  def validateCheckDeclarationDetailsPage(
    doc: Document,
    claim: SecuritiesClaim
  ): Assertion = {

    val claimDetailsCard               = getSummaryCardByTitle(doc, "Claim details")
    val importDetailsCard              = getSummaryCardByTitle(doc, "Import details")
    val securityDepositOrGuaranteeCard = getSummaryCardByTitle(doc, "Security deposit or guarantee")
    val importerDetailsCard            = getSummaryCardByTitle(doc, "Importer details")
    val declarantDetailsCard           = getSummaryCardByTitle(doc, "Declarant details")

    claimDetailsCard.isDefined               should not be false
    importDetailsCard.isDefined              should not be false
    securityDepositOrGuaranteeCard.isDefined should not be false
    importerDetailsCard.isDefined            should not be false
    declarantDetailsCard.isDefined           should not be false

    getSummaryList(claimDetailsCard.get)               should containOnlyDefinedPairsOf(
      Seq(
        "Movement Reference Number (MRN)"    -> claim.getLeadMovementReferenceNumber.map(_.value),
        "Why was a security deposit needed?" -> claim.answers.reasonForSecurity
          .map(rfs => messages(s"choose-reason-for-security.securities.${ReasonForSecurity.keyOf(rfs)}"))
      )
    )
    getSummaryList(importDetailsCard.get)              should containOnlyDefinedPairsOf(
      Seq(
        claim.getLeadDisplayDeclaration.get.getMaybeLRN match {
          case Some(lrn) => "Local Reference Number (LRN)" -> Some(lrn)
          case _         => ""                             -> None
        },
        "Date of import" -> DateUtils.displayFormat(
          claim.getLeadDisplayDeclaration.map(_.displayResponseDetail.acceptanceDate)
        )
      )
    )
    getSummaryList(securityDepositOrGuaranteeCard.get) should containOnlyDefinedPairsOf(
      Seq(
        "Method of payment" -> claim.getLeadDisplayDeclaration.get.getSingleSecurityDetails.map { securityDetails =>
          messages(s"method-of-payment.${securityDetails.paymentMethod.value}")
        }
      ) ++
        claim.getLeadDisplayDeclaration.get.getSingleSecurityDetails.map { securityDetails =>
          securityDetails.taxDetails.map(taxDetails =>
            messageFromMessageKey(s"tax-code.${taxDetails.getTaxCode}") -> Some(
              taxDetails.getAmount.toPoundSterlingString
            )
          )
        }.get ++
        Seq(
          "Total deposit or guarantee amount" -> claim.getLeadDisplayDeclaration.get.getSingleSecurityDetails.map {
            securityDetails =>
              securityDetails.getTotalAmount.toPoundSterlingString
          }
        )
    )
    getSummaryList(importerDetailsCard.get)            should containOnlyDefinedPairsOf(
      Seq(
        "Name"    -> claim.getLeadDisplayDeclaration.flatMap(_.consigneeName),
        "Email"   -> claim.getLeadDisplayDeclaration.flatMap(_.consigneeEmail),
        "Address" -> claim.getLeadDisplayDeclaration.flatMap(d =>
          d.displayResponseDetail.consigneeDetails.map(details =>
            d.establishmentAddress(details.establishmentAddress).mkString(" ")
          )
        )
      )
    )
    getSummaryList(declarantDetailsCard.get)           should containOnlyDefinedPairsOf(
      Seq(
        "Name"    -> claim.getLeadDisplayDeclaration.map(_.declarantName),
        "Email"   -> claim.getLeadDisplayDeclaration.flatMap(_.declarantEmailAddress),
        "Address" -> claim.getLeadDisplayDeclaration.map(d =>
          d.establishmentAddress(d.displayResponseDetail.declarantDetails.establishmentAddress).mkString(" ")
        )
      )
    )
  }

  "CheckDeclarationDetailsSingleSecurityController" when {
    "show page" must {
      def performAction(): Future[Result] = controller.show(FakeRequest())

      "display page" in forAllWith(
        ClaimGenerator(
          testParamsGenerator = mrnWithtRfsWithDisplayDeclarationWithoutIPROrENUGen,
          claimBuilder = buildSecuritiesClaimReadyForSelectingSecurities
        )
      ) { case (claim, _) =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkPageIsDisplayed(
          performAction(),
          messages("check-import-declaration-details.title"),
          doc => validateCheckDeclarationDetailsPage(doc, claim)
        )
      }

      "submit" must {

        def performAction(data: (String, String)*): Future[Result] =
          controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

        "redirect to confirm full repayment when reason for security is not NTAS or MDP" in {
          forAll(
            mrnWithRfsExcludingWithDisplayDeclarationGen(
              ReasonForSecurity.ntas + MissingPreferenceCertificate
            )
          ) { case (mrn, rfs, decl) =>
            val initialClaim = emptyClaim
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
            }

            checkIsRedirect(
              performAction(),
              routes.ConfirmFullRepaymentController.showFirst
            )
          }
        }

        "redirect to have documents ready when reason for security is NTAS or MDP" in {
          forAll(
            mrnWithRfsWithDisplayDeclarationGen(
              ReasonForSecurity.ntas + MissingPreferenceCertificate
            )
          ) { case (mrn, rfs, decl) =>
            val initialClaim = emptyClaim
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
            }

            checkIsRedirect(
              performAction(),
              routes.HaveDocumentsReadyController.show
            )
          }
        }
      }
    }
  }
}
