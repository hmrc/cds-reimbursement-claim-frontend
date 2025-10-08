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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle
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
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.OverpaymentsSingleClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.MethodOfPaymentSummary

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class CheckDeclarationDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryMatchers {

  val mockClaimService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimService)
    )

  val controller: CheckDeclarationDetailsController = instanceOf[CheckDeclarationDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val session: SessionData = SessionData(claimWithMrnAndDeclaration)

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
    claim: OverpaymentsSingleClaim
  ): Assertion = {

    val claimDetailsCard     = getSummaryCardByTitle(doc, "Claim details")
    val importDetailsCard    = getSummaryCardByTitle(doc, "Import details")
    val dutiesAndVATCard     = getSummaryCardByTitle(doc, "Duties and VAT")
    val importerDetailsCard  = getSummaryCardByTitle(doc, "Importer details")
    val declarantDetailsCard = getSummaryCardByTitle(doc, "Declarant details")

    claimDetailsCard.isDefined     should not be false
    importDetailsCard.isDefined    should not be false
    dutiesAndVATCard.isDefined     should not be false
    importerDetailsCard.isDefined  should not be false
    declarantDetailsCard.isDefined should not be false

    getSummaryList(claimDetailsCard.get)     should containOnlyDefinedPairsOf(
      Seq(
        "MRN" -> claim.getLeadMovementReferenceNumber.map(_.value)
      )
    )
    getSummaryList(importDetailsCard.get)    should containOnlyDefinedPairsOf(
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
    getSummaryList(dutiesAndVATCard.get)     should containOnlyDefinedPairsOf(
      Seq(
        "Method of payment" -> claim.getLeadDisplayDeclaration.get.getMethodsOfPayment
          .map { methods =>
            MethodOfPaymentSummary(methods)
          }
      ) ++
        claim.getLeadDisplayDeclaration.get.getNdrcDutiesWithAmount
          .map(_.map { case (taxCode, amount) =>
            messageFromMessageKey(s"tax-code.$taxCode") -> Some(
              amount.toPoundSterlingString
            )
          })
          .get ++
        Seq(
          "Total" -> claim.getLeadDisplayDeclaration.map(_.totalPaidCharges.toPoundSterlingString)
        )
    )
    getSummaryList(importerDetailsCard.get)  should containOnlyDefinedPairsOf(
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
    getSummaryList(declarantDetailsCard.get) should containOnlyDefinedPairsOf(
      Seq(
        "Name"    -> claim.getLeadDisplayDeclaration.map(_.declarantName),
        "Email"   -> claim.getLeadDisplayDeclaration.flatMap(_.declarantEmailAddress),
        "Address" -> claim.getLeadDisplayDeclaration.map(d =>
          d.establishmentAddress(d.displayResponseDetail.declarantDetails.establishmentAddress).mkString(" ")
        )
      )
    )
  }

  "Check Declaration Details Controller" when {
    "Check Declaration Details page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "display the page" in {
        val claim = buildCompleteClaimGen(
          acc14DeclarantMatchesUserEori = false,
          acc14ConsigneeMatchesUserEori = false
        ).sample.getOrElse(fail("Claim building has failed."))

        val sessionToAmend = SessionData(claim)

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(sessionToAmend)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc => validateCheckDeclarationDetailsPage(doc, claim)
        )
      }
    }

    "Submit Check Declaration Details page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "submit" in {
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.BasisForClaimController.show
        )
      }
    }
  }
}
