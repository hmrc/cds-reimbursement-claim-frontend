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
import org.scalacheck.Gen
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.MethodOfPaymentSummary

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class CheckDuplicateDeclarationDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  val mockClaimService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimService)
    )

  val controller: CheckDuplicateDeclarationDetailsController = instanceOf[CheckDuplicateDeclarationDetailsController]

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
        "MRN" -> claim.getDuplicateImportDeclaration.map(_.getMRN.value)
      )
    )
    getSummaryList(importDetailsCard.get)    should containOnlyDefinedPairsOf(
      Seq(
        claim.getDuplicateImportDeclaration.get.getMaybeLRN match {
          case Some(lrn) => "Local Reference Number (LRN)" -> Some(lrn)
          case _         => ""                             -> None
        },
        "Date of import" -> DateUtils.displayFormat(
          claim.getDuplicateImportDeclaration.map(_.displayResponseDetail.acceptanceDate)
        )
      )
    )
    getSummaryList(dutiesAndVATCard.get)     should containOnlyDefinedPairsOf(
      Seq(
        "Method of payment" -> claim.getDuplicateImportDeclaration.get.getMethodsOfPayment
          .map { methods =>
            MethodOfPaymentSummary(methods)
          }
      ) ++
        claim.getDuplicateImportDeclaration.get.getNdrcDutiesWithAmount
          .map(_.map { case (taxCode, amount) =>
            messageFromMessageKey(s"tax-code.$taxCode") -> Some(
              amount.toPoundSterlingString
            )
          })
          .get ++
        Seq(
          "Total" -> claim.getDuplicateImportDeclaration.map(_.totalPaidCharges.toPoundSterlingString)
        )
    )
    getSummaryList(importerDetailsCard.get)  should containOnlyDefinedPairsOf(
      Seq(
        "Name"    -> claim.getDuplicateImportDeclaration.flatMap(_.consigneeName),
        "Email"   -> claim.getDuplicateImportDeclaration.flatMap(_.consigneeEmail),
        "Address" -> claim.getDuplicateImportDeclaration.flatMap(d =>
          d.displayResponseDetail.consigneeDetails.map(details =>
            d.establishmentAddress(details.establishmentAddress).mkString(" ")
          )
        )
      )
    )
    getSummaryList(declarantDetailsCard.get) should containOnlyDefinedPairsOf(
      Seq(
        "Name"    -> claim.getDuplicateImportDeclaration.map(_.declarantName),
        "Email"   -> claim.getDuplicateImportDeclaration.flatMap(_.declarantEmailAddress),
        "Address" -> claim.getDuplicateImportDeclaration.map(d =>
          d.establishmentAddress(d.displayResponseDetail.declarantDetails.establishmentAddress).mkString(" ")
        )
      )
    )
  }

  val claimGen: Gen[OverpaymentsSingleClaim] =
    for
      j1            <- buildClaimFromAnswersGen(answersUpToBasisForClaimGen())
                         .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
      mrn           <- genMRN
      consigneeEori <- genEori
      declarantEori <- genEori
      decl           = buildImportDeclaration(
                         id = mrn.value,
                         declarantEORI = declarantEori,
                         consigneeEORI = Some(consigneeEori),
                         Seq((TaxCode.A50, 100, false), (TaxCode.B00, 50, false))
                       )
    yield j1
      .submitDuplicateMovementReferenceNumberAndDeclaration(mrn, decl)
      .flatMapWhenDefined(decl.getConsigneeEori)(_.checkConsigneeEoriNumberWithDuplicateDeclaration)
      .flatMap(_.checkDeclarantEoriNumberWithDuplicateDeclaration(decl.getDeclarantEori))
      .getOrFail

  "Check Duplicate Declaration Details Controller" when {
    "Check Duplicate Declaration Details page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "display the page if duplicate declaration exists" in {
        val claim =
          claimGen.sample.getOrElse(fail("Claim building has failed."))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messagesKey.duplicate.title"),
          doc => validateCheckDeclarationDetailsPage(doc, claim)
        )
      }

      "redirect if duplicate declaration not expected" in {
        val claim =
          buildClaimFromAnswersGen(answersUpToBasisForClaimGen())
            .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DutySuspension))
            .sample
            .get

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterAdditionalDetailsController.show
        )
      }

      "redirect if duplicate declaration expected but not provided" in {
        val claim =
          buildClaimFromAnswersGen(answersUpToBasisForClaimGen())
            .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
            .sample
            .get

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterDuplicateMovementReferenceNumberController.show
        )
      }

      "redirect if duplicate declaration not verified" in {
        val claim =
          (for
            j1   <- buildClaimFromAnswersGen(answersUpToBasisForClaimGen())
                      .map(_.submitBasisOfClaim(BasisOfOverpaymentClaim.DuplicateEntry))
            mrn  <- genMRN
            eori <- genEori
            decl  = buildImportDeclaration(
                      id = mrn.value,
                      declarantEORI = eori
                    )
          yield j1
            .submitDuplicateMovementReferenceNumberAndDeclaration(mrn, decl)
            .getOrFail).sample.get

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterImporterEoriNumberOfDuplicateDeclaration.show
        )
      }
    }

    "Submit Check Duplicate Declaration Details page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "submit" in {
        val claim: OverpaymentsSingleClaim =
          claimGen.sample.get

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(
            SessionData(
              claim.withEnterContactDetailsMode(true)
            )
          )(Right(()))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterAdditionalDetailsController.show
        )
      }
    }
  }
}
