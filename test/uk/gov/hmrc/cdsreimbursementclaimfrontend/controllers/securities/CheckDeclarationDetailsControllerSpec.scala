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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.MissingPreferenceCertificate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils

import scala.collection.immutable.SortedMap
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class CheckDeclarationDetailsControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryMatchers {

  val mockClaimsService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService)
    )

  val controller: CheckDeclarationDetailsController = instanceOf[CheckDeclarationDetailsController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "check-declaration-details"

  def validateCheckDeclarationDetailsPage(
    doc: Document,
    claim: SecuritiesClaim
  ): Assertion = {

    val numberOfSecurities: Int = claim.getLeadDisplayDeclaration.map(_.getNumberOfSecurityDeposits).getOrElse(0)

    val headers       = doc.select("h2.govuk-heading-m").eachText().asScala
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

    headers       should not be empty
    summaryKeys   should not be empty
    summaryValues should not be empty

    val correctedAmounts: SortedMap[String, SortedMap[TaxCode, BigDecimal]] = claim.getSecuritiesReclaims

    headers.toSeq should contain theSameElementsAs Seq("Contact details", "Claim details")

    summaries.toSeq should containOnlyDefinedPairsOf(
      Seq(
        "Import Movement Reference Number (MRN)" -> claim.getLeadMovementReferenceNumber.map(_.value),
        "Importer name"                          -> claim.answers.displayDeclaration.flatMap(_.consigneeName),
        "Importer email"                         -> claim.answers.displayDeclaration.flatMap(_.consigneeEmail),
        "Importer address"                       -> claim.answers.displayDeclaration.flatMap(d =>
          d.displayResponseDetail.consigneeDetails.map(details =>
            d.establishmentAddress(details.establishmentAddress).mkString(" ")
          )
        ),
        "Importer telephone"                     -> claim.answers.displayDeclaration.flatMap(_.consigneeTelephone),
        "Declarant name"                         -> claim.answers.displayDeclaration.map(_.declarantName),
        "Declarant address"                      -> claim.answers.displayDeclaration.map(d =>
          d.establishmentAddress(d.displayResponseDetail.declarantDetails.establishmentAddress).mkString(" ")
        ),
        "Reason for security deposit"            -> claim.answers.reasonForSecurity.map(rfs =>
          messages(s"choose-reason-for-security.securities.${ReasonForSecurity.keyOf(rfs)}")
        ),
        "Date security deposit made"             -> claim.answers.displayDeclaration
          .flatMap(_.displayResponseDetail.btaDueDate)
          .flatMap(DateUtils.displayFormat),
        "Total security deposit value"           -> (if claim.getSecuritiesReclaims.isEmpty
                                           then None
                                           else
                                             claim.answers.displayDeclaration
                                               .map(
                                                 _.getTotalSecuritiesAmountFor(
                                                   claim.getSecuritiesReclaims.keySet
                                                 ).toPoundSterlingString
                                               )
        ),
        "Total security deposit paid"            -> (if claim.getSecuritiesReclaims.isEmpty
                                          then None
                                          else
                                            claim.answers.displayDeclaration
                                              .map(
                                                _.getTotalSecuritiesPaidAmountFor(
                                                  claim.getSecuritiesReclaims.keySet
                                                ).toPoundSterlingString
                                              )
        ),
        "Method of payment"                      -> (if correctedAmounts.isEmpty then Some("Unavailable")
                                else
                                  claim.answers.displayDeclaration
                                    .map(
                                      _.isAllSelectedSecuritiesEligibleForGuaranteePayment(
                                        correctedAmounts.keySet
                                      )
                                    )
                                    .map {
                                      case true  => "Guarantee"
                                      case false => "Bank account transfer"
                                    }
        )
      ) ++
        (if claim.getSecuritiesReclaims.isEmpty
         then Seq.empty
         else
           claim.answers.displayDeclaration
             .flatMap(_.getSecurityDepositIds)
             .getOrElse(Seq.empty)
             .zipWithIndex
             .map { (sid, securityIndex) =>
               s"Claim for security deposit or guarantee ${securityIndex + 1} of $numberOfSecurities" -> Some(
                 if correctedAmounts.contains(sid) then "Yes"
                 else "No"
               )
             }
        )
    )

  }

  def shouldShowCheckDischargedPage(rfs: ReasonForSecurity): Boolean =
    ReasonForSecurity.InwardProcessingRelief == rfs ||
      ReasonForSecurity.EndUseRelief == rfs

  "CheckDeclarationDetailsController" when {

    "show page" must {

      def performAction(): Future[Result] = controller.show(FakeRequest())

      "redirect to the enter movement reference number if empty claim" in {
        val initialClaim = emptyClaim

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialClaim))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterMovementReferenceNumberController.show
        )
      }

      "redirect to the enter declarant eori page if EORI hasn't pass validation" in {
        forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
          val declarationWithNonMatchingEori = decl.withConsigneeEori(Eori("foo")).withDeclarantEori(Eori("bar"))
          val depositIds                     = decl.getSecurityDepositIds.getOrElse(Seq.empty)
          whenever(depositIds.nonEmpty) {
            val initialClaim = emptyClaim
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, declarationWithNonMatchingEori)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
            }

            checkIsRedirect(
              performAction(),
              routes.EnterDeclarantEoriNumberController.show
            )
          }
        }
      }

      "display the page if at least one security has been selected" in {
        forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
          whenever(depositIds.nonEmpty) {
            val initialClaim = emptyClaim
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositId(depositIds.head))
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
              mockStoreSession(SessionData(initialClaim.submitCheckDeclarationDetailsChangeMode(true)))(Right(()))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(s"$messagesKey.title"),
              doc => validateCheckDeclarationDetailsPage(doc, initialClaim)
            )
          }
        }
      }

      "display the page if none security has been selected" in {
        forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
          val initialClaim = emptyClaim
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialClaim))
            mockStoreSession(SessionData(initialClaim.submitCheckDeclarationDetailsChangeMode(true)))(Right(()))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateCheckDeclarationDetailsPage(doc, initialClaim)
          )

        }
      }

    }

    "submit" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit(FakeRequest().withFormUrlEncodedBody(data*))

      "continue to the confirm full repayment page if some securities has been selected and RFS is not NTAS or MDP" in {
        forAll(
          mrnWithRfsExcludingWithDisplayDeclarationGen(
            ReasonForSecurity.ntas + MissingPreferenceCertificate
          )
        ) { case (mrn, rfs, decl) =>
          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
          whenever(depositIds.nonEmpty && !shouldShowCheckDischargedPage(rfs)) {

            val initialClaim = emptyClaim
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositId(depositIds.head))
              .map(_.submitCheckDeclarationDetailsChangeMode(true))
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
              mockStoreSession(
                SessionData(
                  initialClaim.submitCheckDeclarationDetailsChangeMode(false)
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(),
              routes.ConfirmFullRepaymentController.showFirst
            )
          }
        }
      }

      "continue to the have your documents ready page if some securities has been selected and RFS is NTAS or MDP" in {
        forAll(
          mrnWithRfsWithDisplayDeclarationGen(
            ReasonForSecurity.ntas + MissingPreferenceCertificate
          )
        ) { case (mrn, rfs, decl) =>
          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
          whenever(depositIds.nonEmpty && !shouldShowCheckDischargedPage(rfs)) {

            val initialClaim = emptyClaim
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositId(depositIds.head))
              .map(_.submitCheckDeclarationDetailsChangeMode(true))
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
              mockStoreSession(SessionData(initialClaim.submitCheckDeclarationDetailsChangeMode(false)))(Right(()))
            }

            checkIsRedirect(
              performAction(),
              routes.HaveDocumentsReadyController.show
            )
          }
        }
      }

      "continue to the confirm single deposit full repayment page if single security" in {
        forAll(
          mrnWithRfsWithSingleSecurityDisplayDeclarationGen(
            Set(ReasonForSecurity.RevenueDispute.value)
          )
        ) { case (mrn, rfs, decl) =>
          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
          whenever(depositIds.nonEmpty && !shouldShowCheckDischargedPage(rfs)) {

            val initialClaim = emptyClaim
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositId(depositIds.head))
              .map(_.submitCheckDeclarationDetailsChangeMode(true))
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
              mockStoreSession(
                SessionData(
                  initialClaim.submitCheckDeclarationDetailsChangeMode(false)
                )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(),
              routes.ConfirmSingleDepositRepaymentController.show
            )
          }
        }
      }

      "re-display the check declaration details page if none securities selected" in {
        forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
          whenever(depositIds.nonEmpty) {

            val initialClaim = emptyClaim
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .map(_.submitCheckDeclarationDetailsChangeMode(true))
              .getOrFail

            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(initialClaim))
              mockStoreSession(SessionData(initialClaim.submitCheckDeclarationDetailsChangeMode(false)))(Right(()))
            }

            checkIsRedirect(
              performAction(),
              routes.CheckDeclarationDetailsController.show
            )
          }
        }
      }

      "redirect to the CYA page when in change your answers mode" in forAll(completeClaimGen) { initialClaim =>
        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(initialClaim))
        }

        checkIsRedirect(
          performAction(),
          routes.CheckYourAnswersController.show
        )
      }
    }

  }

}
