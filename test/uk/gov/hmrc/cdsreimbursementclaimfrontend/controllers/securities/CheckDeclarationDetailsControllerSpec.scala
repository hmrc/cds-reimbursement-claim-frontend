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
import play.api.http.Status.NOT_FOUND
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.status
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.EitherOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils

import scala.collection.immutable.SortedMap
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

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

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private val messagesKey: String = "check-declaration-details"

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  def validateCheckDeclarationDetailsPage(
    doc: Document,
    journey: SecuritiesJourney
  ): Assertion = {
    val headers       = doc.select("h2.govuk-heading-m").eachText().asScala
    val summaryKeys   = doc.select(".govuk-summary-list__key").eachText()
    val summaryValues = doc.select(".govuk-summary-list__value").eachText()
    val summaries     = summaryKeys.asScala.zip(summaryValues.asScala)

    headers       should not be empty
    summaryKeys   should not be empty
    summaryValues should not be empty

    val correctedAmounts: SortedMap[String, SortedMap[TaxCode, BigDecimal]] = journey.getSecuritiesReclaims

    headers.toSeq should contain theSameElementsAs Seq("Contact details", "Claim details")

    summaries.toSeq should containOnlyDefinedPairsOf(
      Seq(
        "Import MRN"                   -> journey.getLeadMovementReferenceNumber.map(_.value),
        "Importer name"                -> journey.answers.displayDeclaration.flatMap(_.consigneeName),
        "Importer email"               -> journey.answers.displayDeclaration.flatMap(_.consigneeEmail),
        "Importer address"             -> journey.answers.displayDeclaration.flatMap(d =>
          d.displayResponseDetail.consigneeDetails.map(details =>
            d.establishmentAddress(details.establishmentAddress).mkString(" ")
          )
        ),
        "Importer telephone"           -> journey.answers.displayDeclaration.flatMap(_.consigneeTelephone),
        "Declarant name"               -> journey.answers.displayDeclaration.map(_.declarantName),
        "Declarant address"            -> journey.answers.displayDeclaration.map(d =>
          d.establishmentAddress(d.displayResponseDetail.declarantDetails.establishmentAddress).mkString(" ")
        ),
        "Reason for security deposit"  -> journey.answers.reasonForSecurity.map(rfs =>
          messages(s"choose-reason-for-security.securities.${ReasonForSecurity.keyOf(rfs)}")
        ),
        "Date duty to be collected"    -> journey.answers.displayDeclaration
          .map(_.displayResponseDetail.acceptanceDate)
          .flatMap(DateUtils.displayFormat),
        "Date security deposit made"   -> journey.answers.displayDeclaration
          .flatMap(_.displayResponseDetail.btaDueDate)
          .flatMap(DateUtils.displayFormat),
        "Total security deposit value" -> journey.answers.displayDeclaration
          .map(_.getTotalSecuritiesAmountFor(journey.getSecuritiesReclaims.keySet).toPoundSterlingString),
        "Total security deposit paid"  -> journey.answers.displayDeclaration
          .map(_.getTotalSecuritiesPaidAmountFor(journey.getSecuritiesReclaims.keySet).toPoundSterlingString),
        "Method of payment"            -> (if (correctedAmounts.isEmpty)
                                  Some("Unavailable")
                                else
                                  journey.answers.displayDeclaration
                                    .map(
                                      _.isAllSelectedSecuritiesEligibleForGuaranteePayment(
                                        correctedAmounts.keySet
                                      )
                                    )
                                    .map {
                                      case true  => "Guarantee"
                                      case false => "Bank account transfer"
                                    })
      ) ++
        journey.answers.displayDeclaration
          .flatMap(_.getSecurityDepositIds)
          .getOrElse(Seq.empty)
          .map { sid =>
            s"Claim for $sid" -> Some(
              if (correctedAmounts.contains(sid))
                "Yes"
              else
                "No"
            )
          }
    )

  }

  def shouldShowCheckDischargedPage(rfs: ReasonForSecurity): Boolean =
    ReasonForSecurity.InwardProcessingRelief == rfs ||
      ReasonForSecurity.EndUseRelief == rfs

  "CheckDeclarationDetailsController" when {

    "show page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction()) shouldBe NOT_FOUND
      }

      "redirect to the enter movement reference number if empty journey" in {
        val initialJourney = emptyJourney

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkIsRedirect(
          performAction(),
          routes.EnterMovementReferenceNumberController.show()
        )
      }

      "redirect to the enter declarant eori page if EORI hasn't pass validation" in {
        forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
          val declarationWithNonMatchingEori = decl.withConsigneeEori(Eori("foo")).withDeclarantEori(Eori("bar"))
          val depositIds                     = decl.getSecurityDepositIds.getOrElse(Seq.empty)
          whenever(depositIds.nonEmpty) {
            val initialJourney = emptyJourney
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, declarationWithNonMatchingEori)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .getOrFail

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(initialJourney))
            }

            checkIsRedirect(
              performAction(),
              routes.EnterDeclarantEoriNumberController.show()
            )
          }
        }
      }

      "display the page if at least one security has been selected" in {
        forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
          whenever(depositIds.nonEmpty) {
            val initialJourney = emptyJourney
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositId(depositIds.head))
              .getOrFail

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(initialJourney))
              mockStoreSession(SessionData(initialJourney.submitCheckDeclarationDetailsChangeMode(true)))(Right(()))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey(s"$messagesKey.title"),
              doc => validateCheckDeclarationDetailsPage(doc, initialJourney)
            )
          }
        }
      }

      "display the page if none security has been selected" in {
        forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
          val initialJourney = emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(initialJourney.submitCheckDeclarationDetailsChangeMode(true)))(Right(()))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$messagesKey.title"),
            doc => validateCheckDeclarationDetailsPage(doc, initialJourney)
          )

        }
      }

    }

    "submit" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)

        status(performAction()) shouldBe NOT_FOUND
      }

      "continue to the check claimant details page if some securities has been selected" in {
        forAll(
          mrnWithRfsExcludingWithDisplayDeclarationGen(
            ReasonForSecurity.temporaryAdmissions.toList
          )
        ) { case (mrn, rfs, decl) =>
          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
          whenever(depositIds.nonEmpty && !shouldShowCheckDischargedPage(rfs)) {

            val initialJourney = emptyJourney
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositId(depositIds.head))
              .map(_.submitCheckDeclarationDetailsChangeMode(true))
              .getOrFail

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(initialJourney))
              mockStoreSession(SessionData(initialJourney.submitCheckDeclarationDetailsChangeMode(false)))(Right(()))
            }

            checkIsRedirect(
              performAction(),
              routes.CheckClaimantDetailsController.show()
            )
          }
        }
      }

      "continue to the export method page if some securities has been selected (Temporary Admission)" in {
        forAll(
          mrnWithRfsWithDisplayDeclarationGen(
            ReasonForSecurity.temporaryAdmissions.toList
          )
        ) { case (mrn, rfs, decl) =>
          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
          whenever(depositIds.nonEmpty && !shouldShowCheckDischargedPage(rfs)) {

            val initialJourney = emptyJourney
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositId(depositIds.head))
              .map(_.submitCheckDeclarationDetailsChangeMode(true))
              .getOrFail

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(initialJourney))
              mockStoreSession(SessionData(initialJourney.submitCheckDeclarationDetailsChangeMode(false)))(Right(()))
            }

            checkIsRedirect(
              performAction(),
              routes.ChooseExportMethodController.show()
            )
          }
        }
      }

      "re-display the check declaration details page if none securities selected" in {
        forAll(mrnWithtRfsWithDisplayDeclarationGen) { case (mrn, rfs, decl) =>
          val depositIds = decl.getSecurityDepositIds.getOrElse(Seq.empty)
          whenever(depositIds.nonEmpty) {

            val initialJourney = emptyJourney
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .map(_.submitCheckDeclarationDetailsChangeMode(true))
              .getOrFail

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(initialJourney))
              mockStoreSession(SessionData(initialJourney.submitCheckDeclarationDetailsChangeMode(false)))(Right(()))
            }

            checkIsRedirect(
              performAction(),
              routes.CheckDeclarationDetailsController.show()
            )
          }
        }
      }

      "redirect to the CYA page when in change your answers mode" in forAll(completeJourneyGen) { initialJourney =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData(initialJourney))
        }

        checkIsRedirect(
          performAction(),
          routes.CheckYourAnswersController.show()
        )
      }
    }

  }

}
