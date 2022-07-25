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

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.scalacheck.Prop
import org.scalacheck.ShrinkLowPriority
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.BeforeAndAfterEach
import org.scalatest.GivenWhenThen
import org.scalatest.OptionValues
import play.api.http.Status.BAD_REQUEST
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
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.EmptyRetrieval
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.partialGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.securityIdWithMoreChoicesThanThoseSelected
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.securityIdWithTaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.selectCheckBoxes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.emptyJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SummaryInspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.http.HeaderCarrier

import java.text.NumberFormat
import java.util.Locale
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.Future

class ConfirmFullRepaymentControllerSpec
    extends PropertyBasedControllerSpec
    with SecuritiesJourneyTestData
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryInspectionAddress
    with SummaryMatchers
    with TypeCheckedTripleEquals
    with OptionValues
    with ShrinkLowPriority
    with Logging {
// class SecuritiesJourneySpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers with ShrinkLowPriority {
  val confirmFullRepaymentKey: String = "confirm-full-repayment"

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: ConfirmFullRepaymentController = instanceOf[ConfirmFullRepaymentController]
  implicit val messagesApi: MessagesApi          = controller.messagesApi
  implicit val messages: Messages                = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  val session: SessionData = SessionData.empty.copy(
    securitiesJourney = Some(SecuritiesJourney.empty(exampleEori).submitMovementReferenceNumber(exampleMrn))
  )

  def validateConfirmFullRepaymentPage(
    securityId: String,
    doc: Document,
    journey: SecuritiesJourney,
    isError: Boolean = false
  ) = {
    val title               = doc.select("title").eachText().asScala.toList
    val caption             = doc.select("span.govuk-caption-xl").eachText().asScala.toList
    val heading             = doc.select(".govuk-heading-xl").eachText().asScala.toList
    val legend              = doc.select(".govuk-fieldset__legend").eachText().asScala.toList
    val currencyFormatter   = NumberFormat.getCurrencyInstance(Locale.UK)
    val amountPaidFormatted = currencyFormatter.format(
      BigDecimal(journey.getSecurityDetailsFor(securityId).value.totalAmount)
    )
    title           should ===(
      List(
        (if (isError) "ERROR: "
         else
           "") + "Do you want to reclaim the full amount for this security? - Claim back import duty and VAT - GOV.UK"
      )
    )
    caption         should ===(List(s"Security ID: $securityId"))
    heading         should ===(List("Do you want to reclaim the full amount for this security?"))
    legend          should ===(
      List(
        s"The total value of $securityId is " +
          s"$amountPaidFormatted."
      )
    )
    radioItems(doc) should contain theSameElementsAs Seq(
      ("Yes", "true"),
      ("No", "false")
    )
    val spam = doc
    hasContinueButton(doc)
  }

  "Confirm Full Repayment Controller" when {
    "show page is called" must {
      def performAction(securityId: String): Future[Result] = controller.show(securityId)(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction("anySecurityId")) shouldBe NOT_FOUND
      }

      "AC1: Arrive on page; display the page on a complete journey" in
        forAll(completeJourneyGen) { journey =>
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          val securityId     = securityIdWithTaxCodes(journey).value
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(securityId),
            messageFromMessageKey(s"$confirmFullRepaymentKey.title"),
            doc => validateConfirmFullRepaymentPage(securityId, doc, journey)
          )
        }
    }

    "submit page is called" must {
      def performAction(securityId: String, data: Seq[(String, String)]): Future[Result] =
        controller.submit(securityId)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not succeed if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction("anySecurityId", Seq.empty)) shouldBe NOT_FOUND
      }

      "redirect to the error page if we have arrived with an invalid security deposit ID" in {
        mrnWithNonExportRfsWithDisplayDeclarationGen.sample.map { case (mrn, rfs, decl) =>
          val initialJourney = emptyJourney
            .submitMovementReferenceNumber(mrn)
            .submitReasonForSecurityAndDeclaration(rfs, decl)
            .flatMap(_.submitClaimDuplicateCheckStatus(false))
            .getOrFail

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData(initialJourney))
          }

          checkIsTechnicalErrorPage(performAction("anySecurityId", Seq.empty))

        }
      }

      //todo there is no CED CDSR-1776
      "AC2 move on to /choose-file-type page when yes is selected and continue is clicked (if RfS = CEP, CED, OPR, RED or MOD)" in {
        forAll(mrnWithNonExportRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
          whenever(
            Set[ReasonForSecurity](UKAPEntryPrice, OutwardProcessingRelief, RevenueDispute, ManualOverrideDeposit)
              .contains(rfs)
          ) {
            val depositIds: Seq[String]                                                = reclaims.map(_._1).distinct
            val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
              reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, amount) => (tc, amount) }).toSeq
            val journey: SecuritiesJourney                                             =
              emptyJourney
                .submitMovementReferenceNumber(mrn)
                .submitReasonForSecurityAndDeclaration(rfs, decl)
                .flatMap(a => a.submitDeclarantEoriNumber(decl.getDeclarantEori))
                .flatMap(a => a.submitConsigneeEoriNumber(decl.getConsigneeEori.value))
                .flatMap(_.submitClaimDuplicateCheckStatus(false))
                .flatMap(_.selectSecurityDepositIds(depositIds))
                .flatMapEach(
                  reclaimsBySecurityDepositId,
                  (journey: SecuritiesJourney) =>
                    (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                      journey
                        .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                )
                .getOrFail
            val securityId                                                             = journey.getSecurityDepositIds.head
            val sessionData                                                            = SessionData(journey)
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
              mockStoreSession(Right(()))
            }

            checkIsRedirect(
              performAction(securityId, Seq(confirmFullRepaymentKey -> "true")),
              routes.ChooseFileTypeController.show()
            )
          }
        }
      }

      "AC3 move on to /upload-file page when yes is selected and continue is clicked (if RFS = MDP, MDL, ACS, IPR, ENU, TA or MDC)" in {
        forAll(mrnIncludingExportRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
          whenever(
            Set[ReasonForSecurity](
              MissingPreferenceCertificate,
              MissingLicenseQuota,
              AccountSales,
              InwardProcessingRelief,
              EndUseRelief,
              TemporaryAdmission2Y,
              TemporaryAdmission6M,
              TemporaryAdmission3M,
              TemporaryAdmission2M,
              CommunitySystemsOfDutyRelief
            ).contains(rfs)
          ) {
            val depositIds: Seq[String]                                                = reclaims.map(_._1).distinct
            val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
              reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, amount) => (tc, amount) }).toSeq
            val journey: SecuritiesJourney                                             =
              emptyJourney
                .submitMovementReferenceNumber(mrn)
                .submitReasonForSecurityAndDeclaration(rfs, decl)
                .flatMap(a => a.submitDeclarantEoriNumber(decl.getDeclarantEori))
                .flatMap(a => a.submitConsigneeEoriNumber(decl.getConsigneeEori.value))
                .flatMap(_.submitClaimDuplicateCheckStatus(false))
                .flatMap(_.selectSecurityDepositIds(depositIds))
                .flatMapEach(
                  reclaimsBySecurityDepositId,
                  (journey: SecuritiesJourney) =>
                    (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                      journey
                        .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
                )
                .getOrFail
            val securityId                                                             = journey.getSecurityDepositIds.head
            val sessionData                                                            = SessionData(journey)
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
              mockStoreSession(Right(()))
            }

            checkIsRedirect(
              performAction(securityId, Seq(confirmFullRepaymentKey -> "true")),
              routes.ChooseFileTypeController.show()
            )
          }
        }
      }

      "AC4 move on to  /select-duties/:securityID page when no is selected and continue is clicked" in {
        forAll(mrnIncludingExportRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
          val depositIds: Seq[String] = reclaims.map(_._1).distinct

          val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
            reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, amount) => (tc, amount) }).toSeq

          val journey: SecuritiesJourney                                             =
            emptyJourney
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(a => a.submitDeclarantEoriNumber(decl.getDeclarantEori))
              .flatMap(a => a.submitConsigneeEoriNumber(decl.getConsigneeEori.value))
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositIds(depositIds))
              .flatMapEach(
                reclaimsBySecurityDepositId,
                (journey: SecuritiesJourney) =>
                  (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                    journey
                      .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
              )
              .getOrFail
          val securityId                                                             = journey.getSecurityDepositIds.head
          val sessionData                                                            = SessionData(journey)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
//            mockStoreSession(Right(()))
          }

          checkIsRedirect(
            performAction(securityId, Seq(confirmFullRepaymentKey -> "false")),
            routes.SelectDutiesController.show(securityId)
          )
        }

      }

      "AC5 - Complete journey - clicking continue with no option selected should display error" in {
        forAll(buildCompleteJourneyGen(submitFullAmount = true)) { journey =>
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          val securityId     = securityIdWithTaxCodes(journey).value
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
          }

          checkPageIsDisplayed(
            performAction(securityId, Seq()),
            messageFromMessageKey(s"$confirmFullRepaymentKey.title"),
            doc => validateConfirmFullRepaymentPage(securityId, doc, journey, isError = true),
            400
          )
        }
      }

      "AC5 clicking continue with no option selected should display error" in {
        forAll(mrnIncludingExportRfsWithDisplayDeclarationWithReclaimsGen) { case (mrn, rfs, decl, reclaims) =>
          val depositIds: Seq[String]                                                = reclaims.map(_._1).distinct
          val reclaimsBySecurityDepositId: Seq[(String, Seq[(TaxCode, BigDecimal)])] =
            reclaims.groupBy(_._1).mapValues(_.map { case (_, tc, amount) => (tc, amount) }).toSeq

          val journey: SecuritiesJourney                                             =
            emptyJourney
              .submitMovementReferenceNumber(mrn)
              .submitReasonForSecurityAndDeclaration(rfs, decl)
              .flatMap(a => a.submitDeclarantEoriNumber(decl.getDeclarantEori))
              .flatMap(a => a.submitConsigneeEoriNumber(decl.getConsigneeEori.value))
              .flatMap(_.submitClaimDuplicateCheckStatus(false))
              .flatMap(_.selectSecurityDepositIds(depositIds))
              .flatMapEach(
                reclaimsBySecurityDepositId,
                (journey: SecuritiesJourney) =>
                  (args: (String, Seq[(TaxCode, BigDecimal)])) =>
                    journey
                      .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(args._1, args._2.map(_._1))
              )
              .getOrFail
          val securityId                                                             = journey.getSecurityDepositIds.head
          val sessionData                                                            = SessionData(journey)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          //            val doc = Jsoup.parse(contentAsString(performAction(securityId, Seq(confirmFullRepaymentKey -> ""))))
          checkPageIsDisplayed(
            performAction(securityId, Seq()),
            "Do you want to reclaim the full amount for this security?",
            doc => validateConfirmFullRepaymentPage(securityId, doc, journey, true),
            400
          )
        }
      }

      "AC6 From CYA page, change answer from 'Yes' to 'No', clicking continue should go to the select duties controller"
        .ignore {
          // this AC is currently not met as we do not store the Yes/No selection, so auto fast forwarding is happening
          // we will need to introduce some kind of flag, to be discussed
          forAll(buildCompleteJourneyGen(submitFullAmount = true)) { journey =>
            val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
            val securityId     = securityIdWithTaxCodes(journey).value
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(updatedSession)
              mockStoreSession(Right(()))
            }

            checkIsRedirect(
              performAction(securityId, Seq(confirmFullRepaymentKey -> "false")),
              routes.SelectDutiesController.show(securityId)
            )
          }
        }

      "AC9 clicking continue with no option selected should display error" in {
        forAll(buildCompleteJourneyGen(submitFullAmount = false)) { journey =>
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          val securityId     = securityIdWithTaxCodes(journey).value
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockStoreSession(Right(()))
          }

          val result = performAction(securityId, Seq(confirmFullRepaymentKey -> "true"))
          checkIsRedirect(
            result,
            routes.CheckYourAnswersController.show()
          )

        // we cannot verify that all duties have been selected with the full amount as we don't have access to the
        // journey object...
        }
      }

    }
  }
}
object FooData {}
