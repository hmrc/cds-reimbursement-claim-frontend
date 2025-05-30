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
import org.scalacheck.Gen
import org.scalactic.TypeCheckedTripleEquals
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.partialGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.partialGenSingleDuty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.securityIdWithMoreChoicesThanThoseSelected
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.securityIdWithTaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.selectCheckBoxes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.SecurityDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.TaxDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SummaryInspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.SelectDutiesSummary

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity

class SelectDutiesControllerSpec
    extends PropertyBasedControllerSpec
    with SecuritiesJourneyTestData
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with SummaryInspectionAddress
    with SummaryMatchers
    with TypeCheckedTripleEquals
    with Logging {

  val messagesKey: String = "select-duties"

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: SelectDutiesController = instanceOf[SelectDutiesController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = {
    featureSwitch.enable(Feature.Securities)
    featureSwitch.disable(Feature.LimitedAccessSecurities)
  }

  val session: SessionData = SessionData(SecuritiesJourney.empty(exampleEori).submitMovementReferenceNumber(exampleMrn))

  def validateSelectDutiesPage(
    securityId: String,
    singleSecurity: Boolean,
    doc: Document,
    journey: SecuritiesJourney,
    isError: Boolean = false
  ) = {
    val title       = doc.select("title").first().text()
    val caption     = doc.select("span.govuk-caption-l").eachText().asScala.toList
    val formHeading = doc.select(".govuk-heading-l").eachText().asScala.toList

    val dutiesAvailable: Seq[TaxCode] =
      journey.getSecurityTaxCodesFor(securityId)

    val taxDetails: Seq[TaxDetails] =
      dutiesAvailable.flatMap(journey.getSecurityTaxDetailsFor(securityId, _).toList)

    val expectedTitle =
      if (singleSecurity)
        s"What do you want to claim? - Claim back import duty and VAT - GOV.UK"
      else
        s"Security deposit ID: $securityId: What do you want to claim? - Claim back import duty and VAT - GOV.UK"

    title       should ===(
      (if isError then "Error: "
       else "") + expectedTitle
    )
    caption     should ===(
      if singleSecurity then List.empty
      else List(s"Security deposit ID: $securityId")
    )
    formHeading should ===(
      if singleSecurity then List("What do you want to claim?")
      else List(s"Security deposit ID: $securityId What do you want to claim?")
    )

    checkboxes(doc) should contain theSameElementsAs dutiesAvailable.map(tc =>
      (s"${tc.value} - ${messages(s"$messagesKey.duty.${tc.value}")}", tc.value)
    )

    val checkboxDescriptions: List[String] = checkboxes(doc).map(_._1).toList
    val taxCodeDescriptions: List[String]  = taxDetails
      .map(_.getTaxCode)
      .sorted
      .map(tc => s"$tc - ${messages(s"$messagesKey.duty.$tc")}")
      .toList
    checkboxDescriptions should ===(taxCodeDescriptions)
  }

  "Select Duties Controller" when {
    "show page is called" must {
      def performAction(securityId: String): Future[Result] = controller.show(securityId)(FakeRequest())
      def performActionShowFirst(): Future[Result]          = controller.showFirst()(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction("anySecurityId")) shouldBe NOT_FOUND
      }

      "display the page on a journey that has ACC14 tax codes" in
        forAll(completeJourneyWithoutIPROrENUGen) { journey =>
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          securityIdWithTaxCodes(journey).fold(
            (status(performAction("anySecurityId")) shouldBe NOT_FOUND): Any
          ) { securityId =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(updatedSession)
            }

            checkPageIsDisplayed(
              performAction(securityId),
              messageFromMessageKey(s"$messagesKey.securities.title"),
              doc => validateSelectDutiesPage(securityId, journey.isSingleSecurity, doc, journey)
            )
          }
        }
      "display the page on a journey with a single security deposit" in
        forAll(buildCompleteJourneyGen(numberOfSecurityDetails = Some(1))) { journey =>
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          securityIdWithTaxCodes(journey).fold(
            (status(performAction("anySecurityId")) shouldBe NOT_FOUND): Any
          ) { securityId =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(updatedSession)
            }

            checkPageIsDisplayed(
              performAction(securityId),
              messageFromMessageKey(s"$messagesKey.securities.title"),
              doc => validateSelectDutiesPage(securityId, journey.isSingleSecurity, doc, journey)
            )
          }
        }

      "select duty and redirect to enter claim page on a journey with a single duty type" in
        forAll(partialGenSingleDuty) { journey =>
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          securityIdWithTaxCodes(journey).fold(
            (status(performAction("anySecurityId")) shouldBe NOT_FOUND): Any
          ) { securityId =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(updatedSession)
              mockStoreSession(Right(()))
            }

            checkIsRedirect(
              performActionShowFirst(),
              routes.EnterClaimController.showFirst(securityId)
            )
          }
        }
    }

    "submit page is called" must {
      def performAction(securityId: String, data: Seq[(String, String)]): Future[Result] =
        controller.submit(securityId)(FakeRequest().withFormUrlEncodedBody(data*))

      "not succeed if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction("anySecurityId", Seq.empty)) shouldBe NOT_FOUND
      }

      "redirect to ineligible page when there are no duties returned from ACC14" in
        forAll(genReasonForSecurity) { rfs =>
          val journey        = securitiesJourneyWithMrnAndRfsAndDeclaration(rfs)
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          securityIdWithTaxCodes(journey).fold {
            inAnyOrder {
              mockAuthWithDefaultRetrievals()
              mockGetSession(updatedSession)
            }
            checkIsRedirect(
              performAction("anySecurityId", Seq.empty),
              baseRoutes.IneligibleController.ineligible
            )
          } { securityId =>
            throw new Throwable(s"unexpectedly found securityId $securityId with duties available")
          }
        }

      "move on to enter claim page when at least one checkbox has been changed" in
        forAll(partialGen) { journey =>
          val securityIdOpt = securityIdWithMoreChoicesThanThoseSelected(journey)
          whenever(journey.answers.correctedAmounts.nonEmpty && securityIdOpt.isDefined) {
            val securityId                                        = securityIdOpt.get
            val availableTaxCodes: List[TaxCode]                  = journey.getSecurityTaxCodesFor(securityId).toList
            val previouslySelectedTaxCodes: List[TaxCode]         =
              journey.answers.correctedAmounts
                .map(_(securityId))
                .toList
                .flatMap(_.keys)
            val checkBoxesToSelect                                =
              selectCheckBoxes(
                journey,
                securityId,
                availableTaxCodes.zipWithIndex,
                previouslySelectedTaxCodes.zipWithIndex
              )
            val updatedSecuritiesReclaims                         =
              checkBoxesToSelect
                .map(_._2)
                .map(TaxCode(_))
                .sorted
            val updatedJourney: Either[String, SecuritiesJourney] =
              journey.selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(securityId, updatedSecuritiesReclaims)

            updatedJourney.fold(
              error => {
                logger.warn(s"unable to update journey with new tax codes $error")
                assert(false)
              },
              updatedJourney => {
                val updatedSession =
                  SessionData.empty.copy(securitiesJourney = Some(updatedJourney))
                inAnyOrder {
                  mockAuthWithDefaultRetrievals()
                  mockGetSession(updatedSession)
                }
                checkIsRedirect(
                  performAction(securityId, checkBoxesToSelect),
                  routes.EnterClaimController.show(securityId, TaxCode(checkBoxesToSelect.head._2))
                )
              }
            )
          }
        }

      "redisplay the page with an error when no checkboxes are selected" in forAll(completeJourneyWithoutIPROrENUGen) {
        journey =>
          whenever(journey.answers.correctedAmounts.nonEmpty) {
            securityIdWithTaxCodes(journey).fold(
              throw new Throwable("unexpectedly found securities reclaims already populated")
            ) { securityId =>
              val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(updatedSession)
              }
              checkPageIsDisplayed(
                performAction(securityId, Seq.empty),
                messageFromMessageKey(s"$messagesKey.securities.title"),
                doc =>
                  validateSelectDutiesPage(
                    securityId = securityId,
                    journey.isSingleSecurity,
                    doc = doc,
                    journey = journey,
                    isError = true
                  )
              )
            }
          }
      }

      "redirect back to the CYA when the same duties has been selected" in forAll(completeJourneyWithoutIPROrENUGen) {
        journey =>
          whenever(journey.answers.correctedAmounts.nonEmpty) {
            journey.getSelectedDepositIds.foreach { securityId =>
              val selectedDuties: Seq[TaxCode] =
                journey.getSelectedDutiesFor(securityId).get

              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(journey))
              }

              checkIsRedirect(
                performAction(
                  securityId,
                  selectedDuties.map(taxCode => "select-duties[]" -> taxCode.value)
                ),
                routes.CheckYourAnswersController.show
              )
            }
          }
      }

      "redirect back to the check claim page when duty has been de-selected" in forAll(
        completeJourneyWithoutIPROrENUGen
      ) { journey =>
        journey.getSelectedDepositIds.foreach { securityId =>
          val selectedDuties: Seq[TaxCode] =
            journey.getSelectedDutiesFor(securityId).get

          whenever(selectedDuties.size > 1) {
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
              mockStoreSession(Right(()))
            }

            checkIsRedirect(
              performAction(
                securityId,
                selectedDuties.halfNonEmpty.map(taxCode => "select-duties[]" -> taxCode.value)
              ),
              routes.CheckClaimDetailsController.show
            )
          }
        }

      }

      "redirect to the check claim page when new duty has been selected" in forAll(completeJourneyWithoutIPROrENUGen) {
        journey =>
          whenever(journey.answers.correctedAmounts.nonEmpty) {
            journey.getSelectedDepositIds.foreach { securityId =>
              val availableDuties: Set[TaxCode] =
                journey.getSecurityTaxCodesFor(securityId).toSet

              val selectedDuties: Set[TaxCode] =
                journey.getSelectedDutiesFor(securityId).get.toSet

              (availableDuties -- selectedDuties).foreach { taxCode =>
                inSequence {
                  mockAuthWithDefaultRetrievals()
                  mockGetSession(SessionData(journey))
                  mockStoreSession(Right(()))
                }

                checkIsRedirect(
                  performAction(
                    securityId,
                    (selectedDuties + taxCode).toSeq.map(tc => "select-duties[]" -> tc.value)
                  ),
                  routes.CheckClaimDetailsController.show
                )
              }
            }
          }
      }
    }
  }
}
object SelectDutiesControllerSpec {
  private def getFirstSecurityWithTaxCodes(securityDetails: Option[List[SecurityDetails]]): Option[String] =
    securityDetails.fold(Option.empty[String])(a =>
      a.find { b =>
        b.taxDetails.nonEmpty
      }.map(_.securityDepositId)
    )
  def securityIdWithTaxCodes(journey: SecuritiesJourney): Option[String]                                   =
    journey.getLeadDisplayDeclaration
      .map { displayDeclaration =>
        displayDeclaration.displayResponseDetail.securityDetails
      }
      .flatMap(getFirstSecurityWithTaxCodes)

  def securityIdWithMoreChoicesThanThoseSelected(journey: SecuritiesJourney): Option[String] = {
    val securityId = securityIdWithTaxCodes(journey)
    securityId.fold(Option.empty[String]) { securityId =>
      val choices: List[TaxCode]  = journey.getSecurityTaxCodesFor(securityId).toList
      val selected: List[TaxCode] = journey.answers.correctedAmounts
        .map(_(securityId))
        .map(_.keys)
        .toList
        .flatten
      if choices.size > selected.size then Some(securityId) else None
    }
  }

  val partialGen: Gen[SecuritiesJourney] =
    buildCompleteJourneyGen(
      acc14DeclarantMatchesUserEori = true,
      acc14ConsigneeMatchesUserEori = false,
      allDutiesGuaranteeEligibleOpt = None,
      hasConsigneeDetailsInACC14 = true,
      submitConsigneeDetails = false,
      submitContactDetails = false,
      submitContactAddress = false,
      submitBankAccountDetails = false,
      submitBankAccountType = false,
      reasonsForSecurity = ReasonForSecurity.values - ReasonForSecurity.InwardProcessingRelief
    )

  val partialGenSingleDuty: Gen[SecuritiesJourney] =
    buildCompleteJourneyGen(
      acc14DeclarantMatchesUserEori = true,
      acc14ConsigneeMatchesUserEori = false,
      allDutiesGuaranteeEligibleOpt = None,
      hasConsigneeDetailsInACC14 = true,
      submitConsigneeDetails = false,
      submitContactDetails = false,
      submitContactAddress = false,
      submitBankAccountDetails = false,
      submitBankAccountType = false,
      reasonsForSecurity = ReasonForSecurity.values - ReasonForSecurity.InwardProcessingRelief,
      numberOfDutyTypes = Some(1)
    )

  def getSelectedIndices(
    allCodes: List[DutyAmount],
    selected: List[TaxCode],
    messages: Messages
  ): Seq[(DutyAmount, Int)] =
    SelectDutiesSummary(allCodes)(messages)
      .map(_.duty)
      .zipWithIndex
      .filter(a => selected.contains(a._1.taxCode))

  def selectCheckBoxes(
    journey: SecuritiesJourney,
    securityId: String,
    availableTaxCodes: Seq[(TaxCode, Int)],
    previouslySelected: Seq[(TaxCode, Int)]
  ): Seq[(String, String)] =
    (previouslySelected.size, availableTaxCodes.size) match {
      case (0, _)          =>
        Seq("select-duties[0]" -> journey.getSecurityTaxCodesFor(securityId).toList.head.value)
      case (a, b) if a < b =>
        availableTaxCodes
          .find { a =>
            !previouslySelected.map(_._1).contains(a._1)
          }
          .fold(
            throw new Throwable("for this test at least one checkbox must be selected")
          )(a => Seq(s"select-duties[${a._2}]" -> s"${a._1}"))
      case _               =>
        throw new Throwable(
          "cannot test complete journey when all securities are selected or invalid securities are selected"
        )
    }
}
