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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.partialGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.securityIdWithMoreChoicesThanThoseSelected
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.securityIdWithTaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesControllerSpec.selectCheckBoxes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SummaryInspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.SecurityDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.TaxDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary.SelectDutiesSummary

import scala.collection.JavaConverters._
import scala.concurrent.Future

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

  override def beforeEach(): Unit = featureSwitch.enable(Feature.Securities)

  val session: SessionData = SessionData.empty.copy(
    securitiesJourney = Some(SecuritiesJourney.empty(exampleEori).submitMovementReferenceNumber(exampleMrn))
  )

  def validateSelectDutiesPage(
    securityId: String,
    doc: Document,
    journey: SecuritiesJourney,
    isError: Boolean = false
  ) = {
    val title       = doc.select("title").eachText().asScala.toList
    val caption     = doc.select("span.govuk-caption-xl").eachText().asScala.toList
    val formHeading = doc.select(".govuk-heading-xl").eachText().asScala.toList

    val dutiesAvailable: Seq[TaxCode] =
      journey.getSecurityTaxCodesFor(securityId)

    val taxDetails: Seq[TaxDetails] =
      dutiesAvailable.flatMap(journey.getSecurityTaxDetailsFor(securityId, _).toList)

    title                    should ===(
      List(
        (if (isError) "ERROR: "
         else "") + "Select the duties you want to claim for - Claim back import duty and VAT - GOV.UK"
      )
    )
    caption                  should ===(List(s"Security ID: $securityId"))
    formHeading              should ===(List("Select the duties you want to claim for"))
    checkboxes(doc)          should contain theSameElementsAs dutiesAvailable.map(tc =>
      (s"${tc.value} - ${messages(s"$messagesKey.duty.${tc.value}")}", tc.value)
    )
    checkboxesWithHints(doc) should contain theSameElementsAs taxDetails.map(td =>
      (
        s"${td.getTaxCode} - ${messages(s"$messagesKey.duty.${td.getTaxCode}")}",
        messages(s"$messagesKey.duty.caption").format(td.getAmount.toPoundSterlingString)
      )
    )
    val checkboxDescriptions: List[String] = checkboxes(doc).map(_._1).toList
    val taxCodeDescriptions: List[String] = taxDetails.map(_.getTaxCode).sorted
      .map(tc => s"$tc - ${messages(s"$messagesKey.duty.$tc")}").toList
    checkboxDescriptions should ===(taxCodeDescriptions)
  }

  "Select Duties Controller" when {
    "show page is called" must {
      def performAction(securityId: String): Future[Result] = controller.show(securityId)(FakeRequest())

      "not find the page if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction("anySecurityId")) shouldBe NOT_FOUND
      }

      "display the page on a journey that has ACC14 tax codes" in
        forAll(completeJourneyGen) { journey =>
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          securityIdWithTaxCodes(journey).fold(
            (status(performAction("anySecurityId")) shouldBe NOT_FOUND): Any
          ) { securityId =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(updatedSession)
            }

            checkPageIsDisplayed(
              performAction(securityId),
              messageFromMessageKey(s"$messagesKey.title"),
              doc => validateSelectDutiesPage(securityId, doc, journey)
            )
          }
        }
    }

    "submit page is called" must {
      def performAction(securityId: String, data: Seq[(String, String)]): Future[Result] =
        controller.submit(securityId)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not succeed if securities feature is disabled" in {
        featureSwitch.disable(Feature.Securities)
        status(performAction("anySecurityId", Seq.empty)) shouldBe NOT_FOUND
      }

      "redirect to ineligible page when there are no duties returned from ACC14" in
        forAll(emptyJourney) { journey =>
          val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
          securityIdWithTaxCodes(journey).fold {
            inAnyOrder {
              mockAuthWithNoRetrievals()
              mockGetSession(updatedSession)
            }
            checkIsRedirect(
              performAction("anySecurityId", Seq.empty),
              baseRoutes.IneligibleController.ineligible()
            )
          } { securityId =>
            throw new Throwable(s"unexpectedly found securityId $securityId with duties available")
          }
        }

      "move on to enter claim page when at least one checkbox has been changed" in
        forAll(partialGen) { journey =>
          val securityIdOpt = securityIdWithMoreChoicesThanThoseSelected(journey)
          whenever(journey.answers.securitiesReclaims.nonEmpty && securityIdOpt.isDefined) {
            val securityId                                        = securityIdOpt.get
            val availableTaxCodes: List[TaxCode]                  = journey.getSecurityTaxCodesFor(securityId).toList
            val previouslySelectedTaxCodes: List[TaxCode]         =
              journey.answers.securitiesReclaims
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
                  mockAuthWithNoRetrievals()
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

      "redisplay the page with an error when no checkboxes are selected" in forAll(completeJourneyGen) { journey =>
        whenever(journey.answers.securitiesReclaims.nonEmpty) {
          securityIdWithTaxCodes(journey).fold(
            throw new Throwable(s"unexpectedly found securities reclaims already populated")
          ) { securityId =>
            val updatedSession = SessionData.empty.copy(securitiesJourney = Some(journey))
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(updatedSession)
            }
            checkPageIsDisplayed(
              performAction(securityId, Seq.empty),
              messageFromMessageKey(s"$messagesKey.title"),
              doc => validateSelectDutiesPage(securityId, doc, journey, isError = true)
            )
          }
        }
      }

      "redirect back to the CYA when the same duties has been selected" in forAll(completeJourneyGen) { journey =>
        whenever(journey.answers.securitiesReclaims.nonEmpty) {
          journey.getSelectedDepositIds.foreach { securityId =>
            val selectedDuties: Seq[TaxCode] =
              journey.getSelectedDutiesFor(securityId).get

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(journey))
            }

            checkIsRedirect(
              performAction(
                securityId,
                selectedDuties.map(taxCode => "select-duties[]" -> taxCode.value)
              ),
              routes.CheckYourAnswersController.show()
            )
          }
        }
      }

      "redirect back to the check claim page when duty has been de-selected" in forAll(completeJourneyGen) { journey =>
        journey.getSelectedDepositIds.foreach { securityId =>
          val selectedDuties: Seq[TaxCode] =
            journey.getSelectedDutiesFor(securityId).get

          whenever(selectedDuties.size > 1) {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData(journey))
              mockStoreSession(Right(()))
            }

            checkIsRedirect(
              performAction(
                securityId,
                selectedDuties.halfNonEmpty.map(taxCode => "select-duties[]" -> taxCode.value)
              ),
              routes.CheckClaimDetailsController.show()
            )
          }
        }

      }

      "redirect to the check claim page when new duty has been selected" in forAll(completeJourneyGen) { journey =>
        whenever(journey.answers.securitiesReclaims.nonEmpty) {
          journey.getSelectedDepositIds.foreach { securityId =>
            val availableDuties: Set[TaxCode] =
              journey.getSecurityTaxCodesFor(securityId).toSet

            val selectedDuties: Set[TaxCode] =
              journey.getSelectedDutiesFor(securityId).get.toSet

            (availableDuties -- selectedDuties).foreach { taxCode =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(SessionData(journey))
                mockStoreSession(Right(()))
              }

              checkIsRedirect(
                performAction(
                  securityId,
                  (selectedDuties + taxCode).toSeq.map(tc => "select-duties[]" -> tc.value)
                ),
                routes.CheckClaimDetailsController.show()
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
      val selected: List[TaxCode] = journey.answers.securitiesReclaims
        .map(_(securityId))
        .map(_.keys)
        .toList
        .flatten
      if (choices.size > selected.size) Some(securityId) else None
    }
  }

  val partialGen: Gen[SecuritiesJourney] = buildCompleteJourneyGen(
    acc14DeclarantMatchesUserEori = true,
    acc14ConsigneeMatchesUserEori = false,
    allDutiesGuaranteeEligibleOpt = None,
    hasConsigneeDetailsInACC14 = true,
    submitConsigneeDetails = false,
    submitContactDetails = false,
    submitContactAddress = false,
    submitBankAccountDetails = false,
    submitBankAccountType = false
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
