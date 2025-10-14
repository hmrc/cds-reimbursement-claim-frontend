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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesClaimTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims.SecuritiesSingleClaimGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.SecurityDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.TaxDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SecuritiesClaimModes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SummaryInspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.SummaryMatchers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers.SelectDutiesSummary

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.collection.immutable.SortedMap

class SelectDutiesControllerSpec
    extends PropertyBasedControllerSpec
    with SecuritiesClaimTestData
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

  val session: SessionData = SessionData(SecuritiesClaim.empty(exampleEori).submitMovementReferenceNumber(exampleMrn))

  def validateSelectDutiesPage(
    securityId: String,
    singleSecurity: Boolean,
    doc: Document,
    claim: SecuritiesClaim,
    isError: Boolean = false
  ) = {
    val title       = doc.select("title").first().text()
    val caption     = doc.select("span.govuk-caption-l").eachText().asScala.toList
    val formHeading = doc.select(".govuk-heading-l").eachText().asScala.toList

    val dutiesAvailable: Seq[TaxCode] =
      claim.getSecurityTaxCodesFor(securityId)

    val taxDetails: Seq[TaxDetails] =
      dutiesAvailable.flatMap(claim.getSecurityTaxDetailsFor(securityId, _).toList)

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

      "display the page on a claim that has ACC14 tax codes" in
        forAll(completeClaimWithoutIPROrENUGen) { claim =>
          val updatedSession = SessionData.empty.copy(securitiesClaim = Some(claim))
          securityIdWithTaxCodes(claim).fold(
            (status(performAction("anySecurityId")) shouldBe NOT_FOUND): Any
          ) { securityId =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(updatedSession)
            }

            checkPageIsDisplayed(
              performAction(securityId),
              messageFromMessageKey(s"$messagesKey.securities.title"),
              doc => validateSelectDutiesPage(securityId, claim.isSingleSecurity, doc, claim)
            )
          }
        }

      "display the page on a claim with a single security deposit" in
        forAll(buildCompleteClaimGen(numberOfSecurityDetails = Some(1))) { claim =>
          val updatedSession = SessionData.empty.copy(securitiesClaim = Some(claim))
          securityIdWithTaxCodes(claim).fold(
            (status(performAction("anySecurityId")) shouldBe NOT_FOUND): Any
          ) { securityId =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(updatedSession)
            }

            checkPageIsDisplayed(
              performAction(securityId),
              messageFromMessageKey(s"$messagesKey.securities.title"),
              doc => validateSelectDutiesPage(securityId, claim.isSingleSecurity, doc, claim)
            )
          }
        }

      "redirect to ineligible page when there are no available duties" in {
        val claim                        = completeClaimWithoutIPROrENUGen.sample.getOrElse(fail("Failed to create claim"))
        val securityId                   = claim.getSecurityDepositIds.head
        val importDeclaration            = claim.answers.importDeclaration.get
        val securityDetails              = importDeclaration.getSecurityDetailsFor(securityId).get
        val updatedSecurityDetails       = securityDetails.copy(taxDetails = List.empty)
        val updatedDisplayResponseDetail = importDeclaration.displayResponseDetail.copy(securityDetails =
          Some(
            claim.getSecurityDetails
              .map(sd => updatedSecurityDetails)
              .filter(sd => sd.securityDepositId == securityId)
              .toList
          )
        )
        val updatedImportDeclaration     = importDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetail)
        val updatedClaim                 = SecuritiesClaim.unsafeModifyAnswers(
          claim,
          answers =>
            answers.copy(
              importDeclaration = Some(updatedImportDeclaration),
              modes = answers.modes.copy(checkYourAnswersChangeMode = false)
            )
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedClaim))
        }

        checkIsRedirect(performAction(securityId), baseRoutes.IneligibleController.ineligible)
      }
    }

    "show page for first duty is called" must {
      def performActionShowFirst(): Future[Result] = controller.showFirst()(FakeRequest())

      "select duty and redirect to enter claim page on a claim with a single duty type" in
        forAll(partialGenSingleDuty) { claim =>
          val securityId = securityIdWithTaxCodes(claim).get
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
            mockStoreSession(Right(()))
          }

          checkIsRedirect(
            performActionShowFirst(),
            routes.EnterClaimController.showFirst(securityId)
          )
        }

      "display the select duties page when there are multiple available duties" in
        forAll(completeClaimWithoutIPROrENUGen) { claim =>
          val securityId = securityIdWithTaxCodes(claim).get
          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(claim))
          }

          checkPageIsDisplayed(
            performActionShowFirst(),
            messageFromMessageKey(s"$messagesKey.securities.title"),
            doc => validateSelectDutiesPage(securityId, claim.isSingleSecurity, doc, claim)
          )
        }

      "redirect to ineligible page when there are no available duties" in {
        val claim                        = completeClaimWithoutIPROrENUGen.sample.getOrElse(fail("Failed to create claim"))
        val securityId                   = claim.getSecurityDepositIds.head
        val importDeclaration            = claim.answers.importDeclaration.get
        val securityDetails              = importDeclaration.getSecurityDetailsFor(securityId).get
        val updatedSecurityDetails       = securityDetails.copy(taxDetails = List.empty)
        val updatedDisplayResponseDetail = importDeclaration.displayResponseDetail.copy(securityDetails =
          Some(
            claim.getSecurityDetails
              .map(sd => updatedSecurityDetails)
              .filter(sd => sd.securityDepositId == securityId)
              .toList
          )
        )
        val updatedImportDeclaration     = importDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetail)
        val updatedClaim                 = SecuritiesClaim.unsafeModifyAnswers(
          claim,
          answers =>
            answers.copy(
              importDeclaration = Some(updatedImportDeclaration),
              modes = answers.modes.copy(checkYourAnswersChangeMode = false)
            )
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedClaim))
        }

        checkIsRedirect(performActionShowFirst(), baseRoutes.IneligibleController.ineligible)
      }

      "redirect to ineligible page when security deposit ID is not found" in {
        val claim                        = completeClaimWithoutIPROrENUGen.sample.getOrElse(fail("Failed to create claim"))
        val importDeclaration            = claim.answers.importDeclaration.get
        val updatedDisplayResponseDetail = importDeclaration.displayResponseDetail.copy(securityDetails = None)
        val updatedImportDeclaration     = importDeclaration.copy(displayResponseDetail = updatedDisplayResponseDetail)
        val updatedClaim                 = SecuritiesClaim.unsafeModifyAnswers(
          claim,
          answers =>
            answers.copy(
              importDeclaration = Some(updatedImportDeclaration),
              modes = answers.modes.copy(checkYourAnswersChangeMode = false)
            )
        )

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(updatedClaim))
        }

        checkIsRedirect(performActionShowFirst(), baseRoutes.IneligibleController.ineligible)
      }
    }

    "submit page is called" must {
      def performAction(securityId: String, data: Seq[(String, String)]): Future[Result] =
        controller.submit(securityId)(FakeRequest().withFormUrlEncodedBody(data*))

      "redirect to ineligible page when there are no duties returned from ACC14" in
        forAll(genReasonForSecurity) { rfs =>
          val claim          = securitiesClaimWithMrnAndRfsAndDeclaration(rfs)
          val updatedSession = SessionData.empty.copy(securitiesClaim = Some(claim))
          securityIdWithTaxCodes(claim).fold {
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
        forAll(partialGen) { claim =>
          val securityIdOpt = securityIdWithMoreChoicesThanThoseSelected(claim)
          whenever(claim.answers.correctedAmounts.nonEmpty && securityIdOpt.isDefined) {
            val securityId                                    = securityIdOpt.get
            val availableTaxCodes: List[TaxCode]              = claim.getSecurityTaxCodesFor(securityId).toList
            val previouslySelectedTaxCodes: List[TaxCode]     =
              claim.answers.correctedAmounts
                .map(_(securityId))
                .toList
                .flatMap(_.keys)
            val checkBoxesToSelect                            =
              selectCheckBoxes(
                claim,
                securityId,
                availableTaxCodes.zipWithIndex,
                previouslySelectedTaxCodes.zipWithIndex
              )
            val updatedSecuritiesReclaims                     =
              checkBoxesToSelect
                .map(_._2)
                .map(TaxCode(_))
                .sorted
            val updatedClaim: Either[String, SecuritiesClaim] =
              claim.selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(securityId, updatedSecuritiesReclaims)

            updatedClaim.fold(
              error => {
                logger.warn(s"unable to update claim with new tax codes $error")
                assert(false)
              },
              updatedClaim => {
                val updatedSession =
                  SessionData.empty.copy(securitiesClaim = Some(updatedClaim))
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

      "select and redirect to enter claim when not in check claim or confirm full amount modes" in {
        val initialClaim = completeClaimGen.sample.get
        val securityId   = securityIdWithTaxCodes(initialClaim).getOrElse(fail("Failed to get security ID"))
        val nextTaxCode  = initialClaim.getSecurityTaxCodesWithAmounts(securityId).head.taxCode
        val claim        = SecuritiesClaim.unsafeModifyAnswers(
          initialClaim,
          _.copy(
            correctedAmounts = Some(SortedMap(securityId -> SortedMap(nextTaxCode -> None))),
            modes = SecuritiesClaimModes(
              checkYourAnswersChangeMode = false,
              checkClaimDetailsChangeMode = false,
              claimFullAmountMode = false
            )
          )
        )

        val updatedClaim = claim
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(securityId, Seq(nextTaxCode))
          .getOrElse(fail("Failed to select duties"))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(SessionData(updatedClaim))(Right(()))
        }

        checkIsRedirect(
          performAction(securityId, Seq("select-duties[]" -> nextTaxCode.value)),
          routes.EnterClaimController.showFirst(securityId)
        )
      }

      "select and redirect to confirm full repayment when check claim and claim full amount modes are true and reclaims is empty for the deposit ID" in {
        val initialClaim = completeClaimGen.sample.get
        val securityId   = securityIdWithTaxCodes(initialClaim).getOrElse(fail("Failed to get security ID"))
        val nextTaxCode  = initialClaim.getSecurityTaxCodesWithAmounts(securityId).head.taxCode
        val claim        = SecuritiesClaim.unsafeModifyAnswers(
          initialClaim,
          _.copy(
            correctedAmounts = Some(SortedMap(securityId -> SortedMap.empty[TaxCode, Option[BigDecimal]])),
            modes = SecuritiesClaimModes(
              checkYourAnswersChangeMode = false,
              checkClaimDetailsChangeMode = true,
              claimFullAmountMode = true
            )
          )
        )

        val updatedClaim = claim
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(securityId, Seq(nextTaxCode))
          .getOrElse(fail("Failed to select duties"))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(SessionData(updatedClaim))(Right(()))
        }

        checkIsRedirect(
          performAction(securityId, Seq("select-duties[]" -> nextTaxCode.value)),
          routes.ConfirmFullRepaymentController.show(securityId)
        )
      }

      "select and redirect to confirm full repayment when check claim and claim full amount modes are true and reclaims is empty for for single security deposit ID" in {
        val initialClaim = SecuritiesSingleClaimGenerators.completeClaimGen.sample.get
        val securityId   = securityIdWithTaxCodes(initialClaim).getOrElse(fail("Failed to get security ID"))
        val nextTaxCode  = initialClaim.getSecurityTaxCodesWithAmounts(securityId).head.taxCode
        val claim        = SecuritiesClaim.unsafeModifyAnswers(
          initialClaim,
          _.copy(
            correctedAmounts = Some(SortedMap(securityId -> SortedMap.empty[TaxCode, Option[BigDecimal]])),
            modes = SecuritiesClaimModes(
              checkYourAnswersChangeMode = false,
              checkClaimDetailsChangeMode = true,
              claimFullAmountMode = true
            )
          )
        )

        val updatedClaim = claim
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(securityId, Seq(nextTaxCode))
          .getOrElse(fail("Failed to select duties"))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(SessionData(updatedClaim))(Right(()))
        }

        checkIsRedirect(
          performAction(securityId, Seq("select-duties[]" -> nextTaxCode.value)),
          routes.ConfirmSingleDepositRepaymentController.show
        )
      }

      "select and redirect to check claim when check claim and claim full amount modes are true and the duty selected is the last incomplete claim" in {
        val initialClaim       = SecuritiesClaimGenerators
          .buildCompleteClaimGen(numberOfDutyTypes = Some(2))
          .sample
          .getOrElse(fail("Failed to create claim"))
        val securityId         = initialClaim.getSecurityDepositIds.head
        val taxDetails         = initialClaim.getSecurityDetailsFor(securityId).get.taxDetails
        val nextTaxCode        = taxDetails.head.getTaxCode
        val taxCodeWithAmounts =
          taxDetails.map(td => td.getTaxCode -> Some(td.getAmount)).filter((taxCode, _) => taxCode != nextTaxCode)
        val correctedAmounts   = SortedMap(securityId -> SortedMap(taxCodeWithAmounts*))

        val claim = SecuritiesClaim.unsafeModifyAnswers(
          initialClaim,
          _.copy(
            correctedAmounts = Some(correctedAmounts),
            modes = SecuritiesClaimModes(
              checkYourAnswersChangeMode = false,
              checkClaimDetailsChangeMode = true,
              claimFullAmountMode = true
            )
          )
        )

        val updatedClaim = claim
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(securityId, Seq(nextTaxCode))
          .getOrElse(fail("Failed to select duties"))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(SessionData(updatedClaim))(Right(()))
        }

        checkIsRedirect(
          performAction(securityId, Seq("select-duties[]" -> nextTaxCode.value)),
          routes.CheckClaimDetailsController.show
        )
      }

      "select and redirect to check claim when check claim and claim full amount modes are true and the duty selected is the last incomplete claim for a single security deposit" in {
        val initialClaim       = SecuritiesSingleClaimGenerators
          .buildCompleteClaimGen()
          .sample
          .getOrElse(fail("Failed to create claim"))
        val securityId         = initialClaim.getSecurityDepositIds.head
        val taxDetails         = initialClaim.getSecurityDetailsFor(securityId).get.taxDetails
        val nextTaxCode        = taxDetails.head.getTaxCode
        val taxCodeWithAmounts =
          taxDetails.map(td => td.getTaxCode -> Some(td.getAmount)).filter((taxCode, _) => taxCode != nextTaxCode)
        val correctedAmounts   = SortedMap(securityId -> SortedMap(taxCodeWithAmounts*))

        val claim = SecuritiesClaim.unsafeModifyAnswers(
          initialClaim,
          _.copy(
            correctedAmounts = Some(correctedAmounts),
            modes = SecuritiesClaimModes(
              checkYourAnswersChangeMode = false,
              checkClaimDetailsChangeMode = true,
              claimFullAmountMode = true
            )
          )
        )

        val updatedClaim = claim
          .selectAndReplaceTaxCodeSetForSelectedSecurityDepositId(securityId, Seq(nextTaxCode))
          .getOrElse(fail("Failed to select duties"))

        inSequence {
          mockAuthWithDefaultRetrievals()
          mockGetSession(SessionData(claim))
          mockStoreSession(SessionData(updatedClaim))(Right(()))
        }

        checkIsRedirect(
          performAction(securityId, Seq("select-duties[]" -> nextTaxCode.value)),
          routes.CheckClaimDetailsSingleSecurityController.show
        )
      }

      "redisplay the page with an error when no checkboxes are selected" in forAll(completeClaimWithoutIPROrENUGen) {
        claim =>
          whenever(claim.answers.correctedAmounts.nonEmpty) {
            securityIdWithTaxCodes(claim).fold(
              throw new Throwable("unexpectedly found securities reclaims already populated")
            ) { securityId =>
              val updatedSession = SessionData.empty.copy(securitiesClaim = Some(claim))
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
                    claim.isSingleSecurity,
                    doc = doc,
                    claim = claim,
                    isError = true
                  )
              )
            }
          }
      }

      "redirect back to the CYA when the same duties has been selected" in forAll(completeClaimWithoutIPROrENUGen) {
        claim =>
          whenever(claim.answers.correctedAmounts.nonEmpty) {
            claim.getSelectedDepositIds.foreach { securityId =>
              val selectedDuties: Seq[TaxCode] =
                claim.getSelectedDutiesFor(securityId).get

              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(claim))
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
        completeClaimWithoutIPROrENUGen
      ) { claim =>
        claim.getSelectedDepositIds.foreach { securityId =>
          val selectedDuties: Seq[TaxCode] =
            claim.getSelectedDutiesFor(securityId).get

          whenever(selectedDuties.size > 1) {
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(claim))
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

      "redirect to the check claim page when new duty has been selected" in forAll(completeClaimWithoutIPROrENUGen) {
        claim =>
          whenever(claim.answers.correctedAmounts.nonEmpty) {
            claim.getSelectedDepositIds.foreach { securityId =>
              val availableDuties: Set[TaxCode] =
                claim.getSecurityTaxCodesFor(securityId).toSet

              val selectedDuties: Set[TaxCode] =
                claim.getSelectedDutiesFor(securityId).get.toSet

              (availableDuties -- selectedDuties).foreach { taxCode =>
                inSequence {
                  mockAuthWithDefaultRetrievals()
                  mockGetSession(SessionData(claim))
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
  def securityIdWithTaxCodes(claim: SecuritiesClaim): Option[String]                                       =
    claim.getLeadImportDeclaration
      .map { importDeclaration =>
        importDeclaration.displayResponseDetail.securityDetails
      }
      .flatMap(getFirstSecurityWithTaxCodes)

  def securityIdWithMoreChoicesThanThoseSelected(claim: SecuritiesClaim): Option[String] = {
    val securityId = securityIdWithTaxCodes(claim)
    securityId.fold(Option.empty[String]) { securityId =>
      val choices: List[TaxCode]  = claim.getSecurityTaxCodesFor(securityId).toList
      val selected: List[TaxCode] = claim.answers.correctedAmounts
        .map(_(securityId))
        .map(_.keys)
        .toList
        .flatten
      if choices.size > selected.size then Some(securityId) else None
    }
  }

  val partialGen: Gen[SecuritiesClaim] =
    buildCompleteClaimGen(
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

  val partialGenSingleDuty: Gen[SecuritiesClaim] =
    buildCompleteClaimGen(
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
    claim: SecuritiesClaim,
    securityId: String,
    availableTaxCodes: Seq[(TaxCode, Int)],
    previouslySelected: Seq[(TaxCode, Int)]
  ): Seq[(String, String)] =
    (previouslySelected.size, availableTaxCodes.size) match {
      case (0, _)          =>
        Seq("select-duties[0]" -> claim.getSecurityTaxCodesFor(securityId).toList.head.value)
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
          "cannot test complete claim when all securities are selected or invalid securities are selected"
        )
    }
}
