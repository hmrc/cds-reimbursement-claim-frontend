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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Call
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

import scala.concurrent.Future

class EnterClaimControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: EnterClaimController = instanceOf[EnterClaimController]
  val enterClaimKey: String            = "enter-claim.scheduled"

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val journeyGen: Gen[RejectedGoodsScheduledJourney] =
    buildJourneyFromAnswersGen(answersWithDutiesSelectedGen())

  "Enter Claim Controller" should {

    "Show enter claim page" must {

      "display the enter claim page" when {

        "the user has selected duty and tax codes for the first time" in forAll(journeyGen) { journey =>
          journey.getFirstDutyToClaim.map { case (dutyType: DutyType, taxCode: TaxCode) =>
            inSequence {
              mockAuthWithDefaultRetrievals()
              mockGetSession(SessionData(journey))
            }

            checkIsRedirect(
              controller.showFirst()(FakeRequest()),
              routes.EnterClaimController.show(dutyType, taxCode)
            )
          }

        }

        "the user revisits enter claim page again" in forAll(completeJourneyGen) { journey =>
          val dutyType: DutyType      = journey.getReimbursementClaims.head._1
          val taxCode: TaxCode        = journey.getReimbursementClaimsFor(dutyType).get.head._1
          val reimbursement           = journey.getReimbursementClaimsFor(dutyType).get.head._2.get
          val paidAmount: BigDecimal  = reimbursement.paidAmount.setScale(2, BigDecimal.RoundingMode.HALF_UP)
          val claimAmount: BigDecimal = reimbursement.claimAmount.setScale(2, BigDecimal.RoundingMode.HALF_UP)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkPageIsDisplayed(
            controller.show(dutyType, taxCode)(FakeRequest()),
            if DutyTypes.custom.contains(dutyType) then
              messageFromMessageKey(
                s"enter-claim.scheduled.title",
                messages(s"select-duties.duty.$taxCode")
              )
            else
              messageFromMessageKey(
                s"enter-claim.scheduled.title.excise",
                messages(s"duty-type.${dutyType.repr}"),
                taxCode.value,
                messages(s"excise-category.${ExciseCategory.categoryOf(taxCode).repr}")
              )
            ,
            doc => {
              val elements = doc.select("input")
              BigDecimal(elements.get(0).`val`()) should be(paidAmount)
              BigDecimal(elements.get(1).`val`()) should be(claimAmount)
            }
          )
        }
      }

      "redirect to the select duty type page" when {

        "the user has not chosen duty type or tax code" in {

          val journey = buildJourneyFromAnswersGen(answersUpToBasisForClaimGen()).sample.get
          val session = SessionData(journey)

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            controller.showFirst()(FakeRequest()),
            routes.SelectDutyTypesController.show
          )
        }

        "the user has empty selected duties" in {
          val journey = journeyWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(UkDuty))
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(journey))
          }

          checkIsRedirect(
            controller.showFirst()(FakeRequest()),
            routes.SelectDutiesController.show(UkDuty)
          )
        }
      }
    }

    "Submit enter claim page" must {

      def performAction(dutyType: DutyType, taxCode: TaxCode, data: Seq[(String, String)]): Future[Result] =
        controller.submit(dutyType, taxCode)(FakeRequest().withFormUrlEncodedBody(data*))

      "save user defined amounts and ask user to enter next amounts for upcoming reimbursement" in {
        forAll(journeyGen, amountPaidWithClaimGen) { case (initialJourney, (paidAmount, claimAmount)) =>
          initialJourney.getSelectedDuties.foreachEntry { case (dutyType, taxCodes) =>
            taxCodes.take(taxCodes.length - 1).foreach { taxCode =>
              val updatedJourney = initialJourney
                .submitClaimAmount(
                  dutyType,
                  taxCode,
                  paidAmount,
                  claimAmount
                )
                .getOrFail

              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(initialJourney))
                mockStoreSession(SessionData(updatedJourney))(Right(()))
              }

              val expectedRoute: Call =
                updatedJourney.findNextSelectedTaxCodeAfter(dutyType, taxCode) match {
                  case Some((nextDutyType, nextTaxCode: TaxCode)) =>
                    routes.EnterClaimController.show(nextDutyType, nextTaxCode)

                  case _ =>
                    updatedJourney.findNextSelectedDutyAfter(dutyType) match {
                      case Some(nextDutyType) => routes.SelectDutiesController.show(nextDutyType)
                      case None               => routes.CheckClaimDetailsController.show
                    }

                }

              checkIsRedirect(
                performAction(
                  dutyType,
                  taxCode,
                  Seq(
                    "enter-claim.scheduled.paid-amount"  -> paidAmount.toString,
                    "enter-claim.scheduled.claim-amount" -> claimAmount.toString
                  )
                ),
                expectedRoute
              )
            }
          }
        }
      }

      "save user defined amounts and redirect to the check claim details page" in {
        forAll(completeJourneyGen, amountPaidWithClaimGen) { case (initialJourney, (paidAmount, claimAmount)) =>
          initialJourney.getSelectedDuties.foreachEntry { case (dutyType, taxCodes) =>
            taxCodes.foreach { taxCode =>
              val updatedJourney = initialJourney
                .submitClaimAmount(
                  dutyType,
                  taxCode,
                  paidAmount,
                  claimAmount
                )
                .getOrFail

              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(initialJourney))
                mockStoreSession(SessionData(updatedJourney))(Right(()))
              }

              val expectedRoute: Call =
                routes.CheckClaimDetailsController.show

              checkIsRedirect(
                performAction(
                  dutyType,
                  taxCode,
                  Seq(
                    "enter-claim.scheduled.paid-amount"  -> paidAmount.toString,
                    "enter-claim.scheduled.claim-amount" -> claimAmount.toString
                  )
                ),
                expectedRoute
              )
            }
          }
        }
      }

      "redirect to select duties for the next duty type" when {

        "claims are completed for the current duty type and more duties are selected in another duty type" in {
          val currentDutyType = UkDuty
          val taxCode = TaxCode("A00")

          val initialJourney = journeyWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(currentDutyType, EuDuty))
            .flatMap(_.selectAndReplaceTaxCodeSetForDutyType(currentDutyType, Seq(taxCode)))
            .flatMap(_.selectAndReplaceTaxCodeSetForDutyType(EuDuty, Seq(TaxCode("A50"))))
            .getOrFail

          val paidAmount = 24
          val claimAmount = 12

          val updatedJourney = initialJourney
            .submitClaimAmount(
              currentDutyType,
              taxCode,
              paidAmount,
              claimAmount
            )
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(updatedJourney))(Right(()))
          }

          checkIsRedirect(
            performAction(
              currentDutyType,
              taxCode,
              Seq(
                "enter-claim.scheduled.paid-amount" -> paidAmount.toString,
                "enter-claim.scheduled.claim-amount" -> claimAmount.toString
              )
            ),
            routes.SelectDutiesController.show(EuDuty)
          )
        }

        "claims are completed for the current duty type and no more duties are selected in another duty type" in {
          val currentDutyType = UkDuty
          val taxCode = TaxCode("A00")

          val initialJourney = journeyWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(currentDutyType, EuDuty))
            .flatMap(_.selectAndReplaceTaxCodeSetForDutyType(currentDutyType, Seq(taxCode)))
            .getOrFail

          val paidAmount = 24
          val claimAmount = 12

          val updatedJourney = initialJourney
            .submitClaimAmount(
              currentDutyType,
              taxCode,
              paidAmount,
              claimAmount
            )
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(updatedJourney))(Right(()))
          }

          checkIsRedirect(
            performAction(
              currentDutyType,
              taxCode,
              Seq(
                "enter-claim.scheduled.paid-amount" -> paidAmount.toString,
                "enter-claim.scheduled.claim-amount" -> claimAmount.toString
              )
            ),
            routes.SelectDutiesController.show(EuDuty)
          )
        }
      }

      "save user defined amounts and redirect to select excise duties" when {

        "the current duty is excise and there are more excise categories selected" in {
          val currentDutyType = Excise
          val currentExciseCategory = Beer
          val nextExciseCategory = Wine
          val currentTaxCode = TaxCode("311")

          val initialJourney = journeyWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(currentDutyType))
            .flatMap(_.selectAndReplaceExciseCodeCategories(Seq(currentExciseCategory, nextExciseCategory)))
            .flatMap(_.selectAndReplaceTaxCodeSetForExciseCategory(currentExciseCategory, Seq(currentTaxCode)))
            .getOrFail

          val paidAmount = 24
          val claimAmount = 12

          val updatedJourney = initialJourney
            .submitClaimAmount(
              currentDutyType,
              currentTaxCode,
              paidAmount,
              claimAmount
            )
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(updatedJourney))(Right(()))
          }

          checkIsRedirect(
            performAction(
              currentDutyType,
              currentTaxCode,
              Seq(
                "enter-claim.scheduled.paid-amount" -> paidAmount.toString,
                "enter-claim.scheduled.claim-amount" -> claimAmount.toString
              )
            ),
            routes.SelectDutiesController.showExciseDuties(nextExciseCategory)
          )
        }
      }

      "save user defined amounts and redirect to check claim details" when {

        "the current duty is excise and there are no more incomplete claims" in {
          val currentDutyType = Excise
          val currentExciseCategory = Beer
          val currentTaxCode = TaxCode("311")
          val paidAmount = 24
          val claimAmount = 12

          val initialJourney = journeyWithMrnAndDeclaration
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(currentDutyType))
            .flatMap(_.selectAndReplaceExciseCodeCategories(Seq(currentExciseCategory)))
            .flatMap(_.selectAndReplaceTaxCodeSetForExciseCategory(currentExciseCategory, Seq(currentTaxCode)))
            .getOrFail

          val updatedJourney = initialJourney
            .submitClaimAmount(
              currentDutyType,
              currentTaxCode,
              paidAmount,
              claimAmount
            )
            .getOrFail

          inSequence {
            mockAuthWithDefaultRetrievals()
            mockGetSession(SessionData(initialJourney))
            mockStoreSession(SessionData(updatedJourney))(Right(()))
          }

          checkIsRedirect(
            performAction(
              currentDutyType,
              currentTaxCode,
              Seq(
                "enter-claim.scheduled.paid-amount" -> paidAmount.toString,
                "enter-claim.scheduled.claim-amount" -> claimAmount.toString
              )
            ),
            routes.CheckClaimDetailsController.show
          )
        }
      }

      "show an error summary" when {
        "duty amounts are missing or invalid" in forAll(journeyGen) { journey =>
          journey.getSelectedDuties.foreachEntry { case (dutyType, taxCodes) =>
            taxCodes.foreach { taxCode =>
              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(journey))
              }

              checkPageIsDisplayed(
                controller.submit(dutyType, taxCode)(
                  FakeRequest().withFormUrlEncodedBody(
                    Seq(
                      "enter-claim.scheduled.paid-amount"  -> "",
                      "enter-claim.scheduled.claim-amount" -> "bad"
                    )*
                  )
                ),
                if DutyTypes.custom.contains(dutyType) then
                  messageFromMessageKey(
                    s"enter-claim.scheduled.title",
                    messages(s"select-duties.duty.$taxCode")
                  )
                else
                  messageFromMessageKey(
                    s"enter-claim.scheduled.title.excise",
                    messages(s"duty-type.${dutyType.repr}"),
                    taxCode.value,
                    messages(s"excise-category.${ExciseCategory.categoryOf(taxCode).repr}")
                  )
                ,
                doc => {
                  doc
                    .select(".govuk-error-summary__list > li:nth-child(1) > a")
                    .text() shouldBe messageFromMessageKey("enter-claim.scheduled.paid-amount.error.required")
                  doc
                    .select(".govuk-error-summary__list > li:nth-child(2) > a")
                    .text() shouldBe messageFromMessageKey("enter-claim.scheduled.claim-amount.error.invalid")
                },
                BAD_REQUEST
              )

              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(journey))
              }

              checkPageIsDisplayed(
                controller.submit(dutyType, taxCode)(
                  FakeRequest().withFormUrlEncodedBody(
                    Seq(
                      s"$enterClaimKey.paid-amount"  -> "0.00",
                      s"$enterClaimKey.claim-amount" -> "0.00"
                    )*
                  )
                ),
                if DutyTypes.custom.contains(dutyType) then
                  messageFromMessageKey(
                    s"enter-claim.scheduled.title",
                    messages(s"select-duties.duty.$taxCode")
                  )
                else
                  messageFromMessageKey(
                    s"enter-claim.scheduled.title.excise",
                    messages(s"duty-type.${dutyType.repr}"),
                    taxCode.value,
                    messages(s"excise-category.${ExciseCategory.categoryOf(taxCode).repr}")
                  )
                ,
                doc =>
                  doc
                    .select(".govuk-error-summary__list > li:nth-child(1) > a")
                    .text() shouldBe messageFromMessageKey(s"$enterClaimKey.paid-amount.error.zero"),
                BAD_REQUEST
              )

              inSequence {
                mockAuthWithDefaultRetrievals()
                mockGetSession(SessionData(journey))
              }

              checkPageIsDisplayed(
                controller.submit(dutyType, taxCode)(
                  FakeRequest().withFormUrlEncodedBody(
                    Seq(
                      s"$enterClaimKey.paid-amount"  -> "12.34",
                      s"$enterClaimKey.claim-amount" -> "12.35"
                    )*
                  )
                ),
                if DutyTypes.custom.contains(dutyType) then
                  messageFromMessageKey(
                    s"enter-claim.scheduled.title",
                    messages(s"select-duties.duty.$taxCode")
                  )
                else
                  messageFromMessageKey(
                    s"enter-claim.scheduled.title.excise",
                    messages(s"duty-type.${dutyType.repr}"),
                    taxCode.value,
                    messages(s"excise-category.${ExciseCategory.categoryOf(taxCode).repr}")
                  )
                ,
                doc =>
                  doc
                    .select(".govuk-error-summary__list > li:nth-child(1) > a")
                    .text() shouldBe messageFromMessageKey(s"$enterClaimKey.invalid.claim"),
                BAD_REQUEST
              )
            }
          }
        }
      }

    }
  }
}
