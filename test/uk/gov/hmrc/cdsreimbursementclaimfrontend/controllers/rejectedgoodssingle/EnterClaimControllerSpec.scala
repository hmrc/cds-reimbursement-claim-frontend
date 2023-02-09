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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import cats.implicits.catsSyntaxEq
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

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

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  def sessionWithNdrcDetails(ndrcDetails: List[NdrcDetails], displayDeclaration: DisplayDeclaration): SessionData = {
    val drd       = displayDeclaration.displayResponseDetail.copy(ndrcDetails = Some(ndrcDetails))
    val updatedDd = displayDeclaration.copy(displayResponseDetail = drd)
    val taxCode   = ndrcDetails.map(details => TaxCode(details.taxType))
    val journey   = RejectedGoodsSingleJourney
      .empty(displayDeclaration.getDeclarantEori)
      .submitMovementReferenceNumberAndDeclaration(updatedDd.getMRN, updatedDd)
      .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(taxCode))
      .getOrFail
    SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))
  }

  "Enter Claim Controller" when {
    "Enter Claim page" must {

      def performAction(taxCode: Option[TaxCode] = None): Future[Result] =
        taxCode.fold(controller.showFirst)(controller.show(_))(FakeRequest())

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "redirect to the first enter claim page on a new journey" in forAll {
        (ndrcDetails: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
          val session = sessionWithNdrcDetails(List(ndrcDetails), displayDeclaration)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          val taxCode: TaxCode = TaxCode(ndrcDetails.taxType)

          val result = performAction()

          checkIsRedirect(
            result,
            routes.EnterClaimController.show(taxCode)
          )
      }

      "display the enter claim page on a new journey" in forAll {
        (ndrcDetails: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
          val taxCodeDescription = messageFromMessageKey(s"select-duties.duty.${ndrcDetails.taxType}")
          val amountPaid         = BigDecimal(ndrcDetails.amount)
          val session            = sessionWithNdrcDetails(List(ndrcDetails), displayDeclaration)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          val taxCode: TaxCode = TaxCode(ndrcDetails.taxType)

          val result = performAction(Some(taxCode))

          checkPageIsDisplayed(
            result,
            messageFromMessageKey("enter-claim.rejected-goods.single.title", ndrcDetails.taxType, taxCodeDescription),
            doc => {
              doc
                .select("p.govuk-inset-text")
                .html()                                                                   shouldBe messageFromMessageKey("enter-claim.rejected-goods.inset-text")
              doc.select("#amount-paid").text()                                           shouldBe amountPaid.toPoundSterlingString
              doc.select("input[name='enter-claim.rejected-goods.claim-amount']").`val`() shouldBe ""
              doc.select("form").attr("action")                                           shouldBe routes.EnterClaimController.submit(taxCode).url
            }
          )
      }

      "display the page when trying to amend a specific tax code" in forAll {
        (ndrcDetails: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
          whenever(BigDecimal(ndrcDetails.amount) > 12) {
            val drd                = displayDeclaration.displayResponseDetail.copy(ndrcDetails = Some(List(ndrcDetails)))
            val updatedDd          = displayDeclaration.copy(displayResponseDetail = drd)
            val taxCode            = TaxCode(ndrcDetails.taxType)
            val taxCodeDescription = messageFromMessageKey(s"select-duties.duty.${ndrcDetails.taxType}")
            val amountPaid         = BigDecimal(ndrcDetails.amount)
            val amountClaimed      = BigDecimal(ndrcDetails.amount) - 10
            val journey            = RejectedGoodsSingleJourney
              .empty(displayDeclaration.getDeclarantEori)
              .submitMovementReferenceNumberAndDeclaration(updatedDd.getMRN, updatedDd)
              .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(List(taxCode)))
              .flatMap(_.submitAmountForReimbursement(taxCode, amountClaimed))
              .getOrFail
            val session            = SessionData(journey)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            val result = performAction(Some(taxCode))

            checkPageIsDisplayed(
              result,
              messageFromMessageKey("enter-claim.rejected-goods.single.title", ndrcDetails.taxType, taxCodeDescription),
              doc => {
                doc
                  .select("p.govuk-inset-text")
                  .html()                         shouldBe messageFromMessageKey("enter-claim.rejected-goods.inset-text")
                doc.select("#amount-paid").text() shouldBe amountPaid.toPoundSterlingString
                doc
                  .select("input[name='enter-claim.rejected-goods.claim-amount']")
                  .`val`()                        shouldBe f"$amountClaimed%1.2f"
                doc.select("form").attr("action") shouldBe routes.EnterClaimController.submit(taxCode).url
              }
            )
          }
      }

      "redirect to the select tax codes page if none have been specified" in forAll {
        (displayDeclaration: DisplayDeclaration) =>
          val journey = RejectedGoodsSingleJourney
            .empty(displayDeclaration.getDeclarantEori)
            .submitMovementReferenceNumberAndDeclaration(displayDeclaration.getMRN, displayDeclaration)
            .getOrFail
          val session = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(),
            routes.SelectDutiesController.show()
          )
      }
    }

    "Submit Enter Claim  page" must {

      def performAction(taxCode: String, data: (String, String)*): Future[Result] =
        controller.submit(TaxCode(taxCode))(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction("A95")) shouldBe NOT_FOUND
      }

      "reject an empty Claim Amount" in forAll { (ndrcDetails: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
        val taxCodeDescription = messageFromMessageKey(s"select-duties.duty.${ndrcDetails.taxType}")
        val session            = sessionWithNdrcDetails(List(ndrcDetails), displayDeclaration)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(ndrcDetails.taxType, "enter-claim.rejected-goods.claim-amount" -> ""),
          messageFromMessageKey("enter-claim.rejected-goods.single.title", ndrcDetails.taxType, taxCodeDescription),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-claim.rejected-goods.claim-amount.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject a Claim Amount of 0" in forAll { (ndrcDetails: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
        val taxCodeDescription = messageFromMessageKey(s"select-duties.duty.${ndrcDetails.taxType}")
        val session            = sessionWithNdrcDetails(List(ndrcDetails), displayDeclaration)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(ndrcDetails.taxType, "enter-claim.rejected-goods.claim-amount" -> "0"),
          messageFromMessageKey("enter-claim.rejected-goods.single.title", ndrcDetails.taxType, taxCodeDescription),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-claim.rejected-goods.claim-amount.error.zero"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject a Claim Amount that is higher than the amount paid" in forAll {
        (ndrcDetails: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
          val taxCodeDescription = messageFromMessageKey(s"select-duties.duty.${ndrcDetails.taxType}")
          val amountToClaim      = BigDecimal(ndrcDetails.amount) + 10
          val session            = sessionWithNdrcDetails(List(ndrcDetails), displayDeclaration)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(
              ndrcDetails.taxType,
              "enter-claim.rejected-goods.claim-amount" -> amountToClaim.toString()
            ),
            messageFromMessageKey("enter-claim.rejected-goods.single.title", ndrcDetails.taxType, taxCodeDescription),
            doc =>
              getErrorSummary(doc) shouldBe messageFromMessageKey(
                "enter-claim.rejected-goods.claim-amount.error.invalid-amount"
              ),
            expectedStatus = BAD_REQUEST
          )
      }

      "reject a Claim Amount that contains invalid characters" in forAll {
        (ndrcDetails: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
          val taxCodeDescription = messageFromMessageKey(s"select-duties.duty.${ndrcDetails.taxType}")
          val session            = sessionWithNdrcDetails(List(ndrcDetails), displayDeclaration)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(ndrcDetails.taxType, "enter-claim.rejected-goods.claim-amount" -> "invalid"),
            messageFromMessageKey("enter-claim.rejected-goods.single.title", ndrcDetails.taxType, taxCodeDescription),
            doc =>
              getErrorSummary(doc) shouldBe messageFromMessageKey(
                "enter-claim.rejected-goods.claim-amount.error.invalid-text"
              ),
            expectedStatus = BAD_REQUEST
          )
      }

      "reject a Claim Amount when no tax codes selected and show the enter claim amount page" in forAll {
        (ndrcDetails1: NdrcDetails, ndrcDetails2: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
          whenever(ndrcDetails1.amount.toInt > 11 && ndrcDetails1.taxType =!= ndrcDetails2.taxType) {
            val drd           =
              displayDeclaration.displayResponseDetail.copy(ndrcDetails = Some(List(ndrcDetails1, ndrcDetails2)))
            val updatedDd     = displayDeclaration.copy(displayResponseDetail = drd)
            val journey       = RejectedGoodsSingleJourney
              .empty(displayDeclaration.getDeclarantEori)
              .submitMovementReferenceNumberAndDeclaration(updatedDd.getMRN, updatedDd)
              .getOrFail
            val session       = SessionData(journey)
            val amountToClaim = BigDecimal(ndrcDetails1.amount) - 10

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            checkIsRedirect(
              performAction(
                ndrcDetails1.taxType,
                "enter-claim.rejected-goods.claim-amount" -> amountToClaim.toString()
              ),
              routes.SelectDutiesController.show()
            )
          }
      }

      "reject a Claim Amount when mrn has been selected and show the enter mrn page" in forAll {
        (ndrcDetails: NdrcDetails) =>
          whenever(ndrcDetails.amount.toInt > 11) {
            val journey       = RejectedGoodsSingleJourney
              .empty(exampleEori)
            val session       = SessionData(journey)
            val amountToClaim = BigDecimal(ndrcDetails.amount) - 10

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            val result = performAction(
              ndrcDetails.taxType,
              "enter-claim.rejected-goods.claim-amount" -> amountToClaim.toString()
            )

            checkIsRedirect(
              result,
              routes.EnterMovementReferenceNumberController.show()
            )

          }
      }

      "accept claim amount when only a single claim is present and move on to total reimbursement page" in forAll {
        (ndrcDetails: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
          whenever(ndrcDetails.amount.toInt > 11) {
            val session        = sessionWithNdrcDetails(List(ndrcDetails), displayDeclaration)
            val journey        = session.rejectedGoodsSingleJourney.getOrElse(fail("No journey present"))
            val amountToClaim  = BigDecimal(ndrcDetails.amount) - 10
            val updatedJourney =
              journey.submitAmountForReimbursement(TaxCode(ndrcDetails.taxType), amountToClaim).getOrFail
            val updatedSession = session.copy(rejectedGoodsSingleJourney = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(updatedSession)(Right(()))
            }

            val result = performAction(
              ndrcDetails.taxType,
              "enter-claim.rejected-goods.claim-amount" -> amountToClaim.toString()
            )

            checkIsRedirect(
              result,
              routes.CheckClaimDetailsController.show()
            )

          }
      }

      "accept claim amount, for the full amount, when only a single claim is present and move on to total reimbursement page" in forAll {
        (ndrcDetails: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
          whenever(ndrcDetails.amount.toInt > 11) {
            val session        = sessionWithNdrcDetails(List(ndrcDetails), displayDeclaration)
            val journey        = session.rejectedGoodsSingleJourney.getOrElse(fail("No journey present"))
            val amountToClaim  = BigDecimal(ndrcDetails.amount)
            val updatedJourney =
              journey.submitAmountForReimbursement(TaxCode(ndrcDetails.taxType), amountToClaim).getOrFail
            val updatedSession = session.copy(rejectedGoodsSingleJourney = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(updatedSession)(Right(()))
            }

            val result = performAction(
              ndrcDetails.taxType,
              "enter-claim.rejected-goods.claim-amount" -> amountToClaim.toString()
            )

            checkIsRedirect(
              result,
              routes.CheckClaimDetailsController.show()
            )

          }
      }

      "accept claim amount when multiple claim is present and not all have been claimed" in forAll {
        (ndrcDetails1: NdrcDetails, ndrcDetails2: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
          whenever(ndrcDetails1.amount.toInt > 11 && ndrcDetails1.taxType =!= ndrcDetails2.taxType) {
            val session        = sessionWithNdrcDetails(List(ndrcDetails1, ndrcDetails2), displayDeclaration)
            val journey        = session.rejectedGoodsSingleJourney.getOrElse(fail("No journey present"))
            val amountToClaim  = BigDecimal(ndrcDetails1.amount) - 10
            val updatedJourney =
              journey.submitAmountForReimbursement(TaxCode(ndrcDetails1.taxType), amountToClaim).getOrFail
            val updatedSession = session.copy(rejectedGoodsSingleJourney = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(updatedSession)(Right(()))
            }

            val result = performAction(
              ndrcDetails1.taxType,
              "enter-claim.rejected-goods.claim-amount" -> amountToClaim.toString()
            )

            checkIsRedirect(
              result,
              routes.EnterClaimController.show(TaxCode(ndrcDetails2.taxType))
            )

          }
      }
    }
  }
}
