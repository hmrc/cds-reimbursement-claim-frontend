package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{DisplayDeclaration, NdrcDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Feature, SessionData, TaxCode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import scala.concurrent.Future

class EnterClaimControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with RejectedGoodsSingleJourneyTestData {

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

  val session = SessionData.empty.copy(
    rejectedGoodsSingleJourney = Some(RejectedGoodsSingleJourney.empty(exampleEori))
  )

  "Enter Claim Controller" when {
    "Enter Claim page" must {

      def performAction(): Future[Result] =
        controller.show()(FakeRequest())

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "display the page on a new journey" in forAll { (ndrcDetails: NdrcDetails, displayDeclaration: DisplayDeclaration) =>
        val drd = displayDeclaration.displayResponseDetail.copy(ndrcDetails = Some(List(ndrcDetails)))
        val updatedDd = displayDeclaration.copy(displayResponseDetail = drd)
        val taxCode = TaxCode(ndrcDetails.taxType)
        val taxCodeDescription = messageFromMessageKey(s"select-duties.duty.${ndrcDetails.taxType}")
        val amountPaid = BigDecimal(ndrcDetails.amount)
        val journey = RejectedGoodsSingleJourney.empty(exampleEori).submitDisplayDeclaration(updatedDd).selectAndReplaceTaxCodeSetForReimbursement(Seq(taxCode)).right.get
        val session = SessionData.empty.copy(rejectedGoodsSingleJourney = Some(journey))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("enter-claim.rejected-goods.single.title", ndrcDetails.taxType, taxCodeDescription),
          doc => {
            doc
              .select("p.govuk-inset-text")
              .text()                                                shouldBe messageFromMessageKey("enter-claim.rejected-goods.single.inset-text")
            doc.select("form p").text()                              shouldBe messageFromMessageKey(
              "enter-claim.rejected-goods.single.paid-amount-label",
              amountPaid.toPoundSterlingString
            )
            doc.select("#enter-claim.rejected-goods.single").`val`() shouldBe ""
            doc.select("form").attr("action")                        shouldBe routes.EnterClaimController.submit().url
          }
        )
      }
    }

    "Submit Enter Claim  page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "do not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

      "reject an empty Claim Amount" in {
//        inSequence {
//          mockAuthWithNoRetrievals()
//          mockGetSession(session)
//        }

        checkPageIsDisplayed(
          performAction("enter-claim.rejected-goods.single.claim-amount" -> ""),
          messageFromMessageKey("enter-claim.rejected-goods.single.title", "A00", "Customs Duty"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-claim.rejected-goods.single.claim-amount.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject a Claim Amount of 0" in {
        //        inSequence {
        //          mockAuthWithNoRetrievals()
        //          mockGetSession(session)
        //        }

        checkPageIsDisplayed(
          performAction("enter-claim.rejected-goods.single.claim-amount" -> "0"),
          messageFromMessageKey("enter-claim.rejected-goods.single.title", "A00", "Customs Duty"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-claim.rejected-goods.single.claim-amount.error.zero"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject a Claim Amount that is higher than the amount paid" in {
        //        inSequence {
        //          mockAuthWithNoRetrievals()
        //          mockGetSession(session)
        //        }

        checkPageIsDisplayed(
          performAction("enter-claim.rejected-goods.single.claim-amount" -> "1111"),
          messageFromMessageKey("enter-claim.rejected-goods.single.title", "A00", "Customs Duty"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-claim.rejected-goods.single.claim-amount.error.invalid-amount"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "reject a Claim Amount that contains invalid characters" in {
        //        inSequence {
        //          mockAuthWithNoRetrievals()
        //          mockGetSession(session)
        //        }

        checkPageIsDisplayed(
          performAction("enter-claim.rejected-goods.single.claim-amount" -> "invalid"),
          messageFromMessageKey("enter-claim.rejected-goods.single.title", "A00", "Customs Duty"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              "enter-claim.rejected-goods.single.claim-amount.error.invalid-text"
            ),
          expectedStatus = BAD_REQUEST
        )
      }
    }
  }
}
