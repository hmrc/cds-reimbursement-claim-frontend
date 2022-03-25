package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.completeJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class SelectDutyCodesControllerSpec
  extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val controller: SelectDutyCodesController = instanceOf[SelectDutyCodesController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "select-duty-codes"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  "Select Duty Codes Controller" when {

    "Show select duty codes page" must {

      def performAction(): Future[Result] = controller.show()(FakeRequest())

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

//      "display the page for the first time" in {
//        val journey = RejectedGoodsScheduledJourney
//          .empty(exampleEori)
//          .submitMovementReferenceNumberAndDeclaration(exampleMrn, exampleDisplayDeclaration)
//          .getOrFail
//
//        val updatedSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = Some(journey))
//
//        inSequence {
//          mockAuthWithNoRetrievals()
//          mockGetSession(updatedSession)
//        }
//
//        checkPageIsDisplayed(
//          performAction(),
//          messageFromMessageKey(s"$messagesKey.title"),
//          doc => selectedCheckBox(doc) shouldBe empty
//        )
//      }

//      "display the page when a duty has already been selected before" in {
//        forAll(completeJourneyGen, genDuty) { (journey, dutyType: DutyType) =>
//          val updatedJourney = journey.selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
//          val updatedSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = updatedJourney.toOption)
//
//          inSequence {
//            mockAuthWithNoRetrievals()
//            mockGetSession(updatedSession)
//          }
//
//          checkPageIsDisplayed(
//            performAction(),
//            messageFromMessageKey(s"$messagesKey.title"),
//            doc => isCheckboxChecked(doc, dutyType.repr)
//          )
//        }
//      }

    }

    "Submit Select Duty Codes page" must {
      def performAction(data: Seq[(String, String)] = Seq.empty): Future[Result] =
        controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "not find the page if rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)

        status(performAction()) shouldBe NOT_FOUND
      }

//      "reject an empty duty type selection" in {
//
//        val journey = RejectedGoodsScheduledJourney
//          .empty(exampleEori)
//
//        val updatedSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = Some(journey))
//
//        inSequence {
//          mockAuthWithNoRetrievals()
//          mockGetSession(updatedSession)
//        }
//
//        checkPageIsDisplayed(
//          performAction(Seq(messagesKey -> "")),
//          messageFromMessageKey(s"$messagesKey.title"),
//          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messagesKey.error.required"),
//          expectedStatus = BAD_REQUEST
//        )
//      }

//      "select valid duty codes when none have been selected before" in forAll { dutyType: DutyType =>
//        val initialJourney = RejectedGoodsScheduledJourney
//          .empty(exampleEori)
//
//        val initialSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = Some(initialJourney))
//
//        val updatedJourney = initialJourney.selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
//        val updatedSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = updatedJourney.toOption)
//
//        inSequence {
//          mockAuthWithNoRetrievals()
//          mockGetSession(initialSession)
//          mockStoreSession(updatedSession)(Right(()))
//        }
//
//        checkIsRedirect(
//          performAction(Seq(s"$messagesKey[]" -> dutyType.repr)),
//          "/select-duties/:category page" //FIXME routes.SelectDutyCodesController.iterate()
//        )
//      }

    }

  }
}

