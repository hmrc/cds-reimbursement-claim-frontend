package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.SelectDutyCodesControllerSpec.genDutyWithRandomlySelectedTaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.EnterClaimController.findDutyTypeAndTaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.{completeJourneyGen, exampleEori, taxCodesGen}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyTypes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Reimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DutyTypeGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.collection.SortedMap

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
  val enterClaimKey: String            = "enter-scheduled-claim"

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session: SessionData = SessionData.empty.copy(
    rejectedGoodsScheduledJourney = Some(RejectedGoodsScheduledJourney.empty(exampleEori))
  )

  "Select Duty Codes Controller" should {

    "not find the page if rejected goods feature is disabled" in forAll(genDutyWithRandomlySelectedTaxCode) {
      case (dutyType: DutyType, taxCode: TaxCode) =>
        featureSwitch.disable(Feature.RejectedGoods)

        status(controller.show(dutyType, taxCode)(FakeRequest())) shouldBe NOT_FOUND
    }

    "redirect to the enter claim page" when {

      "the user has not answered this question before" in forAll(genDutyWithRandomlySelectedTaxCode) {
        case (dutyType: DutyType, taxCode: TaxCode) =>
          val initialJourney = RejectedGoodsScheduledJourney
            .empty(exampleEori)
            .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
            .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(dutyType, Seq(taxCode)))
          val initialSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = initialJourney.toOption)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(initialSession)
          }

          checkIsRedirect(
            controller.iterate()(FakeRequest()),
            routes.EnterClaimController.show(dutyType, taxCode)
          )

      } // not answered question before

    } // redirect to  enter claim page

    "redirect to the check claim page" when {

      "the user has a complete reimbursement claim" in forAll(completeJounrye) { journey=>
          val initialJourney = RejectedGoodsScheduledJourney
            .empty(exampleEori)
            
          val initialSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = initialJourney.toOption)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(initialSession)
          }

          checkIsRedirect(
            controller.iterate()(FakeRequest()),
            "/scheduled/check-claim"
          )

      } // has complete claim

    } // redirect to check claim page

  } //controller end
} //end
