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
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectDutyCodesController.selectDutyCodesKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled.SelectDutyCodesControllerSpec.genDutyWithRandomlySelectedTaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators.{completeJourneyGen, dutyTypesWithTaxCodesGen, exampleEori}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DutyType, Feature, SessionData, TaxCode, TaxCodes}
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
  implicit val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  private val messagesKey: String = "select-duty-codes"

  override def beforeEach(): Unit =
    featureSwitch.enable(Feature.RejectedGoods)

  val session: SessionData = SessionData.empty.copy(
    rejectedGoodsScheduledJourney = Some(RejectedGoodsScheduledJourney.empty(exampleEori))
  )

  "Select Duty Codes Controller" should {

    "redirect to the select duty types page" when {
      "user has not selected any duty types" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          controller.iterate()(FakeRequest()),
          routes.SelectDutyTypesController.show()
        )
      }
    }

    "show select tax codes page" when {

      "user has previously selected duty types" in forAll(completeJourneyGen, genDuty) { (journey, dutyType: DutyType) =>
        val updatedJourney = journey.selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
        val updatedSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = updatedJourney.toOption)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkIsRedirect(
          controller.iterate()(FakeRequest()),
          routes.SelectDutyCodesController.show(dutyType)
        )
      }

      "the user has not answered this question before" in forAll(genDuty) { dutyType =>

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.show(dutyType)(FakeRequest()),
          messageFromMessageKey(
            s"$selectDutyCodesKey.title",
            messageFromMessageKey(s"$selectDutyCodesKey.h1.${dutyType.repr}")
          ),
          doc => selectedCheckBox(doc) shouldBe empty
        )

      }

    }

    "tick existing tax codes" when {
      "select tax code page is shown" in forAll(genDutyWithRandomlySelectedTaxCode) { case (dutyType: DutyType, taxCode: TaxCode) =>

        val journey = RejectedGoodsScheduledJourney
          .empty(exampleEori)
          .selectAndReplaceDutyTypeSetForReimbursement(Seq(dutyType))
          .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(dutyType, Seq(taxCode)))

        val updatedSession = SessionData.empty.copy(rejectedGoodsScheduledJourney = journey.toOption)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(updatedSession)
        }

        checkPageIsDisplayed(
          controller.show(dutyType)(FakeRequest()),
          messageFromMessageKey(
            s"$selectDutyCodesKey.title",
            messageFromMessageKey(s"$selectDutyCodesKey.h1.${dutyType.repr}")
          ),
          doc => isCheckboxChecked(doc, taxCode.value) shouldBe true
        )

      }


    }
  }
  //  def performAction(): Future[Result] = controller.show()(FakeRequest())
  //
  //      "not find the page if rejected goods feature is disabled" in {
  //        featureSwitch.disable(Feature.RejectedGoods)
  //
  //        status(performAction()) shouldBe NOT_FOUND
  //      }

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



  "Submit Select Tax Codes page" must {

    "not find the page if rejected goods feature is disabled" in forAll { duty: DutyType =>
      featureSwitch.disable(Feature.RejectedGoods)

      status(controller.submit(duty)(FakeRequest())) shouldBe NOT_FOUND
    }

    "save user selected tax codes and redirect to the next page" when {

      "no other selected duties remaining" in forAll(genDutyWithRandomlySelectedTaxCode) { case (duty, taxCode) =>

        val initialJourney = RejectedGoodsScheduledJourney.empty(exampleEori).selectAndReplaceDutyTypeSetForReimbursement(Seq(duty))
        val initialSession = session.copy(rejectedGoodsScheduledJourney = initialJourney.toOption)

        val updatedJourney = initialJourney.flatMap(journey => journey.selectAndReplaceTaxCodeSetForReimbursement(duty, Seq(taxCode)))
        val updatedSession = initialSession.copy(rejectedGoodsScheduledJourney = updatedJourney.toOption)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(initialSession)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          controller.submit(duty)(
            FakeRequest().withFormUrlEncodedBody(s"$selectDutyCodesKey[]" -> taxCode.value)
          ),
          "routes.[NEW CONTROLLER].show()" //FIXME routes.EnterScheduledClaimController.iterate()
        )
      }
    }


    "show an error summary" when {

      "no duty code is selected" in forAll { duty: DutyType =>

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          controller.submit(duty)(FakeRequest()),
          messageFromMessageKey(
            s"$selectDutyCodesKey.title",
            messageFromMessageKey(s"$selectDutyCodesKey.h1.${duty.repr}")
          ),
          doc =>
            doc
              .select(".govuk-error-summary__list > li:nth-child(1) > a")
              .text() shouldBe messageFromMessageKey(
              s"$selectDutyCodesKey.error.required"
            ),
          BAD_REQUEST
        )
      }
    }

  }
}


object SelectDutyCodesControllerSpec {

  lazy val genDutyWithRandomlySelectedTaxCode: Gen[(DutyType, TaxCode)] = for {
    duty    <- genDuty
    taxCode <- Gen.oneOf(duty.taxCodes)
  } yield (duty, taxCode)

}
