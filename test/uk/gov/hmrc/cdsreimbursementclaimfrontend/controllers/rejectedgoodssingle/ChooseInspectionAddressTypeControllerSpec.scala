package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import cats.implicits.catsSyntaxOptionId
import org.scalatest.BeforeAndAfterEach
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import shapeless.lens
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AddressLookupSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import scala.concurrent.Future

class ChooseInspectionAddressTypeControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with RejectedGoodsSingleJourneyTestData
    with AddressLookupSupport
    with OptionValues {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[AddressLookupService].toInstance(addressLookupServiceMock)
    )

  val session = SessionData.empty.copy(
    rejectedGoodsSingleJourney = Some(emptyJourney)
  )

  val controller: ChooseInspectionAddressTypeController = instanceOf[ChooseInspectionAddressTypeController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch enable Feature.RejectedGoods

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1)

  "Choose Inspection Address Type Controller" should {

    def showPage(): Future[Result] =
      controller.show()(FakeRequest())

    def submitAddress(data: (String, String)*): Future[Result] =
      controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

    "skip page" when {
      "no ACC14 addresses available" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          showPage(),
          routes.ChooseInspectionAddressTypeController.redirectToALF()
        )
      }
    }

    "display page" when {
      "at least one of the ACC14 addresses are available" in {
        forAll(
          arbitraryDisplayDeclaration.arbitrary.suchThat(declaration =>
            declaration.getDeclarantDetails.contactDetails.isDefined || declaration.getConsigneeDetails
              .flatMap(_.contactDetails)
              .isDefined
          )
        ) { displayDeclaration =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session.copy(rejectedGoodsSingleJourney = emptyJourney.submitDisplayDeclaration(displayDeclaration).some)
            )
          }

          checkPageIsDisplayed(
            showPage(),
            messageFromMessageKey("inspection-address.type.title")
          )
        }
      }
    }

    "call ALF" when {
      "other address type is selected" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          submitAddress("inspection-address.type" -> InspectionAddressType.Other.toString),
          routes.ChooseInspectionAddressTypeController.redirectToALF()
        )
      }
    }

    "update inspection address and redirect to the check bank details page" when {
      "duties are not eligible for CMA" in {
        val declarantContactDetailsLens =
          lens[DisplayDeclaration].displayResponseDetail.declarantDetails.contactDetails

        forAll { (declaration: DisplayDeclaration, contactDetails: ContactDetails) =>
          val journey =
            emptyJourney
              .submitDisplayDeclaration(declarantContactDetailsLens.set(declaration)(contactDetails.some))
              .some

          val sessionWithDeclaration = session.copy(rejectedGoodsSingleJourney = journey)

          val sessionWithInspectionAddress = session.copy(rejectedGoodsSingleJourney =
            journey.map(
              _.submitInspectionAddress(
                InspectionAddress.ofType(InspectionAddressType.Declarant).mapFrom(contactDetails)
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithDeclaration)
            mockStoreSession(sessionWithInspectionAddress)(Right(()))
          }

          checkIsRedirect(
            submitAddress("inspection-address.type" -> InspectionAddressType.Declarant.toString),
            "/check-bank-details"
          )
        }
      }
    }

    "update inspection address and redirect to the choose repayment method page" when {
      "duties are eligible for CMA" in {
        val consigneetDetailsLens = lens[DisplayDeclaration].displayResponseDetail.consigneeDetails

        forAll(arbitraryDisplayDeclaration.arbitrary, genConsigneeDetails.suchThat(_.contactDetails.isDefined)) {
          (declaration, consigneeDetails) =>
            val journey =
              emptyJourney
                .submitDisplayDeclaration(consigneetDetailsLens.set(declaration)(consigneeDetails.some))
                .some

            val sessionWithDeclaration = session.copy(rejectedGoodsSingleJourney = journey)

            val sessionWithInspectionAddress = session.copy(rejectedGoodsSingleJourney =
              journey.map(
                _.submitInspectionAddress(
                  consigneeDetails.contactDetails
                    .map(InspectionAddress.ofType(InspectionAddressType.Importer).mapFrom(_))
                    .value
                )
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDeclaration)
              mockStoreSession(sessionWithInspectionAddress)(Right(()))
            }

            checkIsRedirect(
              submitAddress("inspection-address.type" -> InspectionAddressType.Importer.toString),
              "/choose-repayment-method"
            )
        }
      }
    }
  }
}
