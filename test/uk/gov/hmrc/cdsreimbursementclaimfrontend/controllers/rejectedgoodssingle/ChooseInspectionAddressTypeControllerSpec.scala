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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import cats.implicits.catsSyntaxOptionId
import org.scalatest.BeforeAndAfterEach
import org.scalatest.EitherValues
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.AddressLookupService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class ChooseInspectionAddressTypeControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with RejectedGoodsSingleJourneyTestData
    with AddressLookupSupport
    with OptionValues
    with EitherValues {

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

  implicit val ec: ExecutionContext = ExecutionContext.global

  "Choose Inspection Address Type Controller" should {

    def showPage(): Future[Result] =
      controller.show()(FakeRequest())

    def submitAddress(data: (String, String)*): Future[Result] =
      controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

    def retrieveAddress(maybeAddressId: Option[UUID]): Future[Result] =
      controller.retrieveAddressFromALF(maybeAddressId)(FakeRequest())

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
        forAll { (declaration: DisplayDeclaration, consigneeDetails: ConsigneeDetails, ndrc: NdrcDetails) =>
          val updatedDeclaration = lens[DisplayDeclaration].displayResponseDetail.modify(declaration)(
            _.copy(
              consigneeDetails = consigneeDetails.some,
              ndrcDetails = List(ndrc.copy(cmaEligible = "1".some)).some
            )
          )

          val journey =
            emptyJourney
              .submitDisplayDeclaration(updatedDeclaration)
              .selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode(ndrc.taxType)))
              .value
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

    "update inspection address once address lookup complete" in forAll { contactAddress: ContactAddress =>
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockAddressRetrieve(Right(contactAddress))
        mockStoreSession(
          session.copy(rejectedGoodsSingleJourney =
            session.rejectedGoodsSingleJourney.map(
              _.submitInspectionAddress(
                InspectionAddress.ofType(InspectionAddressType.Other).mapFrom(contactAddress)
              )
            )
          )
        )(Right(()))
      }

      checkIsRedirect(
        retrieveAddress(Some(UUID.randomUUID())),
        "/check-bank-details"
      )
    }
  }
}