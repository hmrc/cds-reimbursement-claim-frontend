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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import play.api.i18n.Lang
import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.MessagesImpl
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.mvc.Results.Redirect
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.PropertyBasedControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyGenerators.buildCompleteJourneyGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourneyTestData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.InspectionAddressUtils
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.StringUtils.StringOps

import java.util.UUID
import scala.concurrent.Future

class ChooseInspectionAddressTypeControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with RejectedGoodsMultipleJourneyTestData
    with InspectionAddressUtils {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache)
    )

  val session = SessionData.empty.copy(
    rejectedGoodsMultipleJourney = Some(emptyJourney)
  )

  val controller: ChooseInspectionAddressTypeController = instanceOf[ChooseInspectionAddressTypeController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch  = instanceOf[FeatureSwitchService]
  private val messagesKey: String = "inspection-address.type"

  override def beforeEach(): Unit = featureSwitch enable Feature.RejectedGoods

  def showPage(): Future[Result] =
    controller.show()(FakeRequest())

  def submitAddress(data: (String, String)*): Future[Result] =
    controller.submit()(FakeRequest().withFormUrlEncodedBody(data: _*))

  def retrieveAddress(maybeAddressId: Option[UUID]): Future[Result] =
    controller.retrieveAddressFromALF(maybeAddressId)(FakeRequest())

  "Choose Inspection Address Type Controller" should {

    "display the page" when {
      "does not find the page if the rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(showPage()) shouldBe NOT_FOUND
      }

      "Show the page when the lead Acc 14 Declaration does not have a consignee" in forAll {
        (contactDetails: ContactDetails, displayDeclaration: DisplayDeclaration) =>
          val declarant             = displayDeclaration.getDeclarantDetails.copy(contactDetails = Some(contactDetails))
          val displayResponseDetail = displayDeclaration.displayResponseDetail
            .copy(
              consigneeDetails = None,
              declarantDetails = declarant
            )
          val updatedJourney        = session.rejectedGoodsMultipleJourney.get
            .submitMovementReferenceNumberAndDeclaration(
              0,
              displayDeclaration.getMRN,
              DisplayDeclaration(displayResponseDetail)
            )
            .getOrFail

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney)))
          }

          checkPageIsDisplayed(
            showPage(),
            messageFromMessageKey("inspection-address.type.title"),
            doc => {
              doc.select("input[value=Other]").isEmpty     shouldBe false
              doc.select("input[value=Declarant]").isEmpty shouldBe false
              doc.select("input[value=Importer]").isEmpty  shouldBe true
            }
          )
      }

      "Show the page when the lead Acc 14 Declaration does has a consignee, with contact details, and the declarant does not have any contact details" in forAll {
        (consignee: ConsigneeDetails, displayDeclaration: DisplayDeclaration) =>
          val displayResponseDetail = displayDeclaration.displayResponseDetail
            .copy(
              consigneeDetails = Some(consignee),
              declarantDetails = displayDeclaration.displayResponseDetail.declarantDetails.copy(contactDetails = None)
            )
          val updatedJourney        = session.rejectedGoodsMultipleJourney.get
            .submitMovementReferenceNumberAndDeclaration(
              0,
              displayDeclaration.getMRN,
              DisplayDeclaration(displayResponseDetail)
            )
            .getOrFail

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney)))
          }

          checkPageIsDisplayed(
            showPage(),
            messageFromMessageKey("inspection-address.type.title"),
            doc => {
              doc.select("input[value=Other]").isEmpty     shouldBe false
              doc.select("input[value=Declarant]").isEmpty shouldBe true
              doc.select("input[value=Importer]").isEmpty  shouldBe false
            }
          )
      }

      "Show the page, with the existing value pre-selected, when the lead Acc 14 Declaration does has a consignee, with contact details, and the declarant does not has contact details" in forAll {
        (contactDetails: ContactDetails, consignee: ConsigneeDetails, displayDeclaration: DisplayDeclaration) =>
          val declarant             = displayDeclaration.getDeclarantDetails.copy(contactDetails = Some(contactDetails))
          val displayResponseDetail = displayDeclaration.displayResponseDetail
            .copy(
              consigneeDetails = Some(consignee),
              declarantDetails = declarant
            )
          val journey               = session.rejectedGoodsMultipleJourney.get
            .submitMovementReferenceNumberAndDeclaration(
              0,
              displayDeclaration.getMRN,
              DisplayDeclaration(displayResponseDetail)
            )
            .getOrFail
          val optionChosen          = Gen.oneOf(Seq(Importer, Declarant)).sample.get
          val address               = optionChosen match {
            case Importer      =>
              inspectionAddressFromContactDetails(consignee.contactDetails.get, Importer)
            case Declarant | _ =>
              inspectionAddressFromContactDetails(declarant.contactDetails.get, Declarant)
          }
          val updatedJourney        = journey.submitInspectionAddress(address)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney)))
          }

          checkPageIsDisplayed(
            showPage(),
            messageFromMessageKey("inspection-address.type.title"),
            doc => {
              doc.select("input[value=Other]").isEmpty      shouldBe false
              doc.select("input[value=Declarant]").isEmpty  shouldBe false
              doc.select("input[value=Importer]").isEmpty   shouldBe false
              isCheckboxChecked(doc, optionChosen.toString) shouldBe true
            }
          )
      }

      "Show the page when the lead Acc 14 Declaration does has a consignee, with contact details, and the declarant does not has contact details" in forAll {
        (contactDetails: ContactDetails, consignee: ConsigneeDetails, displayDeclaration: DisplayDeclaration) =>
          val declarant             = displayDeclaration.getDeclarantDetails.copy(contactDetails = Some(contactDetails))
          val displayResponseDetail = displayDeclaration.displayResponseDetail
            .copy(
              consigneeDetails = Some(consignee),
              declarantDetails = declarant
            )
          val updatedJourney        = session.rejectedGoodsMultipleJourney.get
            .submitMovementReferenceNumberAndDeclaration(
              0,
              displayDeclaration.getMRN,
              DisplayDeclaration(displayResponseDetail)
            )
            .getOrFail

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney)))
          }

          checkPageIsDisplayed(
            showPage(),
            messageFromMessageKey("inspection-address.type.title"),
            doc => {
              doc.select("input[value=Other]").isEmpty     shouldBe false
              doc.select("input[value=Declarant]").isEmpty shouldBe false
              doc.select("input[value=Importer]").isEmpty  shouldBe false
            }
          )
      }

      "redirect to the address lookup page if no addresses present" in {
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

    "handle submit request on new journey" when {
      "does not find the page if the rejected goods feature is disabled" in {
        featureSwitch.disable(Feature.RejectedGoods)
        status(submitAddress()) shouldBe NOT_FOUND
      }

      "the user submits an empty form" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          submitAddress(),
          messageFromMessageKey(s"$messagesKey.title"),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(
              s"$messagesKey.error.required"
            ),
          expectedStatus = BAD_REQUEST
        )
      }

      "the user selects one of the addresses" in forAll {
        (contactDetails: ContactDetails, consignee: ConsigneeDetails, displayDeclaration: DisplayDeclaration) =>
          val declarant             = displayDeclaration.getDeclarantDetails.copy(contactDetails = Some(contactDetails))
          val displayResponseDetail = displayDeclaration.displayResponseDetail
            .copy(
              consigneeDetails = Some(consignee),
              declarantDetails = declarant
            )
          val journey               = session.rejectedGoodsMultipleJourney.get
            .submitMovementReferenceNumberAndDeclaration(
              0,
              displayDeclaration.getMRN,
              DisplayDeclaration(displayResponseDetail)
            )
            .getOrFail
          val optionChosen          = Gen.oneOf(Seq(Importer, Declarant)).sample.get
          val address               = optionChosen match {
            case Importer =>
              inspectionAddressFromContactDetails(consignee.contactDetails.get, Importer)
            case _        =>
              inspectionAddressFromContactDetails(declarant.contactDetails.get, Declarant)
          }
          val updatedJourney        = journey.submitInspectionAddress(address)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(rejectedGoodsMultipleJourney = Some(journey)))
            mockStoreSession(session.copy(rejectedGoodsMultipleJourney = Some(updatedJourney)))(Right(()))
          }

          checkIsRedirect(
            submitAddress(messagesKey -> optionChosen.toString),
            routes.CheckBankDetailsController.show()
          )
      }

      "the user selects 'other' address" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          submitAddress(messagesKey -> Other.toString),
          routes.ChooseInspectionAddressTypeController.redirectToALF()
        )
      }
    }

    "update address" in forAll { address: ContactAddress =>
      val inspectionAddress = InspectionAddress(
        addressLine1 = Some(address.line1),
        addressLine2 = address.line2,
        addressLine3 = address.line3,
        city = address.line4.asSomeIfNonEmpty,
        countryCode = address.country.code.asSomeIfNonEmpty,
        postalCode = address.postcode.asSomeIfNonEmpty,
        addressType = Other
      )

      val expectedJourney = emptyJourney.submitInspectionAddress(inspectionAddress)

      controller.update(emptyJourney)(address) shouldBe expectedJourney
    }

    "redirect to the next page" when {
      "on a new journey" in {
        controller.redirectToTheNextPage(emptyJourney) shouldBe (
          (
            emptyJourney,
            Redirect(routes.CheckBankDetailsController.show())
          )
        )
      }

      "changing the entered details" in forAll(buildCompleteJourneyGen()) { journey =>
        controller.redirectToTheNextPage(journey) shouldBe (
          (
            journey,
            Redirect(routes.CheckYourAnswersController.show())
          )
        )
      }
    }
  }
}
