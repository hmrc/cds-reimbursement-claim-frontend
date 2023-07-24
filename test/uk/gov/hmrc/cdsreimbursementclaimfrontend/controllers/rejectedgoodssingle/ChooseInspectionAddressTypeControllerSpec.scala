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

import cats.implicits.catsSyntaxOptionId
import org.scalatest.{BeforeAndAfterEach, EitherValues, OptionValues}
import play.api.i18n._
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import shapeless.lens
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AddressLookupSupport, AuthSupport, PropertyBasedControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourneyGenerators._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Feature, InspectionAddress, InspectionAddressType, ReimbursementMethod, SessionData, TaxCode}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.ContactAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Acc14Gen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{AddressLookupService, FeatureSwitchService}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

class ChooseInspectionAddressTypeControllerSpec
    extends PropertyBasedControllerSpec
    with AuthSupport
    with SessionSupport
    with DeclarationSupport
    with BeforeAndAfterEach
    with AddressLookupSupport
    with OptionValues
    with EitherValues
    with Logging {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[AddressLookupService].toInstance(addressLookupServiceMock)
    )

  val session: SessionData = SessionData(journeyWithMrnAndDeclaration)

  val controller: ChooseInspectionAddressTypeController = instanceOf[ChooseInspectionAddressTypeController]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private lazy val featureSwitch = instanceOf[FeatureSwitchService]

  override def beforeEach(): Unit = featureSwitch enable Feature.RejectedGoods

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
              session.copy(rejectedGoodsSingleJourney =
                RejectedGoodsSingleJourney
                  .empty(displayDeclaration.getDeclarantEori)
                  .submitMovementReferenceNumberAndDeclaration(displayDeclaration.getMRN, displayDeclaration)
                  .toOption
              )
            )
          }

          checkPageIsDisplayed(
            showPage(),
            messageFromMessageKey("inspection-address.type.title"),
            doc => {
              val hasDeclarant = displayDeclaration.getDeclarantDetails.contactDetails.isDefined
              val hasImporter  = displayDeclaration.getConsigneeDetails
                .flatMap(_.contactDetails)
                .isDefined

              doc.getElementById("inspection-address.type").`val`()             shouldBe (if (hasImporter) "Importer"
                                                                              else "Declarant")
              if (hasImporter && hasDeclarant) {
                doc.getElementById("inspection-address.type-radio-Declarant").`val` shouldBe "Declarant"
              }
              doc.getElementById("inspection-address.type-radio-Other").`val`() shouldBe "Other"
              doc.select("input[value=Other]").isEmpty                          shouldBe false
            }
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
            RejectedGoodsSingleJourney
              .empty(declaration.getDeclarantEori)
              .submitMovementReferenceNumberAndDeclaration(
                declaration.getMRN,
                declarantContactDetailsLens.set(declaration)(contactDetails.some)
              )
              .toOption

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
            routes.CheckBankDetailsController.show()
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
            RejectedGoodsSingleJourney
              .empty(updatedDeclaration.getDeclarantEori)
              .submitMovementReferenceNumberAndDeclaration(updatedDeclaration.getMRN, updatedDeclaration)
              .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode(ndrc.taxType))))
              .toOption

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
            routes.ChooseRepaymentMethodController.show()
          )
        }
      }
    }

    "update inspection address and redirect to choose file type for subsidies only journey" when {
      "duties are eligible for CMA" in {
        forAll { (declaration: DisplayDeclaration, consigneeDetails: ConsigneeDetails, ndrc: NdrcDetails) =>
          val updatedDeclaration =
            declaration
              .copy(displayResponseDetail =
                declaration.displayResponseDetail.copy(
                  consigneeDetails = consigneeDetails.some,
                  ndrcDetails = List(ndrc.copy(cmaEligible = "1".some)).some
                )
              )
              .withAllSubsidiesPaymentMethod()

          val journey =
            RejectedGoodsSingleJourney
              .empty(
                updatedDeclaration.getDeclarantEori,
                features = Some(RejectedGoodsSingleJourney.Features(true, true))
              )
              .submitMovementReferenceNumberAndDeclaration(
                updatedDeclaration.getMRN,
                updatedDeclaration
              )
              .flatMap(_.selectAndReplaceTaxCodeSetForReimbursement(Seq(TaxCode(ndrc.taxType))))
              .flatMap(_.submitReimbursementMethod(ReimbursementMethod.Subsidy))
              .toOption

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
            routes.UploadFilesController.show()
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
        routes.CheckBankDetailsController.show()
      )
    }

    "redirect to CYA page" when {
      "journey is complete" in forAll(completeJourneyGen) { journey =>
        whenever(journey.getDeclarantContactDetailsFromACC14.isDefined) {
          val sessionWithJourney = SessionData(journey)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithJourney)
            mockStoreSession(
              sessionWithJourney.copy(rejectedGoodsSingleJourney =
                sessionWithJourney.rejectedGoodsSingleJourney.map(
                  _.submitInspectionAddress(
                    InspectionAddress
                      .ofType(InspectionAddressType.Declarant)
                      .mapFrom(journey.getDeclarantContactDetailsFromACC14.value)
                  )
                )
              )
            )(Right(()))
          }

          if (journey.needsBanksAccountDetailsSubmission)
            checkIsRedirect(
              submitAddress("inspection-address.type" -> InspectionAddressType.Declarant.toString),
              routes.CheckYourAnswersController.show()
            )
          else
            checkIsRedirect(
              submitAddress("inspection-address.type" -> InspectionAddressType.Declarant.toString),
              routes.UploadFilesController.show()
            )
        }
      }
    }
  }
}
