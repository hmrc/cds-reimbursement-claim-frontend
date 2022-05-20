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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import cats.Functor
import cats.Id
import cats.data.EitherT
import cats.implicits._
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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.AuthSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.C285JourneySessionFixtures
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimsRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.genOtherThan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms

class EnterMovementReferenceNumberControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckPropertyChecks
    with OptionValues
    with EitherValues
    with C285JourneySessionFixtures {

  val mockClaimsService: ClaimService = mock[ClaimService]

  val enterMovementReferenceNumberKey: String = "enter-movement-reference-number"

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService)
    )

  def mockGetDisplayDeclaration(response: Either[Error, Option[DisplayDeclaration]]) =
    (mockClaimsService
      .getDisplayDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))

  val controller: EnterMovementReferenceNumberController = instanceOf[EnterMovementReferenceNumberController]
  val featureSwitch: FeatureSwitchService                = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2)

  "Movement Reference Number Controller page titles" when {

    def runJourney(
      typeOfClaim: TypeOfClaimAnswer,
      expectedTitle: String
    ) = {

      val (session, _) = sessionWithMRNAndTypeOfClaimOnly(None, Some(typeOfClaim))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        controller.enterJourneyMrn(FakeRequest()),
        messageFromMessageKey(expectedTitle),
        doc => {
          doc.select(s"#$enterMovementReferenceNumberKey").`val`() shouldBe ""
          doc.select("form").attr("action")                        shouldBe routes.EnterMovementReferenceNumberController.enterMrnSubmit.url
        }
      )
    }

    "Enter MRN page" must {
      "show title on the Scheduled journey" in {
        runJourney(
          TypeOfClaimAnswer.Scheduled,
          s"$enterMovementReferenceNumberKey.scheduled.title"
        )
      }
    }
  }

  "Movement Reference Number Controller" when {

    "Enter MRN page" must {

      def performAction: Future[Result] =
        controller.enterJourneyMrn(FakeRequest())

      "display the title and the previously saved MRN" in forAll { (mrnAnswer: MRN) =>
        val typeOfClaim = TypeOfClaimAnswer.Scheduled
        val router      = JourneyExtractor.getRoutes(typeOfClaim, JourneyBindable.Scheduled)

        val (session, _) = sessionWithMRNAndTypeOfClaimOnly(mrnAnswer.some, Some(typeOfClaim))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction,
          messageFromMessageKey(
            s"$enterMovementReferenceNumberKey${router.subKey.map(a => s".$a").getOrElse("")}.title"
          ),
          doc => {
            doc.select(s"#$enterMovementReferenceNumberKey").`val`() shouldBe mrnAnswer.value
            doc.select("form").attr("action")                        shouldBe
              routes.EnterMovementReferenceNumberController.enterMrnSubmit.url
          }
        )
      }

    }

    "We enter an MRN for the first time or update it with the back button (enterMrnSubmit)" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.enterMrnSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      "reject an invalid MRN" in {
        val invalidMRN  = MRN("INVALID_MOVEMENT_REFERENCE_NUMBER")
        val typeOfClaim = TypeOfClaimAnswer.Scheduled
        val router      = JourneyExtractor.getRoutes(typeOfClaim, JourneyBindable.Scheduled)

        val (session, _) = sessionWithMRNAndTypeOfClaimOnly(None, Some(TypeOfClaimAnswer.Scheduled))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result = performAction(enterMovementReferenceNumberKey -> invalidMRN.value)

        checkPageIsDisplayed(
          result,
          messageFromMessageKey(
            s"$enterMovementReferenceNumberKey${router.subKey.map(a => s".$a").getOrElse("")}.title"
          ),
          doc =>
            getErrorSummary(doc) shouldBe messageFromMessageKey(s"$enterMovementReferenceNumberKey.invalid.number"),
          expectedStatus = 400
        )
      }

      "start a new claim with an MRN, Eori is importer's Eori" in forAll {
        (
          consigneeDetails: ConsigneeDetails,
          displayDeclaration: DisplayDeclaration,
          mrn: MRN
        ) =>
          val (session, foc) =
            sessionWithMRNAndTypeOfClaimOnly(None, Some(TypeOfClaimAnswer.Scheduled))

          val updatedConsigneeDetails   = consigneeDetails.copy(consigneeEORI = foc.signedInUserDetails.eori.value)
          val updatedDisplayDeclaration = Functor[Id].map(displayDeclaration)(dd =>
            dd.copy(displayResponseDetail =
              dd.displayResponseDetail.copy(consigneeDetails = Some(updatedConsigneeDetails))
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetDisplayDeclaration(Right(Some(updatedDisplayDeclaration)))
            mockStoreSession(Right(()))
          }
          val result = performAction(enterMovementReferenceNumberKey -> mrn.value)

          status(result) shouldBe 303
          checkIsRedirect(
            result,
            claimsRoutes.CheckDeclarationDetailsController.show(JourneyBindable.Scheduled)
          )
      }

      "Update an MRN, Eori is importer's Eori" in forAll {
        (
          displayDeclaration: DisplayDeclaration,
          consigneeDetails: ConsigneeDetails,
          mrn: MRN
        ) =>
          val mrnAnswer      = mrn.some
          val (session, foc) =
            sessionWithMRNAndTypeOfClaimOnly(mrnAnswer, Some(TypeOfClaimAnswer.Scheduled))

          val updatedConsigneeDetails   = consigneeDetails.copy(consigneeEORI = foc.signedInUserDetails.eori.value)
          val updatedDisplayDeclaration = Functor[Id].map(displayDeclaration)(dd =>
            dd.copy(displayResponseDetail =
              dd.displayResponseDetail.copy(consigneeDetails = Some(updatedConsigneeDetails))
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetDisplayDeclaration(Right(Some(updatedDisplayDeclaration)))
            mockStoreSession(Right(()))
          }
          val result = performAction(enterMovementReferenceNumberKey -> genOtherThan(mrn).value)

          status(result) shouldBe 303
          checkIsRedirect(
            result,
            claimsRoutes.CheckDeclarationDetailsController.show(JourneyBindable.Scheduled)
          )
      }

      "On submitting the same MRN as before, don't save it, Eori is importer's Eori" in
        forAll(
          genMRN,
          genConsigneeDetails,
          arbitraryDisplayDeclaration.arbitrary
        ) { (mrn, consigneeDetails, displayDeclaration) =>
          val mrnAnswer      = mrn.some
          val (session, foc) = sessionWithMRNAndTypeOfClaimOnly(mrnAnswer, Some(TypeOfClaimAnswer.Scheduled))

          val updatedConsigneeDetails   = consigneeDetails.copy(consigneeEORI = foc.signedInUserDetails.eori.value)
          val updatedDisplayDeclaration = Functor[Id].map(displayDeclaration)(dd =>
            dd.copy(displayResponseDetail =
              dd.displayResponseDetail.copy(consigneeDetails = Some(updatedConsigneeDetails))
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetDisplayDeclaration(Right(Some(updatedDisplayDeclaration)))
            mockStoreSession(Right(()))
          }

          val result = performAction(enterMovementReferenceNumberKey -> mrn.value)

          status(result) shouldBe 303
          checkIsRedirect(
            result,
            claimsRoutes.CheckDeclarationDetailsController.show(JourneyBindable.Scheduled)
          )
        }

      "start a new claim with an MRN, Eori is not the importer's Eori" in forAll {
        (
          displayDeclaration: DisplayDeclaration,
          consigneeDetails: ConsigneeDetails,
          mrn: MRN,
          eori: Eori
        ) =>
          val (session, _) = sessionWithMRNAndTypeOfClaimOnly(None, Some(TypeOfClaimAnswer.Scheduled))

          val updatedConsigneeDetails   = consigneeDetails.copy(consigneeEORI = eori.value)
          val updatedDisplayDeclaration = Functor[Id].map(displayDeclaration)(dd =>
            dd.copy(displayResponseDetail =
              dd.displayResponseDetail.copy(consigneeDetails = Some(updatedConsigneeDetails))
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetDisplayDeclaration(Right(Some(updatedDisplayDeclaration)))
            mockStoreSession(Right(()))
          }

          val result = performAction(enterMovementReferenceNumberKey -> mrn.value)

          status(result) shouldBe 303
          checkIsRedirect(
            result,
            claimsRoutes.EnterImporterEoriNumberController.enterImporterEoriNumber(JourneyBindable.Scheduled)
          )
      }
    }
  }

  "We update an MRN coming from the Check Your Answer page (changeMrnSubmit)" must {

    def performAction(data: (String, String)*): Future[Result] =
      controller.enterMrnSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

    "return to CYA page if the same MRN is submitted" in forAll { (mrn: MRN) =>
      val (session, _) =
        sessionWithCompleteC285Claim(mrn, TypeOfClaimAnswer.Scheduled)

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }
      val result = performAction(enterMovementReferenceNumberKey -> mrn.value)

      status(result) shouldBe 303
      redirectLocation(
        result
      ).value        shouldBe routes.CheckYourAnswersAndSubmitController.checkAllAnswers.url
    }

    "start a new claim if a different MRN is submitted" in {
      (
        displayDeclaration: DisplayDeclaration,
        mrn: MRN
      ) =>
        val (session, _) =
          sessionWithCompleteC285Claim(mrn, TypeOfClaimAnswer.Scheduled)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
          mockStoreSession(Right(()))
        }

        val result = performAction(enterMovementReferenceNumberKey -> genOtherThan(mrn).value)

        status(result) shouldBe 303
        redirectLocation(
          result
        ).value        shouldBe claimsRoutes.EnterImporterEoriNumberController
          .enterImporterEoriNumber(JourneyBindable.Scheduled)
          .url
    }
  }

  "Form validation" must {

    val form = Forms.movementReferenceNumberForm

    "accept valid MRN" in forAll { mrn: MRN =>
      val errors =
        form.bind(Map(enterMovementReferenceNumberKey -> mrn.value)).errors
      errors shouldBe Nil
    }

    "reject 19 characters" in {
      val errors =
        form.bind(Map(enterMovementReferenceNumberKey -> "910ABCDEFGHIJKLMNO0")).errors
      errors.headOption.value.messages shouldBe List("invalid.number")
    }

    "reject 17 characters" in {
      val errors = form
        .bind(Map(enterMovementReferenceNumberKey -> "123456789A1234567"))
        .errors
      errors.headOption.value.messages shouldBe List("invalid.number")
    }
  }

}