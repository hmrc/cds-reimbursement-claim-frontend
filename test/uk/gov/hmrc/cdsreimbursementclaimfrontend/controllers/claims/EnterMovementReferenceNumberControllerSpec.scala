/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.Functor
import cats.Id
import cats.data.EitherT
import cats.implicits._
import org.jsoup.nodes.Document
import org.scalacheck.Gen
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.enterMovementReferenceNumberKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.genOtherThan
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyBindableGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnterMovementReferenceNumberControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckPropertyChecks
    with OptionValues
    with EitherValues
    with C285JourneySessionFixtures {

  val mockClaimsService: ClaimService = mock[ClaimService]

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

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2)

  def getErrorSummary(document: Document): String =
    document.select(".govuk-error-summary__list > li > a").text()

  "Movement Reference Number Controller page titles" when {

    def runJourney(
      journeyBindable: JourneyBindable,
      typeOfClaim: TypeOfClaimAnswer,
      expectedTitle: String
    ) = {
      val (session, _) = sessionWithMRNAndTypeOfClaimOnly(None, Some(typeOfClaim))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        controller.enterJourneyMrn(journeyBindable)(FakeRequest()),
        messageFromMessageKey(expectedTitle),
        doc => {
          doc.select(s"#$enterMovementReferenceNumberKey").`val`() shouldBe ""
          doc.select("form").attr("action")                        shouldBe routes.EnterMovementReferenceNumberController
            .enterMrnSubmit(journeyBindable)
            .url
        }
      )
    }

    "Enter MRN page" must {
      "show title on the Single journey" in {
        runJourney(
          JourneyBindable.Single,
          TypeOfClaimAnswer.Individual,
          s"$enterMovementReferenceNumberKey.title"
        )
      }
      "show title on the Multiple journey" in {
        runJourney(
          JourneyBindable.Multiple,
          TypeOfClaimAnswer.Multiple,
          s"$enterMovementReferenceNumberKey.multiple.title"
        )
      }
      "show title on the Scheduled journey" in {
        runJourney(
          JourneyBindable.Scheduled,
          TypeOfClaimAnswer.Scheduled,
          s"$enterMovementReferenceNumberKey.scheduled.title"
        )
      }
    }
  }

  "Movement Reference Number Controller" when {

    "Enter MRN page" must {

      def performAction(journeyBindable: JourneyBindable): Future[Result] =
        controller.enterJourneyMrn(journeyBindable)(FakeRequest())

      "display the title and the previously saved MRN" in forAll { (mrnAnswer: MRN, journeyBindable: JourneyBindable) =>
        val typeOfClaim = toTypeOfClaim(journeyBindable)
        val router      = JourneyExtractor.getRoutes(typeOfClaim, journeyBindable)

        val (session, _) = sessionWithMRNAndTypeOfClaimOnly(mrnAnswer.some, Some(typeOfClaim))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(journeyBindable),
          messageFromMessageKey(
            s"$enterMovementReferenceNumberKey${router.subKey.map(a => s".$a").getOrElse("")}.title"
          ),
          doc => {
            doc.select(s"#$enterMovementReferenceNumberKey").`val`() shouldBe mrnAnswer.value
            doc.select("form").attr("action")                        shouldBe
              routes.EnterMovementReferenceNumberController.enterMrnSubmit(journeyBindable).url
          }
        )
      }

    }

    "We enter an MRN for the first time or update it with the back button (enterMrnSubmit)" must {

      def performAction(journeyBindable: JourneyBindable, data: (String, String)*): Future[Result] =
        controller.enterMrnSubmit(journeyBindable)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "reject an invalid MRN" in forAll { journeyBindable: JourneyBindable =>
        val invalidMRN  = MRN("INVALID_MOVEMENT_REFERENCE_NUMBER")
        val typeOfClaim = toTypeOfClaim(journeyBindable)
        val router      = JourneyExtractor.getRoutes(typeOfClaim, journeyBindable)

        val (session, _) = sessionWithMRNAndTypeOfClaimOnly(None, Some(toTypeOfClaim(journeyBindable)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result = performAction(journeyBindable, enterMovementReferenceNumberKey -> invalidMRN.value)

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
          journeyBindable: JourneyBindable,
          displayDeclaration: DisplayDeclaration,
          mrn: MRN
        ) =>
          val (session, foc) =
            sessionWithMRNAndTypeOfClaimOnly(None, Some(toTypeOfClaim(journeyBindable)))

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
          val result = performAction(journeyBindable, enterMovementReferenceNumberKey -> mrn.value)

          status(result) shouldBe 303
          checkIsRedirect(
            result,
            routes.CheckDeclarationDetailsController.show(journeyBindable)
          )
      }

      "Update an MRN, Eori is importer's Eori" in forAll {
        (
          journeyBindable: JourneyBindable,
          displayDeclaration: DisplayDeclaration,
          consigneeDetails: ConsigneeDetails,
          mrn: MRN
        ) =>
          val mrnAnswer      = mrn.some
          val (session, foc) =
            sessionWithMRNAndTypeOfClaimOnly(mrnAnswer, Some(toTypeOfClaim(journeyBindable)))

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
          val result = performAction(journeyBindable, enterMovementReferenceNumberKey -> genOtherThan(mrn).value)

          status(result) shouldBe 303
          checkIsRedirect(
            result,
            routes.CheckDeclarationDetailsController.show(journeyBindable)
          )
      }

      "On submitting the same MRN as before, don't save it, Eori is importer's Eori" in
        forAll(
          Gen.oneOf(JourneyBindable.Single, JourneyBindable.Scheduled),
          genMRN,
          genConsigneeDetails,
          arbitraryDisplayDeclaration.arbitrary
        ) { (journeyBindable, mrn, consigneeDetails, displayDeclaration) =>
          val mrnAnswer      = mrn.some
          val (session, foc) = sessionWithMRNAndTypeOfClaimOnly(mrnAnswer, Some(toTypeOfClaim(journeyBindable)))

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

          val result = performAction(journeyBindable, enterMovementReferenceNumberKey -> mrn.value)

          status(result) shouldBe 303
          checkIsRedirect(
            result,
            routes.CheckDeclarationDetailsController.show(journeyBindable)
          )
        }

      "Redirect back to Check Movement Numbers page when Lead MRN unchanged" in
        forAll { mrn: MRN =>
          val mrnAnswer    = mrn.some
          val (session, _) =
            sessionWithMRNAndTypeOfClaimOnly(mrnAnswer, Some(toTypeOfClaim(JourneyBindable.Multiple)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          val result = performAction(JourneyBindable.Multiple, enterMovementReferenceNumberKey -> mrn.value)

          status(result) shouldBe 303
          checkIsRedirect(
            result,
            routes.CheckMovementReferenceNumbersController.showMrns()
          )
        }

      "start a new claim with an MRN, Eori is not the importer's Eori" in forAll {
        (
          journeyBindable: JourneyBindable,
          displayDeclaration: DisplayDeclaration,
          consigneeDetails: ConsigneeDetails,
          mrn: MRN,
          eori: Eori
        ) =>
          val (session, _) = sessionWithMRNAndTypeOfClaimOnly(None, Some(toTypeOfClaim(journeyBindable)))

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

          val result = performAction(journeyBindable, enterMovementReferenceNumberKey -> mrn.value)

          status(result) shouldBe 303
          checkIsRedirect(
            result,
            routes.EnterImporterEoriNumberController.enterImporterEoriNumber(journeyBindable)
          )
      }
    }
  }

  "We update an MRN coming from the Check Your Answer page (changeMrnSubmit)" must {

    def performAction(journeyBindable: JourneyBindable, data: (String, String)*): Future[Result] =
      controller.enterMrnSubmit(journeyBindable)(FakeRequest().withFormUrlEncodedBody(data: _*))

    "return to CYA page if the same MRN is submitted" in forAll { (journeyBindable: JourneyBindable, mrn: MRN) =>
      val (session, _) =
        sessionWithCompleteC285Claim(mrn, toTypeOfClaim(journeyBindable))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }
      val result = performAction(journeyBindable, enterMovementReferenceNumberKey -> mrn.value)

      status(result) shouldBe 303
      redirectLocation(
        result
      ).value        shouldBe routes.CheckYourAnswersAndSubmitController.checkAllAnswers(journeyBindable).url
    }

    "start a new claim if a different MRN is submitted" in {
      (
        journeyBindable: JourneyBindable,
        displayDeclaration: DisplayDeclaration,
        mrn: MRN
      ) =>
        val (session, _) =
          sessionWithCompleteC285Claim(mrn, toTypeOfClaim(journeyBindable))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
          mockStoreSession(Right(()))
        }

        val result = performAction(journeyBindable, enterMovementReferenceNumberKey -> genOtherThan(mrn).value)

        status(result)                 shouldBe 303
        redirectLocation(result).value shouldBe routes.EnterImporterEoriNumberController
          .enterImporterEoriNumber(journeyBindable)
          .url
    }
  }

  "Form validation" must {

    def form() =
      EnterMovementReferenceNumberController.movementReferenceNumberForm()

    "accept valid MRN" in forAll { mrn: MRN =>
      val errors =
        form().bind(Map(enterMovementReferenceNumberKey -> mrn.value)).errors
      errors shouldBe Nil
    }

    "reject 19 characters" in {
      val errors =
        form().bind(Map(enterMovementReferenceNumberKey -> "910ABCDEFGHIJKLMNO0")).errors
      errors.headOption.value.messages shouldBe List("invalid.number")
    }

    "reject 17 characters" in {
      val errors = form()
        .bind(Map(enterMovementReferenceNumberKey -> "123456789A1234567"))
        .errors
      errors.headOption.value.messages shouldBe List("invalid.number")
    }
  }

}
