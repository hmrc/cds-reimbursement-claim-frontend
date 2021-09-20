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

import cats.data.EitherT
import cats.implicits._
import cats.{Functor, Id}
import org.jsoup.nodes.Document
import org.scalatest.OptionValues
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.enterMovementReferenceNumberKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, JourneyExtractor, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{ConsigneeDetails, DisplayDeclaration}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{genOtherThan, sample}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyBindableGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{ClaimService, FeatureSwitchService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnterMovementReferenceNumberControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with OptionValues {

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
  val featureSwitch: FeatureSwitchService                = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeMovementReferenceNumberAnswer: Option[MovementReferenceNumber],
    numberOfClaims: Option[SelectNumberOfClaimsAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(
        movementReferenceNumber = maybeMovementReferenceNumberAnswer,
        selectNumberOfClaimsAnswer = numberOfClaims
      )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      draftC285Claim
    )
  }

  def getErrorSummary(document: Document): String =
    document.select(".govuk-error-summary__list > li > a").text()

  "Movement Reference Number Controller page titles" when {

    def runJourney(
      journeyBindable: JourneyBindable,
      selectNumberOfClaimsAnswer: SelectNumberOfClaimsAnswer,
      expectedTitle: String
    ) = {
      featureSwitch.BulkClaim.enable()

      val (session, _, _) = sessionWithClaimState(None, Some(selectNumberOfClaimsAnswer))

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
          SelectNumberOfClaimsAnswer.Individual,
          s"$enterMovementReferenceNumberKey.title"
        )
      }
      "show title on the Multiple journey" in {
        runJourney(
          JourneyBindable.Multiple,
          SelectNumberOfClaimsAnswer.Multiple,
          s"$enterMovementReferenceNumberKey.multiple.title"
        )
      }
      "show title on the Scheduled journey" in {
        runJourney(
          JourneyBindable.Scheduled,
          SelectNumberOfClaimsAnswer.Scheduled,
          s"$enterMovementReferenceNumberKey.scheduled.title"
        )
      }
    }
  }

  "Movement Reference Number Controller" when {

    "Enter MRN page" must {

      def performAction(journeyBindable: JourneyBindable): Future[Result] =
        controller.enterJourneyMrn(journeyBindable)(FakeRequest())

      "display the title and the previously saved MRN" in {
        val journeyBindable    = sample[JourneyBindable]
        val mrn                = sample[MRN]
        val mrnAnswer          = MovementReferenceNumber(mrn).some
        val numberOfClaimsType = toSelectNumberOfClaims(journeyBindable)
        val router             = JourneyExtractor.getRoutes(numberOfClaimsType, mrnAnswer, journeyBindable)

        val (session, _, _) = sessionWithClaimState(mrnAnswer, Some(numberOfClaimsType))

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
            doc.select(s"#$enterMovementReferenceNumberKey").`val`() shouldBe mrn.value
            doc.select("form").attr("action")                        shouldBe
              routes.EnterMovementReferenceNumberController.enterMrnSubmit(journeyBindable).url
          }
        )
      }

    }

    "Change MRN page" must {
      def performAction(journeyBindable: JourneyBindable): Future[Result] =
        controller.changeJourneyMrn(journeyBindable)(FakeRequest())

      "display the title and the previously saved MRN" in {
        val journeyBindable    = sample[JourneyBindable]
        val mrn                = sample[MRN]
        val mrnAnswer          = MovementReferenceNumber(mrn).some
        val numberOfClaimsType = toSelectNumberOfClaims(journeyBindable)
        val router             = JourneyExtractor.getRoutes(numberOfClaimsType, mrnAnswer, journeyBindable)

        val (session, _, _) = sessionWithClaimState(mrnAnswer, Some(numberOfClaimsType))

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
            doc.select(s"#$enterMovementReferenceNumberKey").`val`() shouldBe mrn.value
            doc.select("form").attr("action")                        shouldBe
              routes.EnterMovementReferenceNumberController.changeMrnSubmit(journeyBindable).url
          }
        )
      }
    }

    "We enter an MRN for the first time or update it with the back button (enterMrnSubmit)" must {

      def performAction(journeyBindable: JourneyBindable, data: (String, String)*): Future[Result] =
        controller.enterMrnSubmit(journeyBindable)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "reject an invalid MRN" in {
        val journeyBindable    = sample[JourneyBindable]
        val invalidMRN         = MRN("INVALID_MOVEMENT_REFERENCE_NUMBER")
        val invalidMRNAnswer   = MovementReferenceNumber(invalidMRN).some
        val numberOfClaimsType = toSelectNumberOfClaims(journeyBindable)
        val router             = JourneyExtractor.getRoutes(numberOfClaimsType, invalidMRNAnswer, journeyBindable)

        val (session, _, _) = sessionWithClaimState(None, Some(toSelectNumberOfClaims(journeyBindable)))

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

      "start a new claim with an MRN, Eori is importer's Eori" in {
        val journeyBindable   = sample[JourneyBindable]
        val (session, foc, _) =
          sessionWithClaimState(None, Some(toSelectNumberOfClaims(journeyBindable)))

        val consigneeDetails   = sample[ConsigneeDetails].copy(consigneeEORI = foc.signedInUserDetails.eori.value)
        val displayDeclaration = Functor[Id].map(sample[DisplayDeclaration])(dd =>
          dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
          mockStoreSession(Right(()))
        }
        val result = performAction(journeyBindable, enterMovementReferenceNumberKey -> sample[MRN].value)

        status(result) shouldBe 303
        checkIsRedirect(
          result,
          routes.CheckDeclarationDetailsController.show(journeyBindable)
        )
      }

      "Update an MRN, Eori is importer's Eori" in {
        val journeyBindable   = sample[JourneyBindable]
        val mrn               = sample[MRN]
        val mrnAnswer         = MovementReferenceNumber(mrn).some
        val (session, foc, _) =
          sessionWithClaimState(mrnAnswer, Some(toSelectNumberOfClaims(journeyBindable)))

        val consigneeDetails   = sample[ConsigneeDetails].copy(consigneeEORI = foc.signedInUserDetails.eori.value)
        val displayDeclaration = Functor[Id].map(sample[DisplayDeclaration])(dd =>
          dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
          mockStoreSession(Right(()))
        }
        val result = performAction(journeyBindable, enterMovementReferenceNumberKey -> genOtherThan(mrn).value)

        status(result) shouldBe 303
        checkIsRedirect(
          result,
          routes.CheckDeclarationDetailsController.show(journeyBindable)
        )
      }

      "On submitting the same MRN as before, don't save it, Eori is importer's Eori" in {
        val journeyBindable   = sample[JourneyBindable]
        val mrn               = sample[MRN]
        val mrnAnswer         = MovementReferenceNumber(mrn).some
        val (session, foc, _) = sessionWithClaimState(mrnAnswer, Some(toSelectNumberOfClaims(journeyBindable)))

        val consigneeDetails   = sample[ConsigneeDetails].copy(consigneeEORI = foc.signedInUserDetails.eori.value)
        val displayDeclaration = Functor[Id].map(sample[DisplayDeclaration])(dd =>
          dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
          mockStoreSession(Right(()))
        }
        val result = performAction(journeyBindable, enterMovementReferenceNumberKey -> mrn.value)

        status(result) shouldBe 303
        checkIsRedirect(
          result,
          routes.CheckDeclarationDetailsController.show(journeyBindable)
        )
      }

      "start a new claim with an MRN, Eori is not the importer's Eori" in {
        val journeyBindable = sample[JourneyBindable]
        val (session, _, _) = sessionWithClaimState(None, Some(toSelectNumberOfClaims(journeyBindable)))

        val consigneeDetails   = sample[ConsigneeDetails].copy(consigneeEORI = sample[Eori].value)
        val displayDeclaration = Functor[Id].map(sample[DisplayDeclaration])(dd =>
          dd.copy(displayResponseDetail = dd.displayResponseDetail.copy(consigneeDetails = Some(consigneeDetails)))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
          mockStoreSession(Right(()))
        }
        val result = performAction(journeyBindable, enterMovementReferenceNumberKey -> sample[MRN].value)

        status(result) shouldBe 303
        checkIsRedirect(
          result,
          routes.EnterImporterEoriNumberController.enterImporterEoriNumber()
        )
      }

    }

    "We update an MRN coming from the Check Your Answer page (changeMrnSubmit)" must {

      def performAction(journeyBindable: JourneyBindable, data: (String, String)*): Future[Result] =
        controller.changeMrnSubmit(journeyBindable)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "return to CYA page if the same MRN is submitted" in {
        val journeyBindable = sample[JourneyBindable]
        val mrn             = sample[MRN]
        val answers         = MovementReferenceNumber(mrn).some
        val (session, _, _) = sessionWithClaimState(answers, Some(toSelectNumberOfClaims(journeyBindable)))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = performAction(journeyBindable, enterMovementReferenceNumberKey -> mrn.value)

        status(result) shouldBe 303
        redirectLocation(
          result
        ).value        shouldBe routes.CheckYourAnswersAndSubmitController.checkAllAnswers(journeyBindable).url
      }

      "start a new claim if a different MRN is submitted" in {
        val journeyBindable = sample[JourneyBindable]
        val mrn             = sample[MRN]
        val answers         = MovementReferenceNumber(mrn).some
        val (session, _, _) =
          sessionWithClaimState(answers, Some(toSelectNumberOfClaims(journeyBindable)))

        val displayDeclaration = sample[DisplayDeclaration]

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
          mockStoreSession(Right(()))
        }

        val result = performAction(journeyBindable, enterMovementReferenceNumberKey -> genOtherThan(mrn).value)

        status(result)                 shouldBe 303
        redirectLocation(result).value shouldBe routes.EnterImporterEoriNumberController.enterImporterEoriNumber().url
      }
    }

    "Form validation" must {

      def form() =
        EnterMovementReferenceNumberController.movementReferenceNumberForm()

      "accept valid MRN" in {
        val errors =
          form().bind(Map(enterMovementReferenceNumberKey -> sample[MRN].value)).errors
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

}
