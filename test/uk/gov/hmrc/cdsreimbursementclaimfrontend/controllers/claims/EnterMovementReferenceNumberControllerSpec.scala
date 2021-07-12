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
import org.jsoup.Jsoup
import org.scalatest.OptionValues
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.{enterMovementReferenceNumberKey, enterNoLegacyMrnKey}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectNumberOfClaimsController.SelectNumberOfClaimsType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SelectNumberOfClaimsAnswer.CompleteSelectNumberOfClaimsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.{ConsigneeDetails, DisplayDeclaration}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.{genOtherThan, sample}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{ClaimService, FeatureSwitchService}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectNumberOfClaimsController.SelectNumberOfClaimsType._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnterMovementReferenceNumberControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with TableDrivenPropertyChecks
    with OptionValues {

  val mockClaimsService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService)
    )

  val featureSwitch = instanceOf[FeatureSwitchService]
  val keys          = Table("Key", enterNoLegacyMrnKey, enterMovementReferenceNumberKey)

  def mockGetDisplayDeclaration(response: Either[Error, Option[DisplayDeclaration]]) =
    (mockClaimsService
      .getDisplayDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))

  lazy val controller: EnterMovementReferenceNumberController = instanceOf[EnterMovementReferenceNumberController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def sessionWithClaimState(
    maybeMovementReferenceNumberAnswer: Option[MovementReferenceNumber],
    numberOfClaims: Option[SelectNumberOfClaimsType]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      =
      DraftC285Claim.newDraftC285Claim.copy(
        movementReferenceNumber = maybeMovementReferenceNumberAnswer,
        selectNumberOfClaimsAnswer = numberOfClaims.map(CompleteSelectNumberOfClaimsAnswer(_))
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

  "Movement Reference Number Controller page titles" when {

    def runJourney(
      isEntryNumberFeatureEnabled: Boolean,
      journeyBindable: JourneyBindable,
      expectedTitle: String
    ): Unit = {
      isEntryNumberFeatureEnabled match {
        case true  => featureSwitch.EntryNumber.enable()
        case false => featureSwitch.EntryNumber.disable()
      }
      featureSwitch.BulkClaim.enable()

      val (session, _, _) = sessionWithClaimState(None, Some(Scheduled))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        controller.enterJourneyMrn(journeyBindable)(FakeRequest()),
        expectedTitle,
        doc => {
          doc.select(s"#$enterMovementReferenceNumberKey").`val`() shouldBe ""
          doc.select("form").attr("action")                        shouldBe routes.EnterMovementReferenceNumberController
            .enterMrnSubmit(journeyBindable)
            .url
        }
      )

    }

    "The entry number feature is enabled (Both MRN's and Entry Numbers allowed)" must {
      "show title on the sigle journey" in {
        runJourney(true, JourneyBindable.Single, "What is your Movement Reference Number (MRN)?")
      }
      "show title on the bulk journey" in {
        runJourney(true, JourneyBindable.Bulk, "Enter the lead Movement Reference Number (MRN)")
      }
      "show title on the scheduled journey" in {
        runJourney(true, JourneyBindable.Scheduled, "Enter the lead Movement Reference Number (MRN)")
      }
    }

    "The entry number feature is disabed (Only MRN's are allowed)" must {
      "show title on the sigle journey" in {
        runJourney(false, JourneyBindable.Single, "Enter the Movement Reference Number (MRN)")
      }
      "show title on the bulk journey" in {
        runJourney(false, JourneyBindable.Bulk, "Enter the lead Movement Reference Number (MRN)")
      }
      "show title on the scheduled journey" in {
        runJourney(false, JourneyBindable.Scheduled, "Enter the lead Movement Reference Number (MRN)")
      }
    }

  }

  "Movement Reference Number Controller Individual journey" when {

    "Enter MRN page" must {

      def performAction(journeyBindable: JourneyBindable): Future[Result] =
        controller.enterJourneyMrn(journeyBindable)(FakeRequest())

      "show the title for the single journey" in forAll(keys) { key =>
        featureSwitch.EntryNumber.setFlag(key != enterNoLegacyMrnKey)

        val (session, _, _) = sessionWithClaimState(None, Some(Individual))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val doc = Jsoup.parse(contentAsString(performAction(JourneyBindable.Single)))

        doc.select("h1").text                                      should include(messageFromMessageKey(s"$key.title"))
        doc.select(s"#$enterMovementReferenceNumberKey").`val`() shouldBe ""
        doc.select("form").attr("action")                        shouldBe routes.EnterMovementReferenceNumberController
          .enterMrnSubmit(JourneyBindable.Single)
          .url

      }

      "show the title for the scheduled journey" in {
        featureSwitch.EntryNumber.enable()
        featureSwitch.BulkClaim.enable()

        val (session, _, _) = sessionWithClaimState(None, Some(Scheduled))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(JourneyBindable.Scheduled),
          messageFromMessageKey(s"$enterMovementReferenceNumberKey.scheduled.title"),
          doc => {
            doc.select(s"#$enterMovementReferenceNumberKey").`val`() shouldBe ""
            doc.select("form").attr("action")                        shouldBe routes.EnterMovementReferenceNumberController
              .enterMrnSubmit(JourneyBindable.Scheduled)
              .url
          }
        )
      }

    }

    "Change MRN page" must {
      def performAction(): Future[Result] = controller.changeJourneyMrn(JourneyBindable.Single)(FakeRequest())

      "show the title and the MRN number" in forAll(keys) { key =>
        featureSwitch.EntryNumber.setFlag(key != enterNoLegacyMrnKey)

        val mrn             = sample[MRN]
        val mrnAnswer       = sampleMrnAnswer(mrn)
        val (session, _, _) = sessionWithClaimState(mrnAnswer, Some(Individual))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        val doc = Jsoup.parse(contentAsString(performAction()))

        doc.select("h1").text          should include(messageFromMessageKey(s"$key.title"))
        doc.select(s"#$key").`val`() shouldBe mrn.value
      }
    }

    "We enter an Entry/MRN for the first time or update it with the back button (enterMrnSubmit)" must {
      def performAction(journeyBindable: JourneyBindable, data: (String, String)*): Future[Result] =
        controller.enterMrnSubmit(journeyBindable)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "reject an invalid Entry Number/MRN" in {
        featureSwitch.EntryNumber.enable()

        val (session, _, _)    = sessionWithClaimState(None, Some(Individual))
        val invalidEntryNumber = EntryNumber("INVALID_ENTRY_NUMBER")

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result = performAction(JourneyBindable.Single, enterMovementReferenceNumberKey -> invalidEntryNumber.value)

        checkPageIsDisplayed(
          result,
          messageFromMessageKey(s"$enterMovementReferenceNumberKey.title"),
          _ => (),
          400
        )
      }

      "start an Entry Number claim, if the Bulk Claim feature is disabled and the Entry Number feature is enabled and an entry number is entered" in {
        featureSwitch.EntryNumber.enable()
        featureSwitch.BulkClaim.disable()

        val (session, _, _) = sessionWithClaimState(None, None)
        val entryNumber     = EntryNumber("123456789A12345678")

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(Right(()))
        }
        val result = performAction(JourneyBindable.Single, enterMovementReferenceNumberKey -> entryNumber.value)

        status(result) shouldBe 303
        checkIsRedirect(
          result,
          routes.EnterDeclarationDetailsController.enterDeclarationDetails()
        )
      }

      "start an Entry Number claim, if the Bulk Claim feature and the Entry Number feature are enabled and an entry number is entered" in {
        featureSwitch.EntryNumber.enable()
        featureSwitch.BulkClaim.enable()

        val (session, _, _) = sessionWithClaimState(None, Some(Individual))
        val entryNumber     = sample[EntryNumber]

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(Right(()))
        }
        val result = performAction(JourneyBindable.Single, enterMovementReferenceNumberKey -> entryNumber.value)

        status(result) shouldBe 303
        checkIsRedirect(
          result,
          routes.EnterDeclarationDetailsController.enterDeclarationDetails()
        )
      }

      "Update an Entry Number" in {
        featureSwitch.EntryNumber.enable()

        val entryNumber     = sample[EntryNumber]
        val answers         = sampleEntryNumberAnswer(entryNumber)
        val (session, _, _) = sessionWithClaimState(answers, Some(Individual))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(Right(()))
        }
        val result =
          performAction(JourneyBindable.Single, enterMovementReferenceNumberKey -> genOtherThan(entryNumber).value)

        status(result) shouldBe 303
        checkIsRedirect(
          result,
          routes.EnterDeclarationDetailsController.enterDeclarationDetails()
        )
      }

      "Update an Entry Number, but do not change it" in {
        featureSwitch.EntryNumber.enable()

        val originalEntryNumber = sample[EntryNumber]
        val entryNumberAnswer   = sampleEntryNumberAnswer(originalEntryNumber)
        val (session, _, _)     = sessionWithClaimState(entryNumberAnswer, Some(Individual))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result = performAction(JourneyBindable.Single, enterMovementReferenceNumberKey -> originalEntryNumber.value)

        status(result) shouldBe 303
        checkIsRedirect(
          result,
          routes.EnterDeclarationDetailsController.enterDeclarationDetails()
        )
      }

      "start a new claim with an MRN, Eori is importer's Eori" in forAll(keys) { key =>
        featureSwitch.EntryNumber.setFlag(key != enterNoLegacyMrnKey)

        val (session, foc, _) = sessionWithClaimState(None, Some(Individual))

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
        val result = performAction(JourneyBindable.Single, key -> sample[MRN].value)

        status(result) shouldBe 303
        checkIsRedirect(
          result,
          routes.CheckDeclarationDetailsController.checkDetails()
        )
      }

      "Update an MRN, Eori is importer's Eori" in forAll(keys) { key =>
        featureSwitch.EntryNumber.setFlag(key != enterNoLegacyMrnKey)

        val mrn               = sample[MRN]
        val mrnAnswer         = sampleMrnAnswer(mrn)
        val (session, foc, _) = sessionWithClaimState(mrnAnswer, Some(Individual))

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
        val result = performAction(JourneyBindable.Single, key -> genOtherThan(mrn).value)

        status(result) shouldBe 303
        checkIsRedirect(
          result,
          routes.CheckDeclarationDetailsController.checkDetails()
        )
      }

      "Update an MRN, but don't change it, Eori is importer's Eori" in forAll(keys) { key =>
        featureSwitch.EntryNumber.setFlag(key != enterNoLegacyMrnKey)

        val mrn               = sample[MRN]
        val mrnAnswer         = sampleMrnAnswer(mrn)
        val (session, foc, _) = sessionWithClaimState(mrnAnswer, Some(Individual))

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
        val result = performAction(JourneyBindable.Single, key -> mrn.value)

        status(result) shouldBe 303
        checkIsRedirect(
          result,
          routes.CheckDeclarationDetailsController.checkDetails()
        )
      }

      "start a new claim with an MRN, Eori is not the importer's Eori" in forAll(keys) { key =>
        featureSwitch.EntryNumber.setFlag(key != enterNoLegacyMrnKey)

        val (session, _, _) = sessionWithClaimState(None, Some(Individual))

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
        val result = performAction(JourneyBindable.Single, key -> sample[MRN].value)

        status(result) shouldBe 303
        checkIsRedirect(
          result,
          routes.EnterImporterEoriNumberController.enterImporterEoriNumber()
        )
      }

    }

    "We update an Entry/MRN coming from the Check Your Answer page (changeMrnSubmit)" must {

      def performAction(journeyBindable: JourneyBindable, data: (String, String)*): Future[Result] =
        controller.changeMrnSubmit(journeyBindable)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "return to CYA page if the same MRN is submitted" in forAll(keys) { key =>
        featureSwitch.EntryNumber.setFlag(key != enterNoLegacyMrnKey)

        val mrn             = sample[MRN]
        val answers         = sampleMrnAnswer(mrn)
        val (session, _, _) = sessionWithClaimState(answers, Some(Individual))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = performAction(JourneyBindable.Single, key -> mrn.value)

        status(result) shouldBe 303
        redirectLocation(
          result
        ).value        shouldBe routes.CheckYourAnswersAndSubmitController.checkAllAnswers().url
      }

      "return to CYA page if the same entry number is submitted" in {
        featureSwitch.EntryNumber.enable()

        val entryNumber     = sample[EntryNumber]
        val answers         = sampleEntryNumberAnswer(entryNumber)
        val (session, _, _) = sessionWithClaimState(answers, Some(Individual))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result          = performAction(JourneyBindable.Single, enterMovementReferenceNumberKey -> entryNumber.value)

        status(result) shouldBe 303
        redirectLocation(
          result
        ).value        shouldBe routes.CheckYourAnswersAndSubmitController.checkAllAnswers().url
      }

      "start a new claim if a different MRN is submitted" in forAll(keys) { key =>
        featureSwitch.EntryNumber.setFlag(key != enterNoLegacyMrnKey)

        val mrn             = sample[MRN]
        val answers         = sampleMrnAnswer(mrn)
        val (session, _, _) = sessionWithClaimState(answers, Some(Individual))

        val displayDeclaration = sample[DisplayDeclaration]

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
          mockStoreSession(Right(()))
        }
        val result = performAction(JourneyBindable.Single, key -> genOtherThan(mrn).value)

        status(result)                 shouldBe 303
        redirectLocation(result).value shouldBe routes.EnterImporterEoriNumberController.enterImporterEoriNumber().url
      }

      "start a new claim if a different entry number is submitted" in {
        featureSwitch.EntryNumber.enable()

        val entryNumber     = sample[EntryNumber]
        val answers         = sampleEntryNumberAnswer(entryNumber)
        val (session, _, _) = sessionWithClaimState(answers, Some(Individual))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(Right(()))
        }
        val result          =
          performAction(JourneyBindable.Single, enterMovementReferenceNumberKey -> genOtherThan(entryNumber).value)

        status(result) shouldBe 303
        redirectLocation(
          result
        ).value shouldBe routes.EnterDeclarationDetailsController.enterDeclarationDetails().url
      }

    }

    "Form validation" must {

      def form(key: String, isEntryNumberEnabled: Boolean) =
        EnterMovementReferenceNumberController.movementReferenceNumberForm(key, isEntryNumberEnabled)

      val keys = Table("Key", enterNoLegacyMrnKey, enterMovementReferenceNumberKey)

      "accept valid MRN" in forAll(keys) { key =>
        val errors = form(key, isEntryNumberEnabled = false).bind(Map(key -> sample[MRN].value)).errors
        errors shouldBe Nil
      }

      "accept valid Entry Number (Chief Number)" in {
        val errors = form(enterMovementReferenceNumberKey, isEntryNumberEnabled = true)
          .bind(Map(enterMovementReferenceNumberKey -> sample[EntryNumber].value))
          .errors
        errors shouldBe Nil
      }

      "reject 19 characters" in forAll(keys) { key =>
        val errors = form(key, isEntryNumberEnabled = false).bind(Map(key -> "910ABCDEFGHIJKLMNO0")).errors
        errors.headOption.value.messages shouldBe List("invalid.number")
      }

      "reject 17 characters" in {
        val errors = form(enterMovementReferenceNumberKey, isEntryNumberEnabled = true)
          .bind(Map(enterMovementReferenceNumberKey -> "123456789A1234567"))
          .errors
        errors.headOption.value.messages shouldBe List("invalid.number")
      }
    }
  }

  "Movement Reference Number Controller Scheduled journey" when {

    "Enter MRN page" must {

      def performAction(): Future[Result] = controller.enterJourneyMrn(JourneyBindable.Scheduled)(FakeRequest())

      "show the title" in {
        val (session, _, _) = sessionWithClaimState(None, Some(Scheduled))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$enterMovementReferenceNumberKey.scheduled.title"),
          doc => doc.select(s"#$enterMovementReferenceNumberKey").`val`() shouldBe ""
        )
      }
    }

    "enterMrnSubmit" must {

      def performAction(journeyBindable: JourneyBindable, data: (String, String)*): Future[Result] =
        controller.enterMrnSubmit(journeyBindable)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "reject an invalid Entry Number/MRN" in {
        featureSwitch.EntryNumber.enable()

        val (session, _, _) = sessionWithClaimState(None, Some(Scheduled))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result = performAction(
          JourneyBindable.Scheduled,
          enterMovementReferenceNumberKey -> EntryNumber("INVALID_ENTRY_NUMBER").value
        )

        checkPageIsDisplayed(
          result,
          messageFromMessageKey(s"$enterMovementReferenceNumberKey.scheduled.title"),
          _ => (),
          400
        )
      }

    }

  }

}
