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
import cats.data.NonEmptyList
import cats.{Functor, Id}
import cats.implicits._
import org.jsoup.nodes.Document
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterAssociatedMrnController.enterAssociatedMrnKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayResponseDetailGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ConsigneeDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DeclarantDetails

class EnterAssociatedMrnControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckPropertyChecks
    with OptionValues {

  val mockClaimsService: ClaimService = mock[ClaimService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(mockClaimsService)
    )

  val controller: EnterAssociatedMrnController = instanceOf[EnterAssociatedMrnController]
  val featureSwitch: FeatureSwitchService      = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2)

  private def sessionWithClaimState(
    associatedMrns: List[MRN],
    movementReferenceNumber: MovementReferenceNumber,
    numberOfClaims: Option[SelectNumberOfClaimsAnswer],
    associatedDeclarations: List[DisplayDeclaration] = Nil,
    eori: Option[Eori] = None
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      = DraftC285Claim.newDraftC285Claim.copy(
      displayDeclaration = associatedDeclarations.headOption,
      associatedMRNsAnswer = NonEmptyList.fromList(associatedMrns),
      movementReferenceNumber = Some(movementReferenceNumber),
      selectNumberOfClaimsAnswer = numberOfClaims,
      associatedMRNsDeclarationAnswer = NonEmptyList.fromList(associatedDeclarations)
    )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails =
      eori
        .map(e => sample[SignedInUserDetails].copy(eori = e))
        .getOrElse(sample[SignedInUserDetails])
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

  def mockGetDisplayDeclaration(response: Either[Error, Option[DisplayDeclaration]]) =
    (mockClaimsService
      .getDisplayDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))

  val indexWithMrnGenerator: Gen[(AssociatedMrnIndex, List[MRN])] =
    Gen
      .nonEmptyListOf(genMRN)
      .flatMap(list =>
        Gen
          .chooseNum(0, list.length - 1)
          .map(index => (AssociatedMrnIndex.fromListIndex(index), list))
      )

  "EnterAssociatedMrnController" must {

    featureSwitch.BulkClaim.enable()

    def performAction(mrnIndex: AssociatedMrnIndex): Future[Result] =
      controller.enterMrn(mrnIndex)(FakeRequest())

    "display the enter page" in {
      forAll(genMovementReferenceNumber, Gen.nonEmptyListOf(genMRN)) { (reference, mrns) =>
        val (session, _, _) =
          sessionWithClaimState(
            mrns,
            reference,
            Some(SelectNumberOfClaimsAnswer.Multiple)
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(AssociatedMrnIndex.fromUrlIndex(mrns.size + 1)),
          messageFromMessageKey(s"$enterAssociatedMrnKey.title", OrdinalNumeral(mrns.size + 1))
        )
      }

    }

    "display the change page" in {
      forAll(genMovementReferenceNumber, indexWithMrnGenerator) {
        (mrn: MovementReferenceNumber, indexWithMrns: (AssociatedMrnIndex, List[MRN])) =>
          val associatedMrnIndex = indexWithMrns._1

          def performAction(): Future[Result] =
            controller.changeMrn(associatedMrnIndex)(FakeRequest())

          val (session, _, _) =
            sessionWithClaimState(
              indexWithMrns._2,
              mrn,
              Some(SelectNumberOfClaimsAnswer.Multiple)
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$enterAssociatedMrnKey.title", OrdinalNumeral(associatedMrnIndex.toUrlIndex))
          )
      }

    }

    "we enter an MRN for the first time or update it with the back button (enterMrnSubmit)" must {

      def performActionWithData(index: AssociatedMrnIndex, data: (String, String)*): Future[Result] =
        controller.submitEnteredMrn(index)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      def performActionWithDataSeq(index: AssociatedMrnIndex, data: Seq[(String, String)]): Future[Result] =
        controller.submitEnteredMrn(index)(
          FakeRequest().withFormUrlEncodedBody(data: _*)
        )

      "reject an invalid MRN" in {
        forAll(Gen.nonEmptyListOf(genMRN), genMovementReferenceNumber) { (mrns, reference) =>
          val invalidMRN = MRN("INVALID_MOVEMENT_REFERENCE_NUMBER")

          val mrnIndex           = mrns.size + 1
          val associatedMrnIndex = AssociatedMrnIndex.fromListIndex(mrnIndex)

          val (session, _, _) = sessionWithClaimState(
            mrns,
            reference,
            Some(SelectNumberOfClaimsAnswer.Multiple)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          val result = performActionWithData(associatedMrnIndex, enterAssociatedMrnKey -> invalidMRN.value)

          checkPageIsDisplayed(
            result,
            messageFromMessageKey(s"$enterAssociatedMrnKey.title", OrdinalNumeral(associatedMrnIndex.toUrlIndex)),
            doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$enterAssociatedMrnKey.invalid.number"),
            expectedStatus = 400
          )
        }
      }

      "accept the same MRN when changing an existing" in {
        forAll { (movementReferenceNumber: MovementReferenceNumber, mrn: MRN, mrns: List[MRN]) =>
          val associatedMRNsAnswer = mrn +: mrns

          val displayDeclaration = sample[DisplayDeclaration]

          val associatedDeclarations = associatedMRNsAnswer
            .map(_ => displayDeclaration)

          val (session, _, _) = sessionWithClaimState(
            associatedMRNsAnswer,
            movementReferenceNumber,
            Some(SelectNumberOfClaimsAnswer.Multiple),
            associatedDeclarations
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performActionWithData(
              AssociatedMrnIndex.fromListIndex(0),
              enterAssociatedMrnKey -> mrn.value
            ),
            routes.CheckMovementReferenceNumbersController.showMrns()
          )
        }
      }

      "reject the same MRN when entering new one" in {
        forAll { (movementReferenceNumber: MovementReferenceNumber, mrn: MRN, mrns: List[MRN]) =>
          val associatedMRNsAnswer = mrn +: mrns

          val (session, _, _) = sessionWithClaimState(
            associatedMRNsAnswer,
            movementReferenceNumber,
            Some(SelectNumberOfClaimsAnswer.Multiple)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performActionWithData(
              AssociatedMrnIndex.fromListIndex(associatedMRNsAnswer.length),
              enterAssociatedMrnKey -> mrn.value
            ),
            messageFromMessageKey(s"$enterAssociatedMrnKey.title", OrdinalNumeral(associatedMRNsAnswer.length + 2)),
            doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$enterAssociatedMrnKey.error.exists"),
            expectedStatus = 400
          )
        }
      }

      "the user does not select an option and submits the page" in {
        forAll(Gen.choose(0, 9), arbitraryMovementReferenceNumber.arbitrary) { (mrnIndex, reference) =>
          val associatedMrnIndex = AssociatedMrnIndex.fromListIndex(mrnIndex)

          val (session, _, _) =
            sessionWithClaimState(Nil, reference, Some(SelectNumberOfClaimsAnswer.Multiple))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performActionWithDataSeq(associatedMrnIndex, Seq.empty),
            messageFromMessageKey(s"$enterAssociatedMrnKey.title", OrdinalNumeral(associatedMrnIndex.toUrlIndex)),
            getErrorSummary(_) shouldBe messageFromMessageKey(s"$enterAssociatedMrnKey.error.required"),
            BAD_REQUEST
          )
        }
      }

      "redirect to the MRN summary page" in {
        forAll(genMovementReferenceNumber, Gen.nonEmptyListOf(genMRN), genMRN) { (reference, mrns, mrn) =>
          val mrnForwardIndex: Int = mrns.size
          val associatedMrnIndex   = AssociatedMrnIndex.fromListIndex(mrnForwardIndex)
          val eori: Eori           = sample[Eori]
          val declarantDetails     = sample[DeclarantDetails].copy(declarantEORI = eori.value)
          val consigneeDetails     = sample[ConsigneeDetails].copy(consigneeEORI = eori.value)
          val displayDeclaration   = Functor[Id].map(sample[DisplayDeclaration])(dd =>
            dd.copy(displayResponseDetail =
              dd.displayResponseDetail
                .copy(consigneeDetails = Some(consigneeDetails), declarantDetails = declarantDetails)
            )
          )

          val (session, _, _) =
            sessionWithClaimState(
              mrns,
              reference,
              Some(SelectNumberOfClaimsAnswer.Multiple),
              associatedDeclarations = mrns.map(_ => displayDeclaration),
              eori = Some(eori)
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
            mockStoreSession(Right(()))
          }

          checkIsRedirect(
            performActionWithData(associatedMrnIndex, enterAssociatedMrnKey -> mrn.value),
            routes.CheckMovementReferenceNumbersController.showMrns()
          )
        }
      }
    }
  }

  "Form validation" must {

    def form() = controller.mrnInputForm()

    "accept valid MRN" in {
      val errors =
        form().bind(Map(enterAssociatedMrnKey -> sample(arbitraryMrn).value)).errors
      errors shouldBe Nil
    }

    "reject 19 characters" in {
      val errors =
        form().bind(Map(enterAssociatedMrnKey -> "910ABCDEFGHIJKLMNO0")).errors
      errors.headOption.value.messages shouldBe List("invalid.number")
    }

    "reject 17 characters" in {
      val errors = form()
        .bind(Map(enterAssociatedMrnKey -> "123456789A1234567"))
        .errors
      errors.headOption.value.messages shouldBe List("invalid.number")
    }
  }

}

object EnterAssociatedMrnControllerSpec {

  implicit val genMrnsWithRandomIndex: Arbitrary[(AssociatedMrnIndex, List[MRN])] = Arbitrary {
    for {
      index <- Gen.choose(1, 10)
      mrns  <- Gen.listOfN(index + 1, genMRN)
    } yield (AssociatedMrnIndex.fromListIndex(index), mrns)
  }
}
