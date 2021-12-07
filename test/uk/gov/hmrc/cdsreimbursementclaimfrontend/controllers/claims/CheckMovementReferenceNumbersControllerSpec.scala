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

import cats.data.{EitherT, NonEmptyList}
import cats.implicits._
import org.scalatest.OptionValues
import org.jsoup.nodes.Document
import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckMovementReferenceNumbersController.checkMovementReferenceNumbersKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, JourneyBindable, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{AssociatedMrnIndex, GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{ClaimService, FeatureSwitchService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.asScalaBufferConverter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckMovementReferenceNumbersControllerSpec.genMrnsWithRandomIndex
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer

class CheckMovementReferenceNumbersControllerSpec
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

  def mockGetDisplayDeclaration(response: Either[Error, Option[DisplayDeclaration]]) =
    (mockClaimsService
      .getDisplayDeclaration(_: MRN)(_: HeaderCarrier))
      .expects(*, *)
      .returning(EitherT.fromEither[Future](response))

  val controller: CheckMovementReferenceNumbersController = instanceOf[CheckMovementReferenceNumbersController]
  val featureSwitch: FeatureSwitchService                 = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2)

  private def sessionWithClaimState(
    associatedMRNsAnswer: List[MRN],
    movementReferenceNumber: MRN,
    maybeTypeOfClaim: Option[TypeOfClaimAnswer]
  ): (SessionData, FillingOutClaim, DraftClaim) = {
    val draftC285Claim      = DraftClaim.blank.copy(
      associatedMRNsAnswer = NonEmptyList.fromList(associatedMRNsAnswer),
      movementReferenceNumber = Some(movementReferenceNumber),
      typeOfClaim = maybeTypeOfClaim
    )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (
      SessionData.empty.copyWith(
        journeyStatus = Some(journey)
      ),
      journey,
      draftC285Claim
    )
  }

  def getErrorSummary(document: Document): String =
    document.select(".govuk-error-summary__list > li > a").text()

  def isYesChecked(document: Document): Boolean = isChecked(document, s"$checkMovementReferenceNumbersKey")

  def isNoChecked(document: Document): Boolean = isChecked(document, s"$checkMovementReferenceNumbersKey-2")

  def isChecked(document: Document, fieldId: String): Boolean =
    document
      .getElementById(fieldId)
      .attributes()
      .asList()
      .asScala
      .map(_.getKey)
      .contains("checked")

  "CheckMovementReferenceNumbersController" must {
    def performAction(): Future[Result] = controller.showMrns()(FakeRequest())

    def performActionWithData(data: Seq[(String, String)]): Future[Result] =
      controller.submitMrns()(FakeRequest().withFormUrlEncodedBody(data: _*))

    featureSwitch.BulkClaim.enable()

    "redirect to the start of the journey" when {

      "there is no journey status in the session" in forAll { reference: MRN =>
        val (session, _, _) = sessionWithClaimState(Nil, reference, None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copyWith(journeyStatus = None))
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.StartController.start()
        )
      }
    }

    "display the page title" in forAll { reference: MRN =>
      val (session, _, _) =
        sessionWithClaimState(Nil, reference, Some(TypeOfClaimAnswer.Multiple))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey(s"$checkMovementReferenceNumbersKey.title")
      )
    }

    "display the page" when {

      "the user has not answered this question before" in forAll { reference: MRN =>
        val (session, _, _) =
          sessionWithClaimState(Nil, reference, Some(TypeOfClaimAnswer.Multiple))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$checkMovementReferenceNumbersKey.title"),
          doc => {
            isYesChecked(doc) shouldBe false
            isNoChecked(doc)  shouldBe false
          }
        )
      }

    }

    "show an error summary" when {

      "the user does not select an option and submits the page" in forAll { reference: MRN =>
        val (session, _, _) =
          sessionWithClaimState(Nil, reference, Some(TypeOfClaimAnswer.Multiple))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performActionWithData(Seq.empty),
          messageFromMessageKey(s"$checkMovementReferenceNumbersKey.title"),
          getErrorSummary(_) shouldBe messageFromMessageKey(s"$checkMovementReferenceNumbersKey.error.invalid"),
          BAD_REQUEST
        )
      }
    }

    "delete an MRN" when {
      "the user selects the delete link next to an MRN" in {
        forAll { (leadMrn: MRN, indexWithMrns: (AssociatedMrnIndex, List[MRN])) =>
          def performActionDelete(mrnDeleteIndex: AssociatedMrnIndex): Future[Result] =
            controller.deleteMrn(mrnDeleteIndex)(FakeRequest())

          val index = indexWithMrns._1.toListIndex
          val mrns  = indexWithMrns._2

          val (session, fillingOutClaim, draftClaim) =
            sessionWithClaimState(
              mrns,
              leadMrn,
              Some(TypeOfClaimAnswer.Multiple)
            )

          val updatedDraftClaim = draftClaim.copy(
            associatedMRNsAnswer = NonEmptyList.fromList(mrns.take(index) ++ mrns.drop(index + 1))
          )

          val expectedSessionWithDeletedMrn =
            session.copyWith(journeyStatus = Some(fillingOutClaim.copy(draftClaim = updatedDraftClaim)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(expectedSessionWithDeletedMrn)(Right(()))
          }

          val result = performActionDelete(indexWithMrns._1)

          status(result) should be(303)
        }
      }
    }

    "redirect to the next MRN page" when {

      "the user selects yes" in {
        forAll(Gen.nonEmptyListOf(genMRN), genMRN) { (mrns, leadMrn) =>
          val mrnForwardIndex: Int = mrns.size

          val (session, _, _) =
            sessionWithClaimState(
              mrns,
              leadMrn,
              Some(TypeOfClaimAnswer.Multiple)
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performActionWithData(Seq(checkMovementReferenceNumbersKey -> true.toString)),
            routes.EnterAssociatedMrnController.enterMrn(AssociatedMrnIndex.fromListIndex(mrnForwardIndex))
          )
        }
      }
    }

    "redirect to the who is making this claim page" when {

      "the user selects no" in {
        forAll(Gen.nonEmptyListOf(genMRN), genMRN) { (mrns, leadMrn) =>
          val (session, _, _) =
            sessionWithClaimState(
              mrns,
              leadMrn,
              Some(TypeOfClaimAnswer.Multiple)
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performActionWithData(Seq(checkMovementReferenceNumbersKey -> false.toString)),
            routes.SelectWhoIsMakingTheClaimController.selectDeclarantType(JourneyBindable.Multiple)
          )

        }
      }
    }
  }
}

object CheckMovementReferenceNumbersControllerSpec {

  implicit val genMrnsWithRandomIndex: Arbitrary[(AssociatedMrnIndex, List[MRN])] = Arbitrary {
    for {
      index <- Gen.choose(3, 10)
      mrns  <- Gen.listOfN(index + 1, genMRN)
    } yield (AssociatedMrnIndex.fromListIndex(index), mrns)
  }
}
