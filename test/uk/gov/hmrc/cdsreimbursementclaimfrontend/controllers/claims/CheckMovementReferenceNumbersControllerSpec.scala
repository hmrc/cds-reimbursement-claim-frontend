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
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckMovementReferenceNumbersController.checkMovementReferenceNumbersKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AssociatedMRNsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{ClaimService, FeatureSwitchService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.asScalaBufferConverter

class CheckMovementReferenceNumbersControllerSpec
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

  val controller: CheckMovementReferenceNumbersController = instanceOf[CheckMovementReferenceNumbersController]
  val featureSwitch: FeatureSwitchService                 = instanceOf[FeatureSwitchService]

  implicit val messagesApi: MessagesApi = controller.messagesApi
  implicit val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  val messageKey: String = "check-movement-reference-numbers"

  private def sessionWithClaimState(
    maybeAssociatedMRNsAnswer: Option[AssociatedMRNsAnswer],
    movementReferenceNumber: MovementReferenceNumber,
    numberOfClaims: Option[SelectNumberOfClaimsAnswer]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftC285Claim      = DraftC285Claim.newDraftC285Claim.copy(
      associatedMRNsAnswer = maybeAssociatedMRNsAnswer,
      movementReferenceNumber = Some(movementReferenceNumber),
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

  def isYesChecked(document: Document): Boolean = isChecked(document, s"$messageKey")

  def isNoChecked(document: Document): Boolean = isChecked(document, s"$messageKey-2")

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

      "there is no journey status in the session" in {
        val (session, _, _) = sessionWithClaimState(None, sample[MovementReferenceNumber], None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session.copy(journeyStatus = None))
        }

        checkIsRedirect(
          performAction(),
          baseRoutes.StartController.start()
        )
      }
    }

    "display the page title" in {
      val (session, _, _) =
        sessionWithClaimState(None, sample[MovementReferenceNumber], Some(SelectNumberOfClaimsAnswer.Multiple))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey(s"$messageKey.title")
      )
    }

    "display the page" when {

      "the user has not answered this question before" in {
        val (session, _, _) =
          sessionWithClaimState(None, sample[MovementReferenceNumber], Some(SelectNumberOfClaimsAnswer.Multiple))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messageKey.title"),
          doc => {
            isYesChecked(doc) shouldBe false
            isNoChecked(doc)  shouldBe false
          }
        )
      }

    }

    "show an error summary" when {

      "the user does not select an option and submits the page" in {
        val (session, _, _) =
          sessionWithClaimState(None, sample[MovementReferenceNumber], Some(SelectNumberOfClaimsAnswer.Multiple))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performActionWithData(Seq.empty),
          messageFromMessageKey(s"$messageKey.title"),
          getErrorSummary(_) shouldBe messageFromMessageKey(s"$messageKey.invalid-answer"),
          BAD_REQUEST
        )
      }
    }

    "delete an MRN" when {
      "the user selects the delete link next to an MRN" in {
        pending

        val mrns: List[MRN]     = List(sample(arbitraryMrn), sample(arbitraryMrn), sample(arbitraryMrn))
        val mrnDeleteIndex: Int = mrns.size - 1

        def performActionDelete(): Future[Result] = controller.deleteMrn(mrnDeleteIndex)(FakeRequest())
        val associatedMRNsAnswer                  = NonEmptyList.fromList(mrns)

        val (session, _, _) =
          sessionWithClaimState(
            associatedMRNsAnswer,
            sample[MovementReferenceNumber],
            Some(SelectNumberOfClaimsAnswer.Multiple)
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        performActionDelete()

      }
    }

    "redirect to the next MRN page" when {

      "the user selects yes" in {

        val mrns: List[MRN]      = List(sample(arbitraryMrn), sample(arbitraryMrn), sample(arbitraryMrn))
        val mrnForwardIndex: Int = mrns.size + 2
        val associatedMRNsAnswer = NonEmptyList.fromList(mrns)
        val (session, _, _)      =
          sessionWithClaimState(
            associatedMRNsAnswer,
            sample[MovementReferenceNumber],
            Some(SelectNumberOfClaimsAnswer.Multiple)
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performActionWithData(Seq(checkMovementReferenceNumbersKey -> true.toString)),
          routes.EnterAssociatedMrnController.enterMrn(mrnForwardIndex)
        )

      }
    }

    "redirect to the who is making this claim page" when {

      "the user selects no" in {
        val mrns: List[MRN]      = List(sample(arbitraryMrn), sample(arbitraryMrn), sample(arbitraryMrn))
        val associatedMRNsAnswer = NonEmptyList.fromList(mrns)
        val (session, _, _)      =
          sessionWithClaimState(
            associatedMRNsAnswer,
            sample[MovementReferenceNumber],
            Some(SelectNumberOfClaimsAnswer.Multiple)
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
