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

import cats.data.NonEmptyList
import org.jsoup.nodes.Document
import org.scalatest.OptionValues
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterAssociatedMrnController.enterAssociatedMrnKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AssociatedMRNsAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{ClaimService, FeatureSwitchService}

import scala.concurrent.Future

class EnterAssociatedMrnControllerSpec extends ControllerSpec with AuthSupport with SessionSupport with OptionValues {

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

  val messageKey: String   = "enter-associated-mrn"
  val mrns: List[MRN]      = List(sample(arbitraryMrn), sample(arbitraryMrn), sample(arbitraryMrn))
  val associatedMRNsAnswer = NonEmptyList.fromList(mrns)

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

  "EnterAssociatedMrnController" must {

    featureSwitch.BulkClaim.enable()
    val mrnIndex: Int                   = 4
    def performAction(): Future[Result] = controller.enterMrn(mrnIndex)(FakeRequest())

    "display the page title" in {

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

      checkPageIsDisplayed(
        performAction(),
        messageFromMessageKey(s"$messageKey.title", OrdinalNumeral(mrnIndex))
      )
    }

    "the change page" must {

      "display the title" in {

        val mrnIndexChange: Int             = 2
        def performAction(): Future[Result] = controller.changeMrn(mrnIndexChange)(FakeRequest())

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

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey(s"$messageKey.title", OrdinalNumeral(mrnIndexChange))
        )
      }
    }

    "We enter an MRN for the first time or update it with the back button (enterMrnSubmit)" must {
      def performActionWithData(index: Int, data: (String, String)*): Future[Result] =
        controller.submitEnteredMrn(index)(FakeRequest().withFormUrlEncodedBody(data: _*))

      def performActionWithDataSeq(index: Int, data: Seq[(String, String)]): Future[Result] =
        controller.submitEnteredMrn(index)(FakeRequest().withFormUrlEncodedBody(data: _*))

      "reject an invalid MRN" in {

        val invalidMRN = MRN("INVALID_MOVEMENT_REFERENCE_NUMBER")

        val (session, _, _) = sessionWithClaimState(
          associatedMRNsAnswer,
          sample[MovementReferenceNumber],
          Some(SelectNumberOfClaimsAnswer.Multiple)
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        val result = performActionWithData(mrnIndex, enterAssociatedMrnKey -> invalidMRN.value)

        checkPageIsDisplayed(
          result,
          messageFromMessageKey(s"$messageKey.title", OrdinalNumeral(mrnIndex)),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messageKey.invalid.number"),
          expectedStatus = 400
        )
      }

      "reject the same MRN as previously entered" in {

        val mrn                  = sample(arbitraryMrn)
        val mrnsList: List[MRN]  = List(sample(arbitraryMrn), mrn, sample(arbitraryMrn))
        val associatedMRNsAnswer = NonEmptyList.fromList(mrnsList)

        val (session, _, _) = sessionWithClaimState(
          associatedMRNsAnswer,
          sample[MovementReferenceNumber],
          Some(SelectNumberOfClaimsAnswer.Multiple)
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performActionWithData(mrnIndex, enterAssociatedMrnKey -> mrn.value),
          messageFromMessageKey(s"$messageKey.title", OrdinalNumeral(mrnIndex)),
          doc => getErrorSummary(doc) shouldBe messageFromMessageKey(s"$messageKey.error.exists"),
          expectedStatus = 400
        )
      }

      "the user does not select an option and submits the page" in {

        val (session, _, _) =
          sessionWithClaimState(None, sample[MovementReferenceNumber], Some(SelectNumberOfClaimsAnswer.Multiple))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performActionWithDataSeq(mrnIndex, Seq.empty),
          messageFromMessageKey(s"$messageKey.title", OrdinalNumeral(mrnIndex)),
          getErrorSummary(_) shouldBe messageFromMessageKey(s"$messageKey.error.required"),
          BAD_REQUEST
        )
      }

      "redirect to the MRN summary page" in {

        val mrnForwardIndex: Int = mrns.size + 1

        val (session, _, _) =
          sessionWithClaimState(
            associatedMRNsAnswer,
            sample[MovementReferenceNumber],
            Some(SelectNumberOfClaimsAnswer.Multiple)
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreSession(Right(()))
        }

        checkIsRedirect(
          performActionWithData(mrnForwardIndex, enterAssociatedMrnKey -> sample(arbitraryMrn).value),
          routes.CheckMovementReferenceNumbersController.showMrns()
        )

      }

    }

  }

  "Form validation" must {

    def form() = EnterAssociatedMrnController.mrnInputForm()

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
