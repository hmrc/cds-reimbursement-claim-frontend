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
import org.scalatest.BeforeAndAfterEach
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDuplicateMovementReferenceNumberController.duplicateMovementReferenceNumberKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, JourneyBindable, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SignedInUserDetailsGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DisplayDeclarationGen._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnterDuplicateMovementReferenceNumberControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with BeforeAndAfterEach
    with TableDrivenPropertyChecks {

  val claimService = mock[ClaimService]

  lazy val controller: EnterDuplicateMovementReferenceNumberController =
    instanceOf[EnterDuplicateMovementReferenceNumberController]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionCache),
      bind[ClaimService].toInstance(claimService)
    )

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi
  implicit lazy val messages: Messages       = MessagesImpl(Lang("en"), messagesApi)

  def mockGetDisplayDeclaration(response: Either[Error, Option[DisplayDeclaration]]) = (claimService
    .getDisplayDeclaration(_: MRN)(_: HeaderCarrier))
    .expects(*, *)
    .returning(EitherT.fromEither[Future](response))
    .once()

  "enterDuplicateMrn" should {
    def performAction(journey: JourneyBindable) = controller.enterDuplicateMrn(journey)(FakeRequest())

    "display the page" in {
      val journey      = JourneyBindable.Multiple
      val (session, _) = getSession(Some(TypeOfClaimAnswer.Multiple))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
      }

      checkPageIsDisplayed(
        performAction(journey),
        messageFromMessageKey("enter-duplicate-movement-reference-number.mrn.title")
      )
    }
  }

  "enterDuplicateMrnSubmit" should {
    def performAction(journey: JourneyBindable, data: (String, String)*) =
      controller.enterDuplicateMrnSubmit(journey)(FakeRequest().withFormUrlEncodedBody(data: _*))

    "update the duplicate MRN" in {
      val journey          = JourneyBindable.Multiple
      val updatedMrn       = sample[MRN]
      val (session, claim) = getSession(Some(TypeOfClaimAnswer.Multiple))

      val displayDeclaration = sample[DisplayDeclaration]
      val updatedClaim       =
        claim.draftClaim.copy(
          duplicateMovementReferenceNumberAnswer = Some(updatedMrn),
          duplicateDisplayDeclaration = Some(displayDeclaration)
        )
      val updatedSession     = session.copyWith(journeyStatus = Some(claim.copy(draftClaim = updatedClaim)))

      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(session)
        mockGetDisplayDeclaration(Right(Some(displayDeclaration)))
        mockStoreSession(updatedSession)(Right(()))
      }

      checkIsRedirect(
        performAction(journey, duplicateMovementReferenceNumberKey -> updatedMrn.value),
        routes.EnterImporterEoriNumberController.enterImporterEoriNumber(journey)
      )
    }
  }

  private def getSession(maybeTypeOfClaim: Option[TypeOfClaimAnswer]): (SessionData, FillingOutClaim) = {
    val mrn                 = sample[MRN]
    val draftC285Claim      = DraftClaim.blank.copy(
      movementReferenceNumber = Some(mrn),
      duplicateMovementReferenceNumberAnswer = Some(mrn),
      typeOfClaim = maybeTypeOfClaim
    )
    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val journey             = FillingOutClaim(ggCredId, signedInUserDetails, draftC285Claim)
    (SessionData.empty.copyWith(journeyStatus = Some(journey)), journey)
  }
}
