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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SupportingEvidenceAnswers.IncompleteSupportingEvidenceAnswers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftReturnGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.FileUploadGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{GGCredId, UUIDGenerator}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidence
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService

import scala.concurrent.Future

class SupportingEvidenceControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks {

  val mockUUIDGenerator = mock[UUIDGenerator]

  val mockUpscanService: UpscanService = mock[UpscanService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionStore),
      bind[UpscanService].toInstance(mockUpscanService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator)
    )

  lazy val controller = instanceOf[SupportingEvidenceController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def sessionWithClaimState(
    newSupportingEvidenceAnswers: Option[SupportingEvidenceAnswers]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {

    val _ = DraftC285Claim.newDraftC285Claim.copy(
      supportingEvidenceAnswers = newSupportingEvidenceAnswers
    )

    val ggCredId            = sample[GGCredId]
    val signedInUserDetails = sample[SignedInUserDetails]
    val draftClaim          = sample[DraftC285Claim]

    val journey = FillingOutClaim(
      ggCredId,
      signedInUserDetails,
      draftClaim
    )
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      draftClaim
    )
  }

  "Supporting Evidence Controller" when {

    "handling requests to upload supporting evidence" must {

      def performAction(): Future[Result] = controller.uploadSupportingEvidence()(FakeRequest())

      "show check your answers page" when {

        "the number of uploads have reached the maximum allowed" in {
          val supportingEvidence = sample[SupportingEvidence]

          val answers = IncompleteSupportingEvidenceAnswers(
            evidences = List.fill(2)(supportingEvidence)
          )

          val (session, _, _) = sessionWithClaimState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }
      }

    }

  }

}
