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

import cats.data.EitherT
import cats.instances.future._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{AuthSupport, ControllerSpec, RedirectToStartBehaviour, SessionSupport}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SupportingEvidenceAnswers.CompleteSupportingEvidenceAnswers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.DraftReturnGen.draftC285ClaimGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JourneyStatusGen.fillingOutClaimGen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.UpscanGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.{UploadReference, UpscanUpload}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftC285Claim, Error, SessionData, SupportingEvidenceAnswers}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.UpscanService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SupportingEvidenceControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockUUIDGenerator = mock[UUIDGenerator]

  val mockUpscanService: UpscanService = mock[UpscanService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionCache].toInstance(mockSessionStore),
      //     bind[ClaimService].toInstance(mockClaimService),
      bind[UpscanService].toInstance(mockUpscanService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator)
    )

  lazy val controller = instanceOf[SupportingEvidenceController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def mockUpscanInitiate(
    errorRedirectCall: Call,
    successRedirectCall: UploadReference => Call
  )(
    result: Either[Error, UpscanUpload]
  ) =
    (mockUpscanService
      .initiate(_: Call, _: UploadReference => Call)(_: HeaderCarrier))
      .expects(
        where {
          (
            actualErrorRedirectCall: Call,
            actualSuccessRedirectCall: UploadReference => Call,
            _: HeaderCarrier
          ) =>
            val uploadReference = sample[UploadReference]
            actualErrorRedirectCall shouldBe errorRedirectCall
            actualSuccessRedirectCall(
              uploadReference
            )                       shouldBe successRedirectCall(uploadReference)
            true
        }
      )
      .returning(EitherT.fromEither(result))

  def mockGetUpscanUpload(uploadReference: UploadReference)(
    result: Either[Error, UpscanUpload]
  ) =
    (mockUpscanService
      .getUpscanUpload(_: UploadReference)(_: HeaderCarrier))
      .expects(uploadReference, *)
      .returning(EitherT.fromEither[Future](result))

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: FillingOutClaim => true
        case _                  => false
      }
    )

  def sessionWithClaimState(
    supportingEvidenceAnswers: Option[SupportingEvidenceAnswers]
  ): (SessionData, FillingOutClaim, DraftC285Claim) = {
    val draftClaim = sample[DraftC285Claim].copy(
      supportingEvidenceAnswers = supportingEvidenceAnswers
    )
    val journey    = sample[FillingOutClaim].copy(draftClaim = draftClaim)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      draftClaim
    )
  }

  def testFormError(
    data: (String, String)*
  )(
    expectedErrorMessageKey: String,
    errorArgs: Seq[String] = Nil
  )(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData = sessionWithClaimState(
      Some(sample[CompleteSupportingEvidenceAnswers])
    )._1
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(currentSession)
    }
    checkPageIsDisplayed(
      performAction(data),
      messageFromMessageKey(pageTitleKey, titleArgs: _*),
      { doc =>
        doc
          .select("#error-summary-display > ul > li > a")
          .text() shouldBe messageFromMessageKey(
          expectedErrorMessageKey,
          errorArgs: _*
        )
        doc.title() should startWith("Error:")
      },
      BAD_REQUEST
    )
  }

  "Supporting Evidence Controller" when {}

}
