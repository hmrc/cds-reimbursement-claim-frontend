/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.i18n.MessagesApi
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.ContactName
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

final case class RequestWithSessionDataAndRetrievedData[A](
  sessionData: SessionData,
  authenticatedRequest: AuthenticatedRequestWithRetrievedData[A]
) extends WrappedRequest[A](authenticatedRequest)
    with PreferredMessagesProvider {

  override def messagesApi: MessagesApi =
    authenticatedRequest.request.messagesApi

  val signedInUserDetails: Option[SignedInUserDetails] = sessionData match {
    case SessionData(journeyStatus @ Some(_), None, None, None, None, None, None) =>
      journeyStatus.collect {
        case JourneyStatus.FillingOutClaim(_, signedInUserDetails, _)       => signedInUserDetails
        case JourneyStatus.JustSubmittedClaim(_, signedInUserDetails, _, _) => signedInUserDetails
        case JourneyStatus.SubmitClaimFailed(_, signedInUserDetails)        => signedInUserDetails
      }
    case SessionData(None, Some(singleJourney), None, None, None, None, None)     =>
      Some(signedInUserDetailsFromRequest(singleJourney.getClaimantEori))
    case SessionData(None, None, Some(singleJourney), None, None, None, None)     =>
      Some(signedInUserDetailsFromRequest(singleJourney.getClaimantEori))
    case SessionData(None, None, None, Some(multipeJourney), None, None, None)    =>
      Some(signedInUserDetailsFromRequest(multipeJourney.getClaimantEori))
    case SessionData(None, None, None, None, Some(scheduledJourney), None, None)  =>
      Some(signedInUserDetailsFromRequest(scheduledJourney.getClaimantEori))
    case SessionData(None, None, None, None, None, Some(securitiesJourney), None) =>
      Some(signedInUserDetailsFromRequest(securitiesJourney.getClaimantEori))
    case _                                                                        => None
  }

  def signedInUserDetailsFromRequest(eori: Eori): SignedInUserDetails =
    SignedInUserDetails(
      authenticatedRequest.journeyUserType.email,
      eori,
      Email(""),
      ContactName(authenticatedRequest.journeyUserType.name.flatMap(_.name).getOrElse("No name"))
    )

  def using(
    matchExpression: PartialFunction[JourneyStatus, Future[Result]],
    applyIfNone: => Result = startNewJourney
  ): Future[Result] =
    sessionData.journeyStatus
      .collect(matchExpression)
      .getOrElse(Future.successful(applyIfNone))

  def startNewJourney: Result =
    Redirect(baseRoutes.StartController.start())
}

@Singleton
class SessionDataActionWithRetrievedData @Inject() (
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler
)(implicit
  val executionContext: ExecutionContext
) extends SessionDataActionBase[
      AuthenticatedRequestWithRetrievedData,
      RequestWithSessionDataAndRetrievedData
    ] {

  def sessionDataAction[A](
    sessionData: Option[SessionData],
    request: AuthenticatedRequestWithRetrievedData[A]
  ): RequestWithSessionDataAndRetrievedData[A] = {
    val data: SessionData =
      sessionData.getOrElse(SessionData.empty)
    RequestWithSessionDataAndRetrievedData(data, request)
  }

}
