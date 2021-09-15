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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import play.api.mvc.{Result, Results}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, MovementReferenceNumber, SelectNumberOfClaimsAnswer}

import scala.concurrent.Future

trait SessionDataExtractor extends Results {

  def extractRoutes(claim: DraftClaim, journeyBindable: JourneyBindable): ReimbursementRoutes = {
    val numOfClaims = getNumberOfClaims(claim)
    val refType     = getMovementReferenceNumber(claim)
    getRoutes(numOfClaims, refType, journeyBindable)
  }

  def withAnswers[T](
    f: (FillingOutClaim, Option[T]) => Future[Result]
  )(implicit extractor: DraftC285Claim => Option[T], request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)) =>
        draftClaim
          .fold(extractor(_))
          .fold[Future[Result]](f(fillingOutClaim, None))(data => f(fillingOutClaim, Option(data)))
      case _                                                                     =>
        Future.successful(Redirect(routes.StartController.start()))
    }

  def withAnswersAndRoutes[T](
    f: (FillingOutClaim, Option[T], ReimbursementRoutes) => Future[Result]
  )(implicit
    extractor: DraftC285Claim => Option[T],
    request: RequestWithSessionData[_],
    journeyBindable: JourneyBindable
  ): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)) =>
        val router = extractRoutes(draftClaim, journeyBindable)
        draftClaim
          .fold(extractor(_))
          .fold[Future[Result]](f(fillingOutClaim, None, router))(data => f(fillingOutClaim, Option(data), router))
      case _                                                                     =>
        Future.successful(Redirect(baseRoutes.StartController.start()))
    }

  def getNumberOfClaims(draftClaim: DraftClaim): SelectNumberOfClaimsAnswer =
    draftClaim
      .fold(identity)
      .selectNumberOfClaimsAnswer
      .getOrElse(
        SelectNumberOfClaimsAnswer.Individual
      ) //If the bulk claim is disabled, the user never sees the Select Number of Claims page

  def getMovementReferenceNumber(draftClaim: DraftClaim): Option[MovementReferenceNumber] =
    draftClaim.fold(identity).movementReferenceNumber

  def getRoutes(
    numberOfClaims: SelectNumberOfClaimsAnswer,
    maybeMrnOrEntryNmber: Option[MovementReferenceNumber],
    journeyBindable: JourneyBindable
  ): ReimbursementRoutes =
    (journeyBindable, numberOfClaims, maybeMrnOrEntryNmber) match {
      case (JourneyBindable.Single, SelectNumberOfClaimsAnswer.Individual, Some(MovementReferenceNumber(Right(_))))   =>
        MRNSingleRoutes
      case (JourneyBindable.Single, SelectNumberOfClaimsAnswer.Individual, Some(MovementReferenceNumber(Left(_))))    =>
        EntrySingleRoutes
      case (JourneyBindable.Bulk, SelectNumberOfClaimsAnswer.Bulk, Some(MovementReferenceNumber(Right(_))))           =>
        MRNBulkRoutes
      case (JourneyBindable.Bulk, SelectNumberOfClaimsAnswer.Bulk, Some(MovementReferenceNumber(Left(_))))            =>
        EntryBulkRoutes
      case (JourneyBindable.Scheduled, SelectNumberOfClaimsAnswer.Scheduled, Some(MovementReferenceNumber(Right(_)))) =>
        MRNScheduledRoutes
      case (JourneyBindable.Scheduled, SelectNumberOfClaimsAnswer.Scheduled, Some(MovementReferenceNumber(Left(_))))  =>
        EntryScheduledRoutes
      case _                                                                                                          => JourneyNotDetectedRoutes
    }
}

//This method should be used in controllers, where we did not introduce the JourneyBindable yet
object JourneyExtractor extends SessionDataExtractor {

  def extractJourney(implicit request: RequestWithSessionData[_]): JourneyBindable =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(fillingOutClaim: FillingOutClaim) =>
        extractJourney(fillingOutClaim)
      case _                                      =>
        JourneyBindable.Single
    }

  def extractJourney(fillingOutClaim: FillingOutClaim): JourneyBindable =
    getNumberOfClaims(fillingOutClaim.draftClaim) match {
      case SelectNumberOfClaimsAnswer.Individual => JourneyBindable.Single
      case SelectNumberOfClaimsAnswer.Bulk       => JourneyBindable.Bulk
      case SelectNumberOfClaimsAnswer.Scheduled  => JourneyBindable.Scheduled
    }
}
