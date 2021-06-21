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

import cats.syntax.all._
import play.api.mvc.{Result, Results}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectNumberOfClaimsController.SelectNumberOfClaimsType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, MovementReferenceNumber}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}

import scala.concurrent.Future

trait SessionDataExtractor extends Results {
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
  )(implicit extractor: DraftC285Claim => Option[T], request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)) =>
        val numOfClaims = getNumberOfClaims(draftClaim)
        val refType     = getMovementReferenceNumber(draftClaim)
        val router      = getRoutes(numOfClaims, refType)
        draftClaim
          .fold(extractor(_))
          .fold[Future[Result]](f(fillingOutClaim, None, router))(data => f(fillingOutClaim, Option(data), router))
      case _                                                                     =>
        Future.successful(Redirect(baseRoutes.StartController.start()))
    }

  def getNumberOfClaims(draftClaim: DraftClaim): SelectNumberOfClaimsType =
    draftClaim
      .fold(identity)
      .selectNumberOfClaimsAnswer
      .flatMap(
        _.fold(_.selectNumberOfClaimsChoice, a => Option(a.selectNumberOfClaimsChoice))
      )
      .getOrElse(
        SelectNumberOfClaimsType.Individual
      ) //If the bulk claim is disabled, the user never sees the Select Number of Claims page

  def getMovementReferenceNumber(draftClaim: DraftClaim): Option[MovementReferenceNumber] =
    draftClaim.fold(identity).movementReferenceNumber

  def getRoutes(
    numberOfClaims: SelectNumberOfClaimsType,
    maybeMrnOrEntryNmber: Option[MovementReferenceNumber]
  ): ReimbursementRoutes =
    maybeMrnOrEntryNmber match {
      case Some(mrnOrEntryNmber) =>
        (mrnOrEntryNmber.value, numberOfClaims) match {
          case (Right(_), SelectNumberOfClaimsType.Individual) => MRNSingleRoutes
          case (Left(_), SelectNumberOfClaimsType.Individual)  => EntrySingleRoutes
          case (Right(_), SelectNumberOfClaimsType.Bulk)       => MRNBulkRoutes
          case (Left(_), SelectNumberOfClaimsType.Bulk)        => EntryBulkRoutes
          case (Right(_), SelectNumberOfClaimsType.Scheduled)  => MRNScheduledRoutes
          case (Left(_), SelectNumberOfClaimsType.Scheduled)   => EntryScheduledRoutes
        }
      case None                  => DefaultRoutes
    }

}
