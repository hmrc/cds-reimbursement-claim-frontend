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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass

import cats.syntax.all._
import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache2
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.typeclass.model.ClaimType
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[DefaultJourneyService])
trait JourneyService {

  def getJourney(implicit hc: HeaderCarrier, ec: ExecutionContext): EitherT[Future, Error, ClaimType]

  def persist(session: SessionData)(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit]
}

@Singleton
class DefaultJourneyService @Inject() (cache: SessionCache2) extends JourneyService {

  def getJourney(implicit hc: HeaderCarrier, ec: ExecutionContext): EitherT[Future, Error, ClaimType] = EitherT {
    cache
      .get()
      .fold(Error("No session found").asLeft[ClaimType])(_.flatMap(_.journeyStatus match {
        case Some(FillingOutClaim(_, _, _, claimType)) => claimType.asRight[Error]
        case _                                         => Error("Could not extract journey").asLeft[ClaimType]
      }))
  }

  def persist(session: SessionData)(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    cache.store(session)
}
