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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.cache

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import configs.syntax._
import play.api.Configuration
import play.modules.reactivemongo.ReactiveMongoComponent
import uk.gov.hmrc.cache.repository.CacheMongoRepository
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error, SessionData}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[DefaultSessionCache2])
trait SessionCache2 {

  def get()(implicit
    hc: HeaderCarrier
  ): OptionT[Future, Either[Error, SessionData]]

  def store(sessionData: SessionData)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

}

@Singleton
class DefaultSessionCache2 @Inject() (
  mongo: ReactiveMongoComponent,
  configuration: Configuration
)(implicit
  ec: ExecutionContext
) extends SessionCache2
    with Cache2 {

  val cacheRepository: CacheMongoRepository = {
    val expireAfter: FiniteDuration = configuration.underlying
      .get[FiniteDuration]("session-store.expiry-time")
      .value

    new CacheMongoRepository("sessions", expireAfter.toSeconds)(
      mongo.mongoConnector.db,
      ec
    )
  }

  val sessionKey = "cdsrc-session"

  override def get()(implicit hc: HeaderCarrier): OptionT[Future, Either[Error, SessionData]] =
    for {
      sessionId   <- OptionT.fromOption(hc.sessionId.map(_.value))
      sessionData <- get[SessionData](sessionId)
    } yield sessionData

  override def store(
    sessionData: SessionData
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    for {
      sessionId <- EitherT.fromOption(
                     hc.sessionId.map(_.value),
                     Error("no session id found in headers - cannot store data in mongo")
                   )
      _         <- store(sessionId, sessionData)
    } yield ()

}
