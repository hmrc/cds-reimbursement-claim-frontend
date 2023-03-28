/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.syntax.eq._
import com.google.inject.ImplementedBy
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.Configuration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.cache.CacheIdType
import uk.gov.hmrc.mongo.cache.DataKey
import uk.gov.hmrc.mongo.cache.MongoCacheRepository
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.TimestampSupport

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[DefaultSessionCache])
trait SessionCache {

  def get()(implicit
    hc: HeaderCarrier
  ): Future[Either[Error, Option[SessionData]]]

  def store(sessionData: SessionData)(implicit
    hc: HeaderCarrier
  ): Future[Either[Error, Unit]]

  final def update(modify: SessionData => SessionData)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Either[Error, Unit]] =
    try get().flatMap {
      case Right(Some(value)) =>
        val newSessionData = modify(value)
        if (newSessionData =!= value)
          store(newSessionData)
        else
          Future.successful(Right(()))

      case Right(None) =>
        Future.successful(
          Left(Error("no session found in mongo"))
        )

      case Left(error) =>
        Future.successful(Left(error))
    } catch {
      case e: Exception => Future.successful(Left(Error(e)))
    }

  final def updateF[R](
    update: SessionData => Future[Either[Error, SessionData]]
  )(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Either[Error, SessionData]] =
    try get().flatMap {
      case Right(Some(sessionData)) =>
        update(sessionData).flatMap {
          case Right(updatedSessionData) =>
            if (sessionData === updatedSessionData)
              Future.successful(Right(sessionData))
            else
              store(updatedSessionData)
                .map(_.map(_ => updatedSessionData))

          case Left(error) =>
            Future.successful(Left(error))
        }

      case Right(None) =>
        Future.successful(
          Left(Error("no session found in mongo"))
        )

      case Left(error) =>
        Future.successful(Left(error))
    } catch {
      case e: Exception => Future.successful(Left(Error(e)))
    }

}

object HeaderCarrierCacheId extends CacheIdType[HeaderCarrier] {

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  override def run: HeaderCarrier => String =
    _.sessionId
      .map(_.value)
      .getOrElse(throw NoSessionException)

  case object NoSessionException extends Exception("Could not find sessionId")
}

@Singleton
class DefaultSessionCache @Inject() (
  mongoComponent: MongoComponent,
  timestampSupport: TimestampSupport,
  configuration: Configuration
)(implicit
  ec: ExecutionContext
) extends MongoCacheRepository[HeaderCarrier](
      mongoComponent = mongoComponent,
      collectionName = "sessions",
      ttl = configuration.get[FiniteDuration]("session-store.expiry-time"),
      timestampSupport = timestampSupport,
      cacheIdType = HeaderCarrierCacheId
    )
    with SessionCache {

  val sessionDataKey: DataKey[SessionData] =
    DataKey[SessionData]("cdsrc-session")

  def get()(implicit
    hc: HeaderCarrier
  ): Future[Either[Error, Option[SessionData]]] =
    try super
      .get[SessionData](hc)(sessionDataKey)
      .map(Right(_))
      .recover { case e => Left(Error(e)) } catch {
      case e: Exception => Future.successful(Left(Error(e)))
    }

  def store(
    sessionData: SessionData
  )(implicit hc: HeaderCarrier): Future[Either[Error, Unit]] =
    try super
      .put(hc)(sessionDataKey, sessionData)
      .map(_ => Right(()))
      .recover { case e => Left(Error(e)) } catch {
      case e: Exception => Future.successful(Left(Error(e)))
    }

}
