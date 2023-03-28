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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.FeatureSet
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.cache.DataKey
import uk.gov.hmrc.mongo.cache.MongoCacheRepository
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.TimestampSupport

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[DefaultFeaturesCache])
trait FeaturesCache {

  def get()(implicit
    hc: HeaderCarrier
  ): Future[Either[Error, FeatureSet]]

  def store(featureSet: FeatureSet)(implicit
    hc: HeaderCarrier
  ): Future[Either[Error, Unit]]

  final def update(modify: FeatureSet => FeatureSet)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Either[Error, Unit]] =
    try get().flatMap {
      case Right(value) =>
        val newFeatureSet = modify(value)
        if (newFeatureSet =!= value)
          store(newFeatureSet)
        else
          Future.successful(Right(()))

      case Left(error) =>
        Future.successful(Left(error))
    } catch {
      case e: Exception => Future.successful(Left(Error(e)))
    }

}

@Singleton
class DefaultFeaturesCache @Inject() (
  mongoComponent: MongoComponent,
  timestampSupport: TimestampSupport,
  configuration: Configuration
)(implicit
  ec: ExecutionContext
) extends MongoCacheRepository[HeaderCarrier](
      mongoComponent = mongoComponent,
      collectionName = "features",
      ttl = configuration.get[FiniteDuration]("features-store.expiry-time"),
      timestampSupport = timestampSupport,
      cacheIdType = HeaderCarrierCacheId
    )
    with FeaturesCache {

  val featureSetKey: DataKey[FeatureSet] =
    DataKey[FeatureSet]("cdsrc-features")

  def get()(implicit
    hc: HeaderCarrier
  ): Future[Either[Error, FeatureSet]] =
    try super
      .get[FeatureSet](hc)(featureSetKey)
      .map(opt => Right(opt.getOrElse(FeatureSet.empty)))
      .recover { case e => Left(Error(e)) } catch {
      case HeaderCarrierCacheId.NoSessionException => Future.successful(Right(FeatureSet.empty))
      case e: Exception                            => Future.successful(Left(Error(e)))
    }

  def store(
    featureSet: FeatureSet
  )(implicit hc: HeaderCarrier): Future[Either[Error, Unit]] =
    try super
      .put(hc)(featureSetKey, featureSet)
      .map(_ => Right(()))
      .recover { case e => Left(Error(e)) } catch {
      case HeaderCarrierCacheId.NoSessionException => Future.successful(Right(()))
      case e: Exception                            => Future.successful(Left(Error(e)))
    }

}
