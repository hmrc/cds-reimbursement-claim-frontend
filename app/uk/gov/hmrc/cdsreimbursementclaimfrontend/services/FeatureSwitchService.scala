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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import com.google.inject.ImplementedBy
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.Configuration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature

import java.util.concurrent.ConcurrentHashMap
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.FeaturesCache
import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration

@ImplementedBy(classOf[ConfiguredFeatureSwitchService])
trait FeatureSwitchService {

  def isEnabled(feature: Feature)(implicit hc: HeaderCarrier): Boolean

  final def isDisabled(feature: Feature)(implicit hc: HeaderCarrier): Boolean =
    !isEnabled(feature)

  def isEnabledForApplication(feature: Feature): Boolean

  final def optionally[A](feature: Feature, value: A)(implicit hc: HeaderCarrier): Option[A] =
    if (isEnabled(feature)) Some(value) else None

  /** Enable feature switch for the current session */
  def enableForSession(feature: Feature)(implicit hc: HeaderCarrier): Unit

  /** Enable feature switch for the current session */
  def disableForSession(feature: Feature)(implicit hc: HeaderCarrier): Unit

  /** Enable feature switch in JVM for testing */
  def enable(feature: Feature): Unit

  /** Disable feature switch in JVM for testing */
  def disable(feature: Feature): Unit
}

@Singleton
class ConfiguredFeatureSwitchService @Inject() (
  val configuration: Configuration,
  featuresCache: FeaturesCache
) extends FeatureSwitchService {

  private val timeout = Duration("5s")

  private val features: ConcurrentHashMap[Feature, Boolean] =
    new ConcurrentHashMap[Feature, Boolean]()

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def enable(feature: Feature): Unit = {
    features.put(feature, true)
    ()
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def disable(feature: Feature): Unit = {
    features.put(feature, false)
    ()
  }

  def isEnabled(feature: Feature)(implicit hc: HeaderCarrier): Boolean =
    Option(features.get(feature))
      .orElse(isEnabledInCache(feature))
      .orElse(sys.props.get(s"features.${feature.name}").map(_.toBoolean))
      .orElse(configuration.getOptional[Boolean](s"features.${feature.name}"))
      .getOrElse(false)

  @SuppressWarnings(Array("org.wartremover.warts.GlobalExecutionContext"))
  def enableForSession(feature: Feature)(implicit hc: HeaderCarrier): Unit =
    Await.result(
      featuresCache
        .update(_.enable(feature))
        .map(_ => ()),
      timeout
    )

  @SuppressWarnings(Array("org.wartremover.warts.GlobalExecutionContext"))
  def disableForSession(feature: Feature)(implicit hc: HeaderCarrier): Unit =
    Await.result(
      featuresCache
        .update(_.disable(feature))
        .map(_ => ()),
      timeout
    )

  def isEnabledForApplication(feature: Feature): Boolean =
    sys.props
      .get(s"features.${feature.name}")
      .map(_.toBoolean)
      .orElse(configuration.getOptional[Boolean](s"features.${feature.name}"))
      .getOrElse(false)

  @SuppressWarnings(Array("org.wartremover.warts.GlobalExecutionContext", "org.wartremover.warts.Throw"))
  private def isEnabledInCache(feature: Feature)(implicit hc: HeaderCarrier): Option[Boolean] =
    Await.result(
      featuresCache
        .get()
        .map(
          _.fold(
            e => throw e.toException,
            _.isEnabled(feature)
          )
        ),
      timeout
    )
}
