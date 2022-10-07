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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.services

import com.google.inject.ImplementedBy
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.Configuration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature

import java.util.concurrent.ConcurrentHashMap

@ImplementedBy(classOf[ConfiguredFeatureSwitchService])
trait FeatureSwitchService {
  def enable(feature: Feature): Boolean
  def disable(feature: Feature): Boolean
  def isEnabled(feature: Feature): Boolean

  final def isDisabled(feature: Feature): Boolean =
    !isEnabled(feature)

  final def optionally[A](feature: Feature, value: A): Option[A] =
    if (isEnabled(feature)) Some(value) else None
}

@Singleton
class ConfiguredFeatureSwitchService @Inject() (
  val configuration: Configuration
) extends FeatureSwitchService {

  private val features: ConcurrentHashMap[Feature, Boolean] =
    new ConcurrentHashMap[Feature, Boolean]()

  def enable(feature: Feature): Boolean =
    Option(features.put(feature, true))
      .getOrElse(false)

  def disable(feature: Feature): Boolean =
    Option(features.put(feature, false))
      .getOrElse(false)

  def isEnabled(feature: Feature): Boolean =
    Option(features.get(feature))
      .orElse(sys.props.get(s"features.${feature.name}").map(_.toBoolean))
      .orElse(configuration.getOptional[Boolean](s"features.${feature.name}"))
      .getOrElse(false)
}
