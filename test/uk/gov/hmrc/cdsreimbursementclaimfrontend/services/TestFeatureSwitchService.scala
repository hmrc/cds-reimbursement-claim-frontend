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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature

class TestFeatureSwitchService(initialFeatures: Feature*) extends FeatureSwitchService {

  @SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures"))
  private val features: collection.mutable.Set[Feature] =
    collection.mutable.Set(initialFeatures: _*)

  def enable(feature: Feature): Boolean =
    features.add(feature)

  def disable(feature: Feature): Boolean =
    features.remove(feature)

  def isEnabled(feature: Feature): Boolean =
    features.contains(feature)

}
