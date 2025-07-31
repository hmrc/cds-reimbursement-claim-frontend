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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.Eq
import cats.syntax.eq.*
import play.api.libs.json.Format
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.EnumerationFormat

sealed trait Feature {
  def name: String
}

object Feature extends EnumerationFormat[Feature] {

  case object SkipDocumentType extends Feature { val name = "skip-document-type" }
  case object NewEoriFormat extends Feature { val name = "new-eori-format" }
  case object BasisOfClaimOther extends Feature { val name = "basis-of-claim.other.enabled" }

  def of(name: String): Option[Feature] =
    values.find(_.name === name)

  override val values: Set[Feature] =
    Set(
      SkipDocumentType,
      NewEoriFormat,
      BasisOfClaimOther
    )
}

final case class FeatureSet(
  enabled: Set[Feature] = Set.empty,
  disabled: Set[Feature] = Set.empty
) {

  def isEnabled(feature: Feature): Option[Boolean] =
    if enabled.contains(feature) then Some(true)
    else if disabled.contains(feature) then Some(false)
    else None

  def enable(feature: Feature): FeatureSet =
    copy(enabled + feature, disabled - feature)

  def disable(feature: Feature): FeatureSet =
    copy(enabled - feature, disabled + feature)
}

object FeatureSet {

  val empty: FeatureSet = FeatureSet()

  implicit val format: Format[FeatureSet] =
    Json.format[FeatureSet]

  implicit val eq: Eq[FeatureSet] =
    Eq.fromUniversalEquals
}
