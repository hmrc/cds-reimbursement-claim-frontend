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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import cats.Eq
import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait YesNo extends Product with Serializable {
  def asBoolean: Boolean
}

object YesNo {

  def of(flag: Boolean): YesNo = if (flag) Yes else No

  final case object No extends YesNo {
    override final val asBoolean: Boolean = false
  }
  final case object Yes extends YesNo {
    override final val asBoolean: Boolean = true
  }

  implicit val eq: Eq[YesNo] = Eq.fromUniversalEquals[YesNo]

  implicit val format: OFormat[YesNo] = derived.oformat[YesNo]()
}
