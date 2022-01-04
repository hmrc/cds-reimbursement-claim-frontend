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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids

import cats.Eq
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.OrdinalNumeral

final case class AssociatedMrnIndex private (urlIndex: Int) {

  def +(integer: Int): AssociatedMrnIndex =
    AssociatedMrnIndex(urlIndex + integer)

  def -(integer: Int): AssociatedMrnIndex =
    AssociatedMrnIndex(urlIndex - integer)

  def ordinalNumeral: String = OrdinalNumeral(urlIndex)

  def toListIndex: Int = urlIndex - 2

  def toUrlIndex: Int = urlIndex
}

object AssociatedMrnIndex {

  def toListIndex(associatedMrnIndex: AssociatedMrnIndex): Int =
    associatedMrnIndex.toListIndex

  def fromListIndex(index: Int): AssociatedMrnIndex =
    AssociatedMrnIndex(index + 2)

  def toUrlIndex(associatedMrnIndex: AssociatedMrnIndex): Int =
    associatedMrnIndex.toUrlIndex

  def fromUrlIndex(index: Int): AssociatedMrnIndex =
    AssociatedMrnIndex(index)

  implicit val eq: Eq[AssociatedMrnIndex] =
    Eq.fromUniversalEquals

  implicit val format: OFormat[AssociatedMrnIndex] =
    Json.format[AssociatedMrnIndex]
}
