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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.implicits.catsSyntaxEitherId
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EitherUtils._

final case class MovementReferenceNumber(value: Either[EntryNumber, MRN]) extends AnyVal {

  def stringValue: String = value.fold(_.value, _.value)
}

object MovementReferenceNumber {

  def apply(entryNumber: EntryNumber): MovementReferenceNumber =
    new MovementReferenceNumber(entryNumber.asLeft[MRN])

  def apply(mrn: MRN): MovementReferenceNumber =
    new MovementReferenceNumber(mrn.asRight[EntryNumber])

  implicit val format: OFormat[MovementReferenceNumber] = Json.format[MovementReferenceNumber]
}
