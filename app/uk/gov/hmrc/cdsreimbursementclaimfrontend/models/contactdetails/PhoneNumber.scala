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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails

import cats.Eq
import cats.instances.string.*
import cats.syntax.eq.*
import play.api.data.Forms.nonEmptyText
import play.api.data.Mapping
import play.api.libs.functional.syntax.*
import play.api.libs.json.Format

import java.util.function.Predicate

final case class PhoneNumber(value: String)

object PhoneNumber {

  val phoneNumberRegex: Predicate[String] = "^(\\+)?[0-9]{1,15}$".r.pattern.asPredicate()

  implicit val format: Format[PhoneNumber] =
    implicitly[Format[String]].inmap(PhoneNumber(_), _.value)

  implicit val eq: Eq[PhoneNumber] = Eq.instance(_.value === _.value)

  val mapping: Mapping[PhoneNumber] =
    nonEmptyText(maxLength = 30)
      .transform[PhoneNumber](s => PhoneNumber(s.replace(" ", "")), _.value)
      .verifying("invalid", e => phoneNumberRegex.test(e.value))
}
