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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails

import cats.Eq
import cats.instances.string._
import cats.syntax.eq._
import play.api.data.Forms.nonEmptyText
import play.api.data.Mapping
import play.api.libs.functional.syntax._
import play.api.libs.json.Format

import java.util.function.Predicate

final case class Email(value: String) extends AnyVal

object Email {

  val emailRegex: Predicate[String] =
    "^(?=.{1,241}$)(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])".r.pattern
      .asPredicate()

  implicit val format: Format[Email] =
    implicitly[Format[String]].inmap(Email(_), _.value)

  implicit val eq: Eq[Email] = Eq.instance(_.value === _.value)

  val mappingMaxLength: Mapping[Email] =
    nonEmptyText(maxLength = 241)
      .transform[Email](s => Email(s.replaceAllLiterally(" ", "")), _.value)
      .verifying("invalid", e => emailRegex.test(e.value))

}
