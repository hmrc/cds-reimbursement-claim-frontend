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

import play.api.libs.json.Format
import play.api.libs.json.JsFalse
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori

sealed trait UserXiEori {

  final def toEoriOpt: Option[Eori] =
    this match {
      case UserXiEori.NotRegistered => None
      case UserXiEori.Some(value)   => Some(Eori(value))
    }
}

object UserXiEori {

  object NotRegistered extends UserXiEori
  final case class Some private (value: String) extends UserXiEori

  def apply(value: String): UserXiEori =
    Some(value)

  def unapply(userXiEori: UserXiEori): Option[Eori] =
    userXiEori.toEoriOpt

  private val formatOfSome: Format[Some] = Json.format[Some]

  implicit val format: Format[UserXiEori] =
    Format(
      Reads {
        case JsFalse => JsSuccess(NotRegistered)
        case other   => formatOfSome.reads(other)
      },
      Writes {
        case NotRegistered => JsFalse
        case some: Some    => formatOfSome.writes(some)
      }
    )
}
