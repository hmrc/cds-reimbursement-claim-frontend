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

import cats.syntax.eq._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import java.util.Locale

final case class EoriNumbersVerification(
  consigneeEoriNumber: Option[Eori] = None,
  declarantEoriNumber: Option[Eori] = None,
  userXiEori: Option[UserXiEori] = None
) {

  def hasSameXiEoriAs(otherEori: Eori): Boolean =
    userXiEori match {
      case Some(UserXiEori.Some(eori)) =>
        otherEori.value.toUpperCase(Locale.ENGLISH) === eori.toUpperCase(Locale.ENGLISH)

      case _ => false
    }

  def hasSameXiEoriAs(otherEori: Option[Eori]): Boolean =
    otherEori match {
      case None              => false
      case Some(Eori(eori1)) =>
        userXiEori match {
          case Some(UserXiEori.Some(eori2)) =>
            eori1.toUpperCase(Locale.ENGLISH) === eori2.toUpperCase(Locale.ENGLISH)

          case _ => false
        }
    }

  def keepUserXiEoriOnly: EoriNumbersVerification =
    EoriNumbersVerification(userXiEori = userXiEori)
}

object EoriNumbersVerification {
  final implicit val format: OFormat[EoriNumbersVerification] =
    Json.format[EoriNumbersVerification]
}
