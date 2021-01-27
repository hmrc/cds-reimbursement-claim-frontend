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

import play.api.libs.json.{Json, Writes}

final class MRN private[MRN] (val value: String) extends AnyVal

object MRN {

  def parse(value: String): Option[MRN] =
    if (isValid(value)) Some(new MRN(value)) else None

  def isValid(in: String): Boolean = {
    val regex = """\d{2}[a-zA-Z]{2}\w{13}\d""".r
    in match {
      case regex(_*) => true
      case _         => false
    }
  }

  implicit val mrnWrites: Writes[MRN] = Json.valueWrites
}
