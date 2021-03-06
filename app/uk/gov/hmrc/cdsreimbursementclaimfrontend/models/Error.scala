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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error.{IdKey, IdValue}

final case class Error(message: String, throwable: Option[Throwable], identifiers: Map[IdKey, IdValue])

object Error {

  type IdKey   = String
  type IdValue = String

  def apply(message: String, identifiers: (IdKey, IdValue)*): Error = Error(message, None, identifiers.toMap)

  def apply(error: Throwable, identifiers: (IdKey, IdValue)*): Error =
    Error(error.getMessage, Some(error), identifiers.toMap)

}
