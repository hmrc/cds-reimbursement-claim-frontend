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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.Eq
import play.api.libs.json.Json
import play.api.libs.json.OFormat

// path: queryDeclarationStatusResponse/responseDetail/retrieveDeclarationStatusResponse
final case class DEC91Response(
  retrieveDeclarationStatusDetailsList: Seq[DEC91Response.DeclarationStatusDetails]
)

object DEC91Response {

  final case class DeclarationStatusDetails(declaration: Declaration)

  final case class Declaration(irc: String)

  object Declaration {
    implicit val equality: Eq[Declaration] =
      Eq.fromUniversalEquals[Declaration]

    implicit val format: OFormat[Declaration] =
      Json.format[Declaration]
  }

  object DeclarationStatusDetails {
    implicit val equality: Eq[DeclarationStatusDetails] =
      Eq.fromUniversalEquals[DeclarationStatusDetails]

    implicit val format: OFormat[DeclarationStatusDetails] =
      Json.format[DeclarationStatusDetails]
  }

  implicit val equality: Eq[DEC91Response] =
    Eq.fromUniversalEquals[DEC91Response]

  implicit val format: OFormat[DEC91Response] =
    Json.format[DEC91Response]

}
