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

import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Name
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids._

sealed trait RetrievedUserType extends Product with Serializable

object RetrievedUserType {

  final case class Individual(
    ggCredId: GGCredId,
    email: Option[Email],
    eori: Eori,
    name: Option[Name]
  ) extends RetrievedUserType

  final case class Organisation(
    ggCredId: GGCredId,
    eori: Eori,
    name: Option[Name]
  ) extends RetrievedUserType

  final case class NonGovernmentGatewayRetrievedUser(authProvider: String) extends RetrievedUserType

  object NonGovernmentGatewayRetrievedUser {
    implicit val format: OFormat[NonGovernmentGatewayRetrievedUser] = Json.format[NonGovernmentGatewayRetrievedUser]
  }

}
