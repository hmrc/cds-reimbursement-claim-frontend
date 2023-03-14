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

import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Name
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.CdsVerifiedEmail

sealed trait AuthenticatedUser {
  def name: Option[Name]
  def email: Option[Email]
  def eoriOpt: Option[Eori] = None

  final def asVerifiedEmail: Option[CdsVerifiedEmail] =
    email.map(e => CdsVerifiedEmail(e.value, ""))
}

object AuthenticatedUser {

  final case class Individual(
    ggCredId: GGCredId,
    email: Option[Email],
    eori: Eori,
    name: Option[Name]
  ) extends AuthenticatedUser {
    override def eoriOpt: Option[Eori] = Some(eori)
  }

  final case class Organisation(
    ggCredId: GGCredId,
    email: Option[Email],
    eori: Eori,
    name: Option[Name]
  ) extends AuthenticatedUser {
    override def eoriOpt: Option[Eori] = Some(eori)
  }

  final case class NonGovernmentGatewayAuthenticatedUser(authProvider: String) extends AuthenticatedUser {
    override val name: Option[Name]   = None
    override val email: Option[Email] = None
  }

  object NonGovernmentGatewayAuthenticatedUser {
    implicit val format: OFormat[NonGovernmentGatewayAuthenticatedUser] =
      Json.format[NonGovernmentGatewayAuthenticatedUser]
  }

}
