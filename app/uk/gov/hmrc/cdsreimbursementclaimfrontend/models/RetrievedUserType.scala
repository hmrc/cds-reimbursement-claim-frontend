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

import play.api.libs.json.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.CdsVerifiedEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.*

sealed trait AuthenticatedUser {
  def name: Option[String]
  def email: Option[Email] // fixme email can go
  def eoriOpt: Option[Eori] = None

  final def asVerifiedEmail: Option[CdsVerifiedEmail] =
    email.map(e => CdsVerifiedEmail(e.value, ""))
}

object AuthenticatedUser {

  // fixme no need to have Individual and Organisation (we treat them the same way)

  final case class Individual(
    email: Option[Email],
    eori: Eori,
    name: Option[String]
  ) extends AuthenticatedUser {
    override def eoriOpt: Option[Eori] = Some(eori)
  }

  object Individual {
    implicit val format: OFormat[Individual] = Json.format[Individual]
  }

  final case class Organisation(
    email: Option[Email],
    eori: Eori,
    name: Option[String]
  ) extends AuthenticatedUser {
    override def eoriOpt: Option[Eori] = Some(eori)
  }

  object Organisation {
    implicit val format: OFormat[Organisation] = Json.format[Organisation]
  }

  final case class NonGovernmentGatewayAuthenticatedUser(authProvider: String) extends AuthenticatedUser {
    override val name: Option[String] = None
    override val email: Option[Email] = None
  }

  object NonGovernmentGatewayAuthenticatedUser {
    implicit val format: OFormat[NonGovernmentGatewayAuthenticatedUser] =
      Json.format[NonGovernmentGatewayAuthenticatedUser]
  }

  implicit val format: Format[AuthenticatedUser] =
    Format(
      Reads.apply[AuthenticatedUser](json =>
        Individual.format
          .reads(json)
          .map(_.asInstanceOf[AuthenticatedUser])
          .recoverWith(_ => Organisation.format.reads(json).map(_.asInstanceOf[AuthenticatedUser]))
          .recoverWith(_ =>
            NonGovernmentGatewayAuthenticatedUser.format.reads(json).map(_.asInstanceOf[AuthenticatedUser])
          )
      ),
      Writes.apply {
        case value: Individual                            =>
          Individual.format.writes(value)
        case value: Organisation                          =>
          Organisation.format.writes(value)
        case value: NonGovernmentGatewayAuthenticatedUser =>
          NonGovernmentGatewayAuthenticatedUser.format.writes(value)
      }
    )

}
