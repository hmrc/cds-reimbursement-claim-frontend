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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.spike

import cats.Eq
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.GGCredId
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{SignedInUserDetails, UserType}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.spike.model.{Journey, ReferenceNumber}

sealed trait Session extends Product with Serializable {
  val userType: Option[UserType]
}

object Session {

  final case class NonGovernmentGatewaySession(userType: Option[UserType]) extends Session

  final case class GovernmentGatewaySession[T <: ReferenceNumber](
    ggCredId: GGCredId,
    userType: Option[UserType],
    signedInUserDetails: SignedInUserDetails,
    journey: Journey[T],
    status: JourneySubmitStatus
  ) extends Session

  implicit def format[T <: ReferenceNumber]: OFormat[GovernmentGatewaySession[T]] = Json.format
  implicit val eq: Eq[Session]                                                    = Eq.fromUniversalEquals[Session]

}
