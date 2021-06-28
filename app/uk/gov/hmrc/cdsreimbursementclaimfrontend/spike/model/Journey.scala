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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.spike.model

import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.spike.model.Journey.JourneyType

final case class Journey[T <: ReferenceNumber](claim: Claim[T], journeyType: JourneyType)
    extends Product
    with Serializable

object Journey {

  sealed trait JourneyType
  case object Bulk extends JourneyType
  case object Single extends JourneyType
  case object Scheduled extends JourneyType

  def bulkFor[T <: ReferenceNumber](claim: Claim[T]): Journey[T]      = new Journey[T](claim, Bulk)
  def singleFor[T <: ReferenceNumber](claim: Claim[T]): Journey[T]    = new Journey[T](claim, Single)
  def scheduledFor[T <: ReferenceNumber](claim: Claim[T]): Journey[T] = new Journey[T](claim, Scheduled)

  implicit def journeyFormat[T <: ReferenceNumber]: OFormat[Journey[T]] =
    Json.format[Journey[T]]
}
