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

import cats.Eq
import cats.implicits.catsSyntaxOptionId
import play.api.libs.json.{Format, Json}

final case class SessionData(
  journey: Option[JourneyStatus]
) {

  def copyWith(journeyStatus: Option[JourneyStatus]): SessionData =
    copy(journey = journeyStatus)

  def journeyStatus: Option[JourneyStatus] = journey

}

object SessionData {

  def apply(status: JourneyStatus): SessionData =
    SessionData(status.some)

  implicit val format: Format[SessionData] = Json.format

  implicit val eq: Eq[SessionData] = Eq.fromUniversalEquals[SessionData]

  val empty: SessionData = SessionData(None)
}
