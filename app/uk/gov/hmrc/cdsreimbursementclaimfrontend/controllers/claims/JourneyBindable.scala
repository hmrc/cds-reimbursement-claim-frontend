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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import play.api.mvc.PathBindable

sealed abstract class JourneyBindable(val value: String) extends Product with Serializable

object JourneyBindable {

  case object Single extends JourneyBindable("single")
  case object Bulk extends JourneyBindable("bulk")
  case object Schedule extends JourneyBindable("schedule")

  def parse(in: String): JourneyBindable = in match {
    case "single"   => Single
    case "bulk"     => Bulk
    case "schedule" => Schedule
  }

  implicit lazy val pathBindable: PathBindable[JourneyBindable] = new PathBindable[JourneyBindable] {

    override def bind(key: String, value: String): Either[String, JourneyBindable] =
      Right(JourneyBindable.parse(value))

    override def unbind(key: String, bindable: JourneyBindable): String =
      bindable.value
  }

}
