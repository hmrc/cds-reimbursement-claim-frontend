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

import play.api.libs.functional.syntax.*
import play.api.libs.json.Format
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.DateUtils

import java.time.LocalDate

final case class InspectionDate(value: LocalDate)

object InspectionDate {

  implicit class InspectionDateOps(private val inspectionDate: InspectionDate) {
    def checkYourDetailsDisplayFormat: String =
      DateUtils.displayFormat(inspectionDate.value.toString).getOrElse("")
  }

  implicit val format: Format[InspectionDate] =
    implicitly[Format[LocalDate]].inmap(InspectionDate(_), _.value)

}
