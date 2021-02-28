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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.form

import play.api.data.Form
import play.api.data.Forms.{bigDecimal, mapping}
import play.api.libs.json.{Json, OFormat}

final case class ClaimAmount(amount: BigDecimal)

object ClaimAmount {
  val claimAmountForm: Form[ClaimAmount]    = Form(
    mapping(
      "claim" -> bigDecimal
    )(ClaimAmount.apply)(ClaimAmount.unapply)
  )
  implicit val format: OFormat[ClaimAmount] = Json.format[ClaimAmount]
}
