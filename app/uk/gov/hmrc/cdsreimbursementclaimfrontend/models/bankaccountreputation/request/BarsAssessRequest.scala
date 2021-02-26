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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.request

import play.api.libs.json.{Json, OFormat}

sealed trait BarsAssesRequest

final case class BarsBusinessAssessRequest(account: BarsAccount, business: Option[BarsBusiness])
    extends BarsAssesRequest

object BarsBusinessAssessRequest {
  implicit val format: OFormat[BarsBusinessAssessRequest] = Json.format[BarsBusinessAssessRequest]
}

final case class BarsPersonalAssessRequest(account: BarsAccount, subject: BarsSubject) extends BarsAssesRequest

object BarsPersonalAssessRequest {
  implicit val format: OFormat[BarsPersonalAssessRequest] = Json.format[BarsPersonalAssessRequest]
}
