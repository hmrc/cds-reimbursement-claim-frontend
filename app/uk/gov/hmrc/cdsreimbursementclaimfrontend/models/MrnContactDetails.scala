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

import julienrf.json.derived
//import play.api.libs.json.{Json, OFormat}
import play.api.libs.json.OFormat

//sealed trait MrnContactDetails extends Product with Serializable
final case class MrnContactDetails()

object MrnContactDetails {
  //implicit val format: OFormat[ContactDetailsAnswer] = derived.oformat[ContactDetailsAnswer]()
  //implicit val format: OFormat[NonGovernmentGatewayRetrievedUser] = Json.format[NonGovernmentGatewayRetrievedUser]
  //implicit val format: OFormat[MrnContactDetails] = Json.format[MrnContactDetails]
  implicit val format: OFormat[MrnContactDetails] = derived.oformat[MrnContactDetails]()
}
