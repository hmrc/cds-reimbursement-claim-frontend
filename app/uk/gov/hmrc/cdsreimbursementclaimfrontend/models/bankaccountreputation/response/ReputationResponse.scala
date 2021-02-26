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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response

import play.api.libs.json._
import cats.kernel.Eq

sealed trait ReputationResponse extends Product with Serializable

object ReputationResponse {

  case object Yes extends ReputationResponse { override def toString(): String = "yes" }
  case object No extends ReputationResponse { override def toString(): String = "no" }
  case object Indeterminate extends ReputationResponse { override def toString(): String = "indeterminate" }
  case object Inapplicable extends ReputationResponse { override def toString(): String = "inapplicable" }
  case object Error extends ReputationResponse { override def toString(): String = "error" }

  val allValues: Seq[ReputationResponse]         = Seq(Yes, No, Indeterminate, Inapplicable, Error)
  val valuesMap: Map[String, ReputationResponse] = allValues.map(v => (v.toString, v)).toMap

  implicit val ReputationResponseEnumFormat: Format[ReputationResponse] = new Format[ReputationResponse] {
    def reads(json: JsValue): JsResult[ReputationResponse] = {
      val incomingValue = json.as[String]
      valuesMap.get(incomingValue) match {
        case Some(value) => JsSuccess(value)
        case None        =>
          JsError(
            s"Bank account reputation was: $incomingValue not one of: ${allValues.map(_.toString).mkString(", ")}"
          )
      }
    }

    def writes(dec: ReputationResponse): JsValue = JsString(dec.toString)
  }

  implicit val reputationResponseEq: Eq[ReputationResponse] = Eq.fromUniversalEquals
}
