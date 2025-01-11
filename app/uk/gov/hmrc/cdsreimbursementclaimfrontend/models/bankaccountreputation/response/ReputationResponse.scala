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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response

import cats.kernel.Eq
import play.api.libs.json.*

sealed trait ReputationResponse extends Product with Serializable {
  def acceptable: Boolean
}

object ReputationResponse {

  case object Yes extends ReputationResponse {
    override def toString(): String  = "yes"
    override def acceptable: Boolean = true
  }
  case object No extends ReputationResponse {
    override def toString(): String  = "no"
    override def acceptable: Boolean = false
  }
  case object Indeterminate extends ReputationResponse {
    override def toString(): String  = "indeterminate"
    override def acceptable: Boolean = true
  }
  case object Inapplicable extends ReputationResponse {
    override def toString(): String  = "inapplicable"
    override def acceptable: Boolean = false
  }
  case object Error extends ReputationResponse {
    override def toString(): String  = "error"
    override def acceptable: Boolean = false
  }
  case object Partial extends ReputationResponse {
    override def toString(): String  = "partial"
    override def acceptable: Boolean = true
  }

  val allValues: Seq[ReputationResponse]         = Seq(Yes, No, Indeterminate, Inapplicable, Error, Partial)
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
