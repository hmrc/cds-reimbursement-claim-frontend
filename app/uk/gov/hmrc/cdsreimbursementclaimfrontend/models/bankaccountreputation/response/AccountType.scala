/*
 * Copyright 2022 HM Revenue & Customs
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

sealed trait AccountType extends Product with Serializable

object AccountType {

  case object Personal extends AccountType { override def toString(): String = "personal" }
  case object Business extends AccountType { override def toString(): String = "business" }

  val allValues: Seq[AccountType]         = Seq(Personal, Business)
  val valuesMap: Map[String, AccountType] = allValues.map(v => (v.toString, v)).toMap

  implicit val AccountTypeRequestEnumFormat: Format[AccountType] = new Format[AccountType] {
    def reads(json: JsValue): JsResult[AccountType] = {
      val incomingValue = json.as[String]
      valuesMap.get(incomingValue) match {
        case Some(value) => JsSuccess(value)
        case None        =>
          JsError(s"AccountTypeRequest was: $incomingValue not one of: ${allValues.map(_.toString).mkString(", ")}")
      }
    }

    def writes(dec: AccountType): JsValue = JsString(dec.toString)

  }

}
