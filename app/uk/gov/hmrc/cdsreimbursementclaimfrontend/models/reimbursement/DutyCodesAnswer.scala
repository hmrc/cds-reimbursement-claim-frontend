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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.reimbursement

import cats.Eq
import play.api.libs.json._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

final case class DutyCodesAnswer(dutyCodes: Map[DutyType, List[TaxCode]])

object DutyCodesAnswer {

  val empty: DutyCodesAnswer = DutyCodesAnswer(Map.empty)

  implicit def dutyCodesAnswerFormat: Format[Map[DutyType, List[TaxCode]]] =
    new Format[Map[DutyType, List[TaxCode]]] {
      override def reads(json: JsValue): JsResult[Map[DutyType, List[TaxCode]]] =
        json
          .validate[Map[String, List[TaxCode]]]
          .map { dutyCodesMap =>
            dutyCodesMap.map { codes =>
              DutyType.toDutyType(codes._1) match {
                case Some(dutyType) => (dutyType, codes._2)
                case None           => sys.error("Could not convert string to a duty type")
              }
            }
          }

      override def writes(o: Map[DutyType, List[TaxCode]]): JsValue =
        Json.toJson(o.map { dutyCodes =>
          (DutyType.typeToString(dutyCodes._1), dutyCodes._2)
        })
    }

  implicit class DutyCodesAnswerOps(private val dutyCodesAnswer: DutyCodesAnswer) {
    def existsDutyTypeWithNoDutyCodesAnswer: Option[DutyType] =
      dutyCodesAnswer.dutyCodes.find(d => d._2.isEmpty).map { dutyTypeToDutyCodeMap =>
        dutyTypeToDutyCodeMap._1
      }
  }
  implicit val eq: Eq[DutyCodesAnswer]          = Eq.fromUniversalEquals[DutyCodesAnswer]
  implicit val format: OFormat[DutyCodesAnswer] = Json.format[DutyCodesAnswer]
}
