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

import play.api.libs.json._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode

final case class DutyPaidAndClaimAmountAnswer(
  dutyPaidAndClaimAmountsEntered: Map[DutyType, Map[TaxCode, List[DutyPaidAndClaimAmount]]]
)

object DutyPaidAndClaimAmountAnswer {

  implicit def dutyPaidAndClaimAmountAnswerFormat: Format[Map[DutyType, Map[TaxCode, List[DutyPaidAndClaimAmount]]]] =
    new Format[Map[DutyType, Map[TaxCode, List[DutyPaidAndClaimAmount]]]] {
      override def reads(json: JsValue): JsResult[Map[DutyType, Map[TaxCode, List[DutyPaidAndClaimAmount]]]] =
        json
          .validate[Map[String, Map[String, List[DutyPaidAndClaimAmount]]]]
          .map { stringToStringToDutyPaidAndClaimAmounts =>
            stringToStringToDutyPaidAndClaimAmounts.map { dutyTypeTuple =>
              DutyType.toDutyType(dutyTypeTuple._1) match {
                case Some(dutyType) =>
                  val taxCodeToPaidAndClaimAmounts = dutyTypeTuple._2.map { taxCodeTuple =>
                    TaxCode.fromString(taxCodeTuple._1) match {
                      case Some(taxCode) => (taxCode -> taxCodeTuple._2)
                      case None          => sys.error("Could not convert string to tax code type")
                    }
                  }
                  dutyType -> taxCodeToPaidAndClaimAmounts
                case None           => sys.error("Could not convert string to a duty type")
              }
            }
          }

      override def writes(o: Map[DutyType, Map[TaxCode, List[DutyPaidAndClaimAmount]]]): JsValue =
        Json.toJson(o.map { dutyTypesTuple =>
          (
            DutyType.typeToString(dutyTypesTuple._1),
            dutyTypesTuple._2.map { taxCodeTuple =>
              (TaxCode.classToNameString(taxCodeTuple._1), taxCodeTuple._2)
            }
          )
        })
    }

  implicit val format: OFormat[DutyPaidAndClaimAmountAnswer] = Json.format[DutyPaidAndClaimAmountAnswer]
}
