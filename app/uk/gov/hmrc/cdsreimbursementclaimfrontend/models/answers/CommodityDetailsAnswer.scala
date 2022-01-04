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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers

import cats.Id
import cats.implicits.catsSyntaxOption
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.MissingAnswerError
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.validation.Validator

final case class CommodityDetailsAnswer(value: String) extends AnyVal

object CommodityDetailsAnswer {

  val validator: Validator[Id, CommodityDetailsAnswer] = (maybeCommodityDetails: Option[CommodityDetailsAnswer]) =>
    maybeCommodityDetails.toValidNel(MissingAnswerError("Commodity Details"))

  implicit val commodityDetailsFormat: OFormat[CommodityDetailsAnswer] = Json.format[CommodityDetailsAnswer]
}
