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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import cats.implicits.catsSyntaxEq
import play.api.data.Forms.{mapping, text}
import play.api.data.{Form, Mapping}
import play.api.data.validation.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.{No, Yes}

object YesOrNoQuestionForm {

  def apply(key: String): Form[YesNo] =
    Form(
      mapping(
        key -> yesNoMapping(key)
      )(identity)(Some(_))
    )

  def yesNoMapping(key: String): Mapping[YesNo] =
    text()
      .verifying(booleanThatExists)
      .transform[YesNo](
        {
          case "true"  => Yes
          case "false" => No
        },
        {
          case Yes => "true"
          case No  => "false"
        }
      )

  private def booleanThatExists: Constraint[String] = {
    lazy val errorMessage: String = "error.required"
    Constraint[String]("constraint.yesno") { s =>
      if s === null then Invalid(ValidationError(errorMessage))
      else if s.trim.isEmpty then Invalid(ValidationError(errorMessage))
      else isBoolean(s)
    }
  }

  private def isBoolean(s: String): ValidationResult = s match {
    case "true"  => Valid
    case "false" => Valid
    case _       => Invalid(ValidationError("error.required"))
  }
}
