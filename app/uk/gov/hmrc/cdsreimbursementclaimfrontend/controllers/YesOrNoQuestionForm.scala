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
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.text
import play.api.data.validation.Constraint
import play.api.data.validation.Invalid
import play.api.data.validation.Valid
import play.api.data.validation.ValidationError
import play.api.data.validation.ValidationResult
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes

object YesOrNoQuestionForm {

  private def isBoolean(s: String): ValidationResult = s match {
    case "true"  => Valid
    case "false" => Valid
    case _       => Invalid(ValidationError("error.invalid"))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def booleanThatExists: Constraint[String] = {
    lazy val errorMessage: String = "error.invalid"
    Constraint[String]("constraint.yesno") { s =>
      if (s === null) Invalid(ValidationError(errorMessage))
      else if (s.trim.isEmpty) Invalid(ValidationError(errorMessage))
      else isBoolean(s)
    }
  }

  def apply(key: String): Form[YesNo] =
    Form(
      mapping(
        key -> text()
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
      )(identity)(Some(_))
    )
}
