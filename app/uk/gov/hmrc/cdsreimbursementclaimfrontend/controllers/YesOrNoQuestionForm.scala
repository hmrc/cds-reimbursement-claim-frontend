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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import cats.implicits.catsSyntaxEq
import play.api.data.Form
import play.api.data.Forms.boolean
import play.api.data.Forms.mapping
import play.api.data.Forms.optional
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging

object YesOrNoQuestionForm extends Logging {

  // todo we need to validate key exists as a field
  // so make key not optional and check it exists first
  def apply(key: String): Form[YesNo] =
    Form(
      mapping(
        key -> optional(boolean)
          .verifying("error.invalid", _.isDefined)
          .transform[YesNo](
            value => {
              logger.warn(s"**** value=$value")
              if (value.exists(_ === true)) Yes else No
            },
            answer => {
              logger.warn(s"**** answer=$answer")
              Some(answer === Yes)
            }
          )
      )(identity)(Some(_))
    )

}
