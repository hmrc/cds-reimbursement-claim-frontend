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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.components.summary

import cats.data._
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesController.getDescription
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SelectDutiesController.selectDutiesKey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BigDecimalOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyAmount

object SelectDutiesSummary {

  final case class DutyDescription(duty: DutyAmount, description: String, caption: String) {
    override def toString: String = duty.taxCode.value + " - " + description
  }

  @SuppressWarnings(Array("org.wartremover.warts.Option2Iterable"))
  def apply(dutiesAvailable: NonEmptyList[DutyAmount])(implicit messages: Messages): List[DutyDescription] =
    dutiesAvailable.toList
      .flatMap { duty =>
        getDescription(s"$selectDutiesKey.duty.${duty.taxCode.value}", messages)
          .map { description =>
            DutyDescription(
              duty,
              description,
              messages("select-duties.duty.caption").format(duty.amount.toPoundSterlingString)
            )
          }
      }
      .sortBy(_.description)
}
