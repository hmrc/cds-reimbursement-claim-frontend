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

import org.scalactic.TypeCheckedTripleEquals
import play.api.i18n.MessagesApi
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes

class YesOrNoQuestionFormSpec extends ControllerSpec with TypeCheckedTripleEquals {

  lazy val controller: UnauthorisedController = instanceOf[UnauthorisedController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  "A YesOrNoQuestionForm" when {

    "processing a non existent key" should {
      "flag errors" in {
        YesOrNoQuestionForm("")
          .bind(Map("check-declaration-details" -> "true"))
          .fold(
            _ => "Error",
            {
              case Yes => "yes"
              case No  => "no"
            }
          ) should ===("Error")
      }
    }

    "processing an unexpected key" should {
      "flag errors" in {
        YesOrNoQuestionForm("spam.key")
          .bind(Map("check-declaration-details.scheduled" -> "true"))
          .fold(
            _ => "Error",
            {
              case Yes => "yes"
              case No  => "no"
            }
          ) should ===("Error")
      }
    }

    "processing an unexpected sub key" should {
      "flag errors" in {
        YesOrNoQuestionForm("check-declaration-details.spam")
          .bind(Map("check-declaration-details.scheduled" -> "true"))
          .fold(
            _ => "Error",
            {
              case Yes => "yes"
              case No  => "no"
            }
          ) should ===("Error")
      }
    }

    "processing an unexpected sub key when no sub key expected" should {
      "flag errors" in {
        YesOrNoQuestionForm("check-declaration-details")
          .bind(Map("check-declaration-details.scheduled" -> "true"))
          .fold(
            _ => "Error",
            {
              case Yes => "yes"
              case No  => "no"
            }
          ) should ===("Error")
      }
    }

    "processing an expected key when yes is selected" should {
      "flag errors" in {
        YesOrNoQuestionForm("check-declaration-details")
          .bind(Map("check-declaration-details" -> "true"))
          .fold(
            _ => "Error",
            {
              case Yes => "yes"
              case No  => "no"
            }
          ) should ===("yes")
      }
    }

    "processing an expected key when no is selected" should {
      "flag errors" in {
        YesOrNoQuestionForm("check-declaration-details")
          .bind(Map("check-declaration-details" -> "false"))
          .fold(
            _ => "Error",
            {
              case Yes => "yes"
              case No  => "no"
            }
          ) should ===("no")
      }
    }

    "processing an expected sub key when yes is selected" should {
      "flag errors" in {
        YesOrNoQuestionForm("check-declaration-details.scheduled")
          .bind(Map("check-declaration-details.scheduled" -> "true"))
          .fold(
            _ => "Error",
            {
              case Yes => "yes"
              case No  => "no"
            }
          ) should ===("yes")
      }
    }

    "processing an expected sub key when no is selected" should {
      "flag errors" in {
        YesOrNoQuestionForm("check-declaration-details.scheduled")
          .bind(Map("check-declaration-details.scheduled" -> "false"))
          .fold(
            _ => "Error",
            {
              case Yes => "yes"
              case No  => "no"
            }
          ) should ===("no")
      }
    }
  }
}
