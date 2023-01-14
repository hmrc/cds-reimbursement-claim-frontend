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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.views.helpers

import cats.implicits.catsSyntaxOptionId
import play.api.i18n.Messages
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable

object MessagesHelper {

  def combine(level1: String, journey: JourneyBindable, level3: String)(implicit messages: Messages): List[String] =
    combine(level1, journey.value.replace(JourneyBindable.Single.value, "").some, level3)

  def combine(level1: String, level2: Option[String], level3: String)(implicit messages: Messages): List[String] = {
    val default    = s"$level1.$level3"
    val keys       = level2 match {
      case Some(l2) => List(s"$level1.$l2.$level3", default)
      case None     => List(default)
    }
    val candidates = keys.filter(key => messages.isDefinedAt(key))
    if (candidates.isEmpty) List(s"cannot find any of messages: ${keys.mkString(",")}")
    else candidates
  }

}
