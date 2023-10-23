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

import play.api.i18n.Messages
object MethodOfPaymentSummary {
  protected val key = "method-of-payment"

  def apply(methods: Set[String])(implicit messages: Messages): String = {
    val methodStrings = methods.toSeq
      .map(method => messages(s"$key.$method"))

    methodStrings match {
      case Seq(single)  => single
      case init :+ last => s"${init.mkString(", ")} and $last"
      case _            => ""
    }
  }
}
