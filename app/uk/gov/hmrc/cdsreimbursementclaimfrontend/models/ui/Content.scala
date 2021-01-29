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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ui

import play.twirl.api.{Html, HtmlFormat}

sealed trait Content {
  def asHtml: Html

  def nonEmpty: Boolean = this match {
    case NonEmptyHtml(_) | NonEmptyText(_) => true
    case _                                 => false
  }
}

object Content {}

case object Empty extends Content {
  override def asHtml: Html = HtmlFormat.empty
}

final case class HtmlContent(value: Html) extends Content {
  override def asHtml: Html = value
}

object HtmlContent {
  def apply(string: String): HtmlContent =
    HtmlContent(Html(string))
}

object NonEmptyHtml {
  def unapply(htmlContent: HtmlContent): Option[Html] =
    if (htmlContent.value.body.isEmpty) None else Some(htmlContent.value)
}

final case class Text(value: String) extends Content {
  override def asHtml: Html = HtmlFormat.escape(value)
}

object NonEmptyText {
  def unapply(text: Text): Option[String] =
    if (text.value == null || text.value.isEmpty) None else Some(text.value)
}
