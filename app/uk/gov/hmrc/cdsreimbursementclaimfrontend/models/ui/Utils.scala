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

import play.api.templates.PlayMagic.toHtmlArgs
import play.twirl.api.Html

object Utils extends Implicits {

  def toClasses(firstClass: String, rest: String*): String =
    rest.foldLeft(firstClass)((acc, curr) => if (curr.isEmpty) acc else s"$acc $curr")

  def toAttributes(attributes: Map[String, String], padCount: Int = 1): Html = {
    val htmlArgs = toHtmlArgs(attributes.map { case (k, v) => Symbol(k) -> v })
    htmlArgs.padLeft(if (attributes.nonEmpty) padCount else 0)
  }

  object NonEmptyString {
    def unapply(s: String): Option[String] =
      if (s != null && s.nonEmpty) Some(s) else None
  }

  def isNonEmptyOptionString(value: Option[String]) = value match {
    case Some(NonEmptyString(_)) => true
    case _                       => false
  }

}
