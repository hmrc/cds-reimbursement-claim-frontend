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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

/** Methods to validate existence of [[Option]]s. */
@SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.Equals"))
object OptionsValidator {

  def all(options: Option[Any]*): Boolean =
    options.forall(_.isDefined)

  def any(options: Option[Any]*): Boolean =
    options.exists(_.isDefined)

  def none(options: Option[Any]*): Boolean =
    options.forall(_.isEmpty)

  def allOrNone(options: Option[Any]*): Boolean =
    options.forall(_.isDefined) || options.forall(_.isEmpty)

  def onlyOne(options: Option[Any]*): Boolean =
    options.count(_.isDefined) == 1

  def requiredWhen(cond: Boolean)(option: Option[Any]): Boolean =
    !cond || option.isDefined

  def nonEmptyMap(option: Option[Map[_, _]]*): Boolean =
    option.exists(_.nonEmpty)

  implicit def asOptionAny(bool: Boolean): Option[Any] =
    if (bool) Some(()) else None

}
