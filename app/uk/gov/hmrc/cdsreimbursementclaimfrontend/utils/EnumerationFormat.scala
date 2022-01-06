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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

import cats.Eq
import play.api.libs.json.Format

/** Provides parse, verification, serialization and equality capabilities to the set of case objects of sealed trait T. */
@SuppressWarnings(Array("org.wartremover.warts.Throw"))
trait EnumerationFormat[T] {

  val values: Set[T]

  private final lazy val valueMap: Map[String, T] =
    values.map(v => (v.toString, v)).toMap

  final def parse(key: String): Option[T] =
    valueMap.get(key)

  final def keyOf(value: T): String =
    value.toString

  final def tryParse(key: String): T =
    valueMap.getOrElse(
      key,
      throw new IllegalArgumentException(s"The [$key] is NOT a value of the expected enum class.")
    )

  final def hasKey(key: String): Boolean =
    valueMap.contains(key)

  implicit final val equality: Eq[T] = Eq.fromUniversalEquals[T]

  implicit final val format: Format[T] = SimpleStringFormat(
    key =>
      valueMap.getOrElse(
        key,
        throw new IllegalArgumentException(
          s"The [$key] is NOT a value of the expected enum class."
        )
      ),
    (value: T) => keyOf(value)
  )

  implicit final val enumeration: EnumerationFormat[T] = this
}
