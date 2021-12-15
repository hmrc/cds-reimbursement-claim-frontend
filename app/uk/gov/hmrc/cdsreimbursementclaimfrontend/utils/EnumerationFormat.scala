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

import play.api.libs.json.Format
import scala.reflect.ClassTag

/** Utility class to generate Json formatter for enumeration. */
object EnumerationFormat {

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def apply[T : ClassTag](values: Set[T]): Format[T] = {
    val map = values.map(v => (v.toString(), v)).toMap
    SimpleStringFormat(
      key =>
        map
          .get(key)
          .getOrElse(
            throw new IllegalArgumentException(
              s"The [$key] is NOT a value of the enum class ${implicitly[ClassTag[T]].runtimeClass.getName()}."
            )
          ),
      (value: T) => value.toString
    )
  }

}
