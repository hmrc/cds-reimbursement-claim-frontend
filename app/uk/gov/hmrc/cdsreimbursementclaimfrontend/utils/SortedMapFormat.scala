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

import play.api.libs.json.MapWrites.mapWrites
import play.api.libs.json.Reads.mapReads
import play.api.libs.json.Format
import play.api.libs.json.JsResult
import play.api.libs.json.Reads
import play.api.libs.json.Writes

import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import scala.util.Try

object SortedMapFormat {

  def apply[K, V](fromString: String => K, toString: K => String)(implicit
    reads: Reads[V],
    writes: Writes[V],
    ordering: Ordering[K]
  ): Format[SortedMap[K, V]] = {

    def keyValueReads: Reads[SortedMap[K, V]] =
      mapReads[K, V](s => JsResult.fromTry(Try(fromString(s)))).map { entries =>
        TreeMap(entries.toArray: _*)
      }

    def keyValueWrites: Writes[SortedMap[K, V]] =
      mapWrites[V].contramap(_.map { case (k, v) => toString(k) -> v })

    Format(keyValueReads, keyValueWrites)
  }
}
