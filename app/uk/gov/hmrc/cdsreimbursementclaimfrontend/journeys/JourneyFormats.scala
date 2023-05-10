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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import play.api.libs.json.Format
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.MapFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.OrderedMap
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat

import scala.collection.immutable.SortedMap

object JourneyFormats {

  implicit lazy val amountFormat: Format[BigDecimal] =
    SimpleStringFormat[BigDecimal](BigDecimal(_), _.toString())

  implicit lazy val dutyFormat = DutyType.simpleDutyTypeFormat

  implicit def mapFormat1[K : Format, V : Format]: Format[Map[K, V]] =
    MapFormat.format[K, V]

  implicit def mapFormat2[K : Format, V : Format]: Format[Map[K, Option[V]]] =
    MapFormat.formatWithOptionalValue[K, V]

  implicit def mapFormat3[K : Format : Ordering, V : Format]: Format[SortedMap[K, V]] =
    MapFormat.formatSorted[K, V]

  implicit def mapFormat4[K : Format : Ordering, V : Format]: Format[SortedMap[K, Option[V]]] =
    MapFormat.formatSortedWithOptionalValue[K, V]

  implicit def mapFormat5[K : Format : Ordering, V : Format]: Format[SortedMap[DutyType, SortedMap[K, V]]] =
    MapFormat.formatSorted[DutyType, SortedMap[K, V]]

  implicit def mapFormat6[K : Format : Ordering, V : Format]: Format[SortedMap[DutyType, SortedMap[K, Option[V]]]] =
    MapFormat.formatSorted[DutyType, SortedMap[K, Option[V]]]

  implicit def mapFormat7[K : Format, V : Format]: Format[OrderedMap[MRN, Map[K, V]]] =
    MapFormat.formatOrdered[MRN, Map[K, V]]

  implicit def mapFormat8[K : Format, V : Format]: Format[OrderedMap[MRN, Map[K, Option[V]]]] =
    MapFormat.formatOrdered[MRN, Map[K, Option[V]]]

  implicit def mapFormat9[K : Format, V : Format]: Format[OrderedMap[MRN, OrderedMap[K, V]]] =
    MapFormat.formatOrdered[MRN, OrderedMap[K, V]](implicitly[Format[MRN]], MapFormat.formatOrdered[K, V])

  implicit def mapFormat10[K : Format, V : Format]: Format[OrderedMap[MRN, OrderedMap[K, Option[V]]]] =
    MapFormat
      .formatOrdered[MRN, OrderedMap[K, Option[V]]](
        implicitly[Format[MRN]],
        MapFormat.formatOrdered[K, Option[V]](implicitly[Format[K]], Format.optionWithNull[V])
      )

  implicit lazy val mapFormat11: Format[Map[UploadDocumentType, (Nonce, Seq[UploadedFile])]] =
    MapFormat.format[UploadDocumentType, (Nonce, Seq[UploadedFile])]

}
