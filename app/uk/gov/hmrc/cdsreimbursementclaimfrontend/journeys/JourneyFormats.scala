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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import play.api.libs.json.Format
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.MapFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.SimpleStringFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.OrderedMap
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import scala.collection.immutable.SortedMap
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.AmountPaidWithRefund
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce

object JourneyFormats {

  implicit lazy val amountFormat: Format[BigDecimal] =
    SimpleStringFormat[BigDecimal](BigDecimal(_), _.toString())

  implicit lazy val dutyFormat = DutyType.simpleDutyTypeFormat

  implicit lazy val mapFormat1: Format[Map[TaxCode, Option[BigDecimal]]] =
    MapFormat.formatWithOptionalValue[TaxCode, BigDecimal]

  implicit lazy val mapFormat2: Format[Map[UploadDocumentType, (Nonce, Seq[UploadedFile])]] =
    MapFormat.format[UploadDocumentType, (Nonce, Seq[UploadedFile])]

  implicit lazy val mapFormat3: Format[OrderedMap[MRN, Map[TaxCode, Option[BigDecimal]]]] =
    MapFormat.formatOrdered[MRN, Map[TaxCode, Option[BigDecimal]]]

  implicit lazy val mapFormat4: Format[SortedMap[TaxCode, Option[AmountPaidWithRefund]]] =
    MapFormat.formatSortedWithOptionalValue[TaxCode, AmountPaidWithRefund]

  implicit lazy val mapFormat5: Format[SortedMap[DutyType, SortedMap[TaxCode, Option[AmountPaidWithRefund]]]] =
    MapFormat.formatSorted[DutyType, SortedMap[TaxCode, Option[AmountPaidWithRefund]]]

  implicit lazy val mapFormat6: Format[SortedMap[TaxCode, AmountPaidWithRefund]] =
    MapFormat.formatSorted[TaxCode, AmountPaidWithRefund]

  implicit lazy val mapFormat7: Format[SortedMap[DutyType, SortedMap[TaxCode, AmountPaidWithRefund]]] =
    MapFormat.formatSorted[DutyType, SortedMap[TaxCode, AmountPaidWithRefund]]

  implicit lazy val mapFormat8: Format[Map[TaxCode, BigDecimal]] =
    MapFormat.format[TaxCode, BigDecimal]

  implicit lazy val mapFormat9: Format[SortedMap[TaxCode, Option[BigDecimal]]] =
    MapFormat.formatSortedWithOptionalValue[TaxCode, BigDecimal]

  implicit lazy val mapFormat10: Format[SortedMap[String, SortedMap[TaxCode, Option[BigDecimal]]]] =
    MapFormat.formatSorted[String, SortedMap[TaxCode, Option[BigDecimal]]]

  implicit lazy val mapFormat11: Format[SortedMap[TaxCode, BigDecimal]] =
    MapFormat.formatSorted[TaxCode, BigDecimal]

  implicit lazy val mapFormat12: Format[SortedMap[String, SortedMap[TaxCode, BigDecimal]]] =
    MapFormat.formatSorted[String, SortedMap[TaxCode, BigDecimal]]

  implicit lazy val mapFormat13: Format[OrderedMap[MRN, Map[TaxCode, BigDecimal]]] =
    MapFormat.formatOrdered[MRN, Map[TaxCode, BigDecimal]]

}
