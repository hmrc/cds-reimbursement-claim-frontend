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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.EnumerationFormat
import uk.gov.hmrc.govukfrontend.views.Aliases.RadioItem
import uk.gov.hmrc.govukfrontend.views.Aliases.Text

sealed trait ExportMethod

object ExportMethod extends EnumerationFormat[ExportMethod] {
  case object SingleShipment extends ExportMethod
  case object MultipleShipments extends ExportMethod
  case object DeclaredToOtherTraderTemporaryAdmission extends ExportMethod
  case object DeclaredToFreeCirculation extends ExportMethod
  case object DeclaredToInwardProcessingRelief extends ExportMethod
  case object DeclaredToEndUse extends ExportMethod
  case object DeclaredToFreeZone extends ExportMethod
  case object DeclaredToCustomsWarehouse extends ExportMethod
  case object Destroyed extends ExportMethod
  case object Other extends ExportMethod
  case object MoreExportMethods extends ExportMethod

  override val values: Set[ExportMethod] = Set(
    SingleShipment,
    MultipleShipments,
    DeclaredToOtherTraderTemporaryAdmission,
    DeclaredToFreeCirculation,
    DeclaredToInwardProcessingRelief,
    DeclaredToEndUse,
    DeclaredToFreeZone,
    DeclaredToCustomsWarehouse,
    Destroyed,
    Other,
    MoreExportMethods
  )

  val orderedValues: List[ExportMethod] = List[ExportMethod](
    SingleShipment,
    MultipleShipments,
    DeclaredToOtherTraderTemporaryAdmission,
    DeclaredToFreeCirculation,
    DeclaredToInwardProcessingRelief,
    DeclaredToEndUse,
    DeclaredToFreeZone,
    DeclaredToCustomsWarehouse,
    Destroyed,
    Other,
    MoreExportMethods
  )
}
