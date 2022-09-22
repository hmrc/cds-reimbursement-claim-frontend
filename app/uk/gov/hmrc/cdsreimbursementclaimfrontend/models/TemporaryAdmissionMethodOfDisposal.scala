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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.EnumerationFormat

sealed trait TemporaryAdmissionMethodOfDisposal

object TemporaryAdmissionMethodOfDisposal extends EnumerationFormat[TemporaryAdmissionMethodOfDisposal] {

  case object ExportedInSingleShipment extends TemporaryAdmissionMethodOfDisposal
  case object ExportedInMultipleShipments extends TemporaryAdmissionMethodOfDisposal
  case object DeclaredToOtherTraderUnderTemporaryAdmission extends TemporaryAdmissionMethodOfDisposal
  case object DeclaredToFreeCirculation extends TemporaryAdmissionMethodOfDisposal
  case object DeclaredToInwardProcessingRelief extends TemporaryAdmissionMethodOfDisposal
  case object DeclaredToEndUse extends TemporaryAdmissionMethodOfDisposal
  case object DeclaredToAFreeZone extends TemporaryAdmissionMethodOfDisposal
  case object DeclaredToACustomsWarehouse extends TemporaryAdmissionMethodOfDisposal
  case object Destroyed extends TemporaryAdmissionMethodOfDisposal
  case object MultipleDisposalMethodsWereUsed extends TemporaryAdmissionMethodOfDisposal
  case object Other extends TemporaryAdmissionMethodOfDisposal

  override val values: Set[TemporaryAdmissionMethodOfDisposal] =
    Set(
      ExportedInSingleShipment,
      ExportedInMultipleShipments,
      DeclaredToOtherTraderUnderTemporaryAdmission,
      DeclaredToFreeCirculation,
      DeclaredToInwardProcessingRelief,
      DeclaredToEndUse,
      DeclaredToAFreeZone,
      DeclaredToACustomsWarehouse,
      Destroyed,
      Other,
      MultipleDisposalMethodsWereUsed
    )

  val exportedMethodsOfDisposal: Set[TemporaryAdmissionMethodOfDisposal] =
    Set(ExportedInSingleShipment, ExportedInMultipleShipments)
}
