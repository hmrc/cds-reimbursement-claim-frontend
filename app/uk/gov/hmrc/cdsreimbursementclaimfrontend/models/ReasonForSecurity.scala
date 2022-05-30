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

import cats.syntax.eq._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.EnumerationFormat

sealed class ReasonForSecurity(val acc14Code: String)

object ReasonForSecurity extends EnumerationFormat[ReasonForSecurity] {

  case object AccountSales extends ReasonForSecurity("ACS")
  case object CommunitySystemsOfDutyRelief extends ReasonForSecurity("CSD")
  case object EndUseRelief extends ReasonForSecurity("ENU")
  case object InwardProcessingRelief extends ReasonForSecurity("IPR")
  case object ManualOverrideDeposit extends ReasonForSecurity("MOD")
  case object MissingLicenseQuota extends ReasonForSecurity("MLQ") // FIXME
  case object MissingPreferenceCertificate extends ReasonForSecurity("MPC")
  case object OutwardProcessingRelief extends ReasonForSecurity("OPR")
  case object RevenueDispute extends ReasonForSecurity("RED")
  case object TemporaryAdmission2Y extends ReasonForSecurity("TA24") // (2 years Expiration)
  case object TemporaryAdmission6M extends ReasonForSecurity("TA6") // (6 months Expiration)
  case object TemporaryAdmission3M extends ReasonForSecurity("TA3") // (3 months Expiration)
  case object TemporaryAdmission2M extends ReasonForSecurity("TA2") // (2 months Expiration)
  case object UKAPEntryPrice extends ReasonForSecurity("UEP") // FIXME
  case object UKAPSafeguardDuties extends ReasonForSecurity("USD") // FIXME

  override val values: Set[ReasonForSecurity] =
    Set(
      AccountSales,
      CommunitySystemsOfDutyRelief,
      EndUseRelief,
      InwardProcessingRelief,
      ManualOverrideDeposit,
      MissingLicenseQuota,
      MissingPreferenceCertificate,
      OutwardProcessingRelief,
      RevenueDispute,
      TemporaryAdmission2Y,
      TemporaryAdmission6M,
      TemporaryAdmission3M,
      TemporaryAdmission2M,
      UKAPEntryPrice,
      UKAPSafeguardDuties
    )

  // Set of ReasonForSecurity demanding DEC91 call for export declaration status check
  val requiresExportDeclaration: Set[ReasonForSecurity] =
    Set(
      TemporaryAdmission2Y,
      TemporaryAdmission6M,
      TemporaryAdmission3M,
      TemporaryAdmission2M
    )

  private[models] val basisOfSecuritiesStringMap: Map[String, ReasonForSecurity] =
    values.map(a => a.toString -> a).toMap

  def has(basisOfRejectedGoods: String): Boolean                                 =
    basisOfSecuritiesStringMap.contains(basisOfRejectedGoods)

  def find(basisOfRejectedGoods: String): Option[ReasonForSecurity] =
    basisOfSecuritiesStringMap.get(basisOfRejectedGoods)

  def findUnsafe(basisOfRejectedGoods: String): ReasonForSecurity =
    basisOfSecuritiesStringMap(basisOfRejectedGoods)

  def fromACC14Code(acc14Code: String): Option[ReasonForSecurity] =
    values.find(_.acc14Code === acc14Code)
}
