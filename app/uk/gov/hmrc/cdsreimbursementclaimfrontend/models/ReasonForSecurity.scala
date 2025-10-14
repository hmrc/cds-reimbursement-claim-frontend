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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.syntax.eq.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.EnumerationFormat

sealed trait ReasonForSecurity(val acc14Code: String) {
  val order: Int
}

object ReasonForSecurity extends EnumerationFormat[ReasonForSecurity] {

  case object CommunitySystemsOfDutyRelief extends ReasonForSecurity("MDC") { val order = 7 }
  case object EndUseRelief extends ReasonForSecurity("ENU") { val order = 5 }
  case object InwardProcessingRelief extends ReasonForSecurity("IPR") { val order = 4 }
  case object ManualOverrideDeposit extends ReasonForSecurity("MOD") { val order = 6 }
  case object MissingLicenseQuota extends ReasonForSecurity("MDL") { val order = 9 }
  case object MissingPreferenceCertificate extends ReasonForSecurity("MDP") { val order = 8 }
  case object OutwardProcessingRelief extends ReasonForSecurity("OPR") { val order = 10 }
  case object RevenueDispute extends ReasonForSecurity("RED") { val order = 11 }
  case object TemporaryAdmission2Y extends ReasonForSecurity("T24") { val order = 0 }
  case object TemporaryAdmission6M extends ReasonForSecurity("TA6") { val order = 3 }
  case object TemporaryAdmission3M extends ReasonForSecurity("TA3") { val order = 2 }
  case object TemporaryAdmission2M extends ReasonForSecurity("TA2") { val order = 1 }
  case object UKAPEntryPrice extends ReasonForSecurity("CEP") { val order = 12 }
  case object UKAPSafeguardDuties extends ReasonForSecurity("CSD") { val order = 13 }

  val ntas: Set[ReasonForSecurity] =
    Set(
      TemporaryAdmission2Y,
      TemporaryAdmission6M,
      TemporaryAdmission3M,
      TemporaryAdmission2M
    )

  val niru: Set[ReasonForSecurity] =
    Set(
      InwardProcessingRelief,
      EndUseRelief
    )

  val nidac: Set[ReasonForSecurity] =
    Set(
      MissingPreferenceCertificate,
      MissingLicenseQuota,
      UKAPEntryPrice,
      UKAPSafeguardDuties,
      RevenueDispute,
      ManualOverrideDeposit
    )

  override val values: Set[ReasonForSecurity] = ntas ++ niru ++ nidac

  def has(rfs: String): Boolean =
    valueMap.contains(rfs)

  def find(rfs: String): Option[ReasonForSecurity] =
    valueMap.get(rfs)

  def findUnsafe(rfs: String): ReasonForSecurity =
    valueMap(rfs)

  def fromACC14Code(acc14Code: String): Option[ReasonForSecurity] =
    values.find(_.acc14Code === acc14Code)

  implicit val ordering: Ordering[ReasonForSecurity] =
    Ordering.by[ReasonForSecurity, Int](_.order)
}
