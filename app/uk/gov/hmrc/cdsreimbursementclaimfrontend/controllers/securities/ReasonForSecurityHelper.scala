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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import play.api.Configuration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity

class ReasonForSecurityHelper(
  configuration: Configuration
) {

  private lazy val nidacMdpVisibility: Boolean =
    configuration.getOptional[Boolean]("features.security-reasons.nidac.mdp.enabled").getOrElse(false)
  private lazy val nidacMdlVisibility: Boolean =
    configuration.getOptional[Boolean]("features.security-reasons.nidac.mdl.enabled").getOrElse(false)
  private lazy val nidacCepVisibility: Boolean =
    configuration.getOptional[Boolean]("features.security-reasons.nidac.cep.enabled").getOrElse(false)
  private lazy val nidacCsdVisibility: Boolean =
    configuration.getOptional[Boolean]("features.security-reasons.nidac.csd.enabled").getOrElse(false)
  private lazy val nidacRedVisibility: Boolean =
    configuration.getOptional[Boolean]("features.security-reasons.nidac.red.enabled").getOrElse(false)
  private lazy val nidacModVisibility: Boolean =
    configuration.getOptional[Boolean]("features.security-reasons.nidac.mod.enabled").getOrElse(false)

  def availableReasonsForSecurity(): Set[ReasonForSecurity] = getNtasOptions() ++ getNiruOptions() ++ getNidacOptions()

  private def getNtasOptions() = ReasonForSecurity.ntas

  private def getNidacOptions() =
    Set(
      if nidacMdpVisibility then Some(ReasonForSecurity.MissingPreferenceCertificate) else None,
      if nidacMdlVisibility then Some(ReasonForSecurity.MissingLicenseQuota) else None,
      if nidacCepVisibility then Some(ReasonForSecurity.UKAPEntryPrice) else None,
      if nidacCsdVisibility then Some(ReasonForSecurity.UKAPSafeguardDuties) else None,
      if nidacRedVisibility then Some(ReasonForSecurity.RevenueDispute) else None,
      if nidacModVisibility then Some(ReasonForSecurity.ManualOverrideDeposit) else None
    ).flatten

  private def getNiruOptions() = ReasonForSecurity.niru

}
