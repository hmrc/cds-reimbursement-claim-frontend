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

  private lazy val nidacMdpVisibility: String =
    configuration.getOptional[String]("features.security-reasons.nidac-only-mdp").getOrElse("all")

  def avalaibleReasonsForSecurity(): Set[ReasonForSecurity] = getNtasOptions() ++ getNiruOptions() ++ getNidacOptions()

  private def getNtasOptions() = ReasonForSecurity.ntas

  private def getNidacOptions() =
    if isEnabled(nidacMdpVisibility) then Set(ReasonForSecurity.MissingPreferenceCertificate)
    else ReasonForSecurity.nidac

  private def getNiruOptions() = ReasonForSecurity.niru

  private def isEnabled(visibility: String): Boolean = visibility match
    case "public"  => true
    case "private" => true
    case "on"      => true
    case _         => false

}
