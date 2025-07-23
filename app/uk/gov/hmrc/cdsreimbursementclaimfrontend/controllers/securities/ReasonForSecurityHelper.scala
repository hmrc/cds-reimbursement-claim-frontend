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

import cats.implicits.catsSyntaxEq
import play.api.Configuration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.CommunitySystemsOfDutyRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.OutwardProcessingRelief

class ReasonForSecurityHelper(
  configuration: Configuration
) {

  private lazy val ntasVisibility: String     = configuration.get[String]("features.security-reasons.ntas")
  private lazy val nidacVisibility: String    = configuration.get[String]("features.security-reasons.nidac")
  private lazy val nidacMdpVisibility: String = configuration.get[String]("features.security-reasons.nidac-mdp")
  private lazy val niruVisibility: String     = configuration.get[String]("features.security-reasons.niru")
  private lazy val niruOprVisibility: String  = configuration.get[String]("features.security-reasons.niru-opr")
  private lazy val niruCsdrVisibility: String = configuration.get[String]("features.security-reasons.niru-csdr")

  def avalaibleReasonsForSecurity(): Set[ReasonForSecurity] = getNtasOptions() ++ getNiruOptions() ++ getNidacOptions()

  private def getNtasOptions() =
    if isEnabled(ntasVisibility) then ReasonForSecurity.ntas else Set.empty

  private def getNidacOptions() =
    (if isEnabled(nidacVisibility) then ReasonForSecurity.nidac else Set.empty)
      ++ (if isEnabled(nidacMdpVisibility) then Set(ReasonForSecurity.MissingPreferenceCertificate) else Set.empty)

  private def getNiruOptions() = {
    val options = if isEnabled(niruVisibility) then ReasonForSecurity.niru else Set.empty
    options
      .filterNot(rfs => isOutwardProcessingReliefDisabled(rfs))
      .filterNot(rfs => isCommunitySystemsOfDutyReliefDisabled(rfs))
  }

  private def isOutwardProcessingReliefDisabled(rfs: ReasonForSecurity) =
    if !isEnabled(niruOprVisibility) then rfs === OutwardProcessingRelief else false

  private def isCommunitySystemsOfDutyReliefDisabled(rfs: ReasonForSecurity) =
    if !isEnabled(niruCsdrVisibility) then rfs === CommunitySystemsOfDutyRelief else false

  private def isEnabled(visibility: String): Boolean = visibility match
    case "public"  => true
    case "private" => true
    case _         => false

}
