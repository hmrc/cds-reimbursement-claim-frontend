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

import com.typesafe.config.ConfigFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.CommunitySystemsOfDutyRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.EndUseRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.InwardProcessingRelief

class ReasonForSecurityHelperSpec extends AnyWordSpec with Matchers {

  "ReasonForSecurityHelper" should {

    "show only public options (ntas), limited access is on and all other reasons are dev-only" in {

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.ntas = public
            | features.security-reasons.niru = dev-only
            | features.security-reasons.niru-opr = dev-only
            | features.security-reasons.niru-csdr = dev-only
            | features.security-reasons.nidac = dev-only
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config,
        limitedSecuritiesAccessEnabled = true,
        userHasSecuritiesAccess = true
      ).avalaibleReasonsForSecurity() shouldBe ReasonForSecurity.ntas

    }

    "show only all options when limited access is off and regardless of the values of flags" in {

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.ntas = private
            | features.security-reasons.niru = dev-only
            | features.security-reasons.niru-opr = dev-only
            | features.security-reasons.niru-csdr = dev-only
            | features.security-reasons.nidac = private
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config,
        limitedSecuritiesAccessEnabled = false,
        userHasSecuritiesAccess = true
      ).avalaibleReasonsForSecurity() shouldBe ReasonForSecurity.values

    }

    "show public (ntas) and private (only the enabled niru options) when limited access is on and user has securities access" in {

      val expectedOptions = Set(EndUseRelief, InwardProcessingRelief) ++ ReasonForSecurity.ntas

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.ntas = public
            | features.security-reasons.niru = private
            | features.security-reasons.niru-opr = dev-only
            | features.security-reasons.niru-csdr = dev-only
            | features.security-reasons.nidac = dev-only
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config,
        limitedSecuritiesAccessEnabled = true,
        userHasSecuritiesAccess = true
      ).avalaibleReasonsForSecurity() shouldBe expectedOptions

    }

    "show public security reasons when limited access is on and user does not have securities access" in {

      val expectedOptions = ReasonForSecurity.ntas

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.ntas = public
            | features.security-reasons.niru = private
            | features.security-reasons.niru-opr = dev-only
            | features.security-reasons.niru-csdr = dev-only
            | features.security-reasons.nidac = dev-only
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config,
        limitedSecuritiesAccessEnabled = true,
        userHasSecuritiesAccess = false
      ).avalaibleReasonsForSecurity() shouldBe expectedOptions

    }

    "show private security reasons when limited access is on and user does not have securities access" in {

      val expectedOptions =
        ReasonForSecurity.niru.filterNot(rfs => rfs == CommunitySystemsOfDutyRelief) ++ ReasonForSecurity.nidac

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.ntas = dev-only
            | features.security-reasons.niru = private
            | features.security-reasons.niru-opr = private
            | features.security-reasons.niru-csdr = dev-only
            | features.security-reasons.nidac = private
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config,
        limitedSecuritiesAccessEnabled = true,
        userHasSecuritiesAccess = true
      ).avalaibleReasonsForSecurity() shouldBe expectedOptions

    }

    "show no options  when flag values are not recognised" in {

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.ntas = foo
            | features.security-reasons.niru = privaterrrr
            | features.security-reasons.niru-opr = bar
            | features.security-reasons.niru-csdr = blah
            | features.security-reasons.nidac = poublic
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config,
        limitedSecuritiesAccessEnabled = true,
        userHasSecuritiesAccess = true
      ).avalaibleReasonsForSecurity() shouldBe Set.empty

    }

  }
}
