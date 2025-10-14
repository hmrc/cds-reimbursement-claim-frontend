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

class ReasonForSecurityHelperSpec extends AnyWordSpec with Matchers {

  "ReasonForSecurityHelper" should {

    "show all but nidac reasons for security, except for MDP RfS" in {

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.nidac.mdp.enabled = on
            | features.security-reasons.nidac.mdl.enabled = off
            | features.security-reasons.nidac.cep.enabled = off
            | features.security-reasons.nidac.csd.enabled = off
            | features.security-reasons.nidac.red.enabled = off
            | features.security-reasons.nidac.mod.enabled = off
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config
      ).availableReasonsForSecurity() shouldBe ReasonForSecurity.ntas
        ++ ReasonForSecurity.niru
        ++ Set(ReasonForSecurity.MissingPreferenceCertificate)
    }

    "show all but nidac reasons for security" in {

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.nidac.mdp.enabled = off
            | features.security-reasons.nidac.mdl.enabled = off
            | features.security-reasons.nidac.cep.enabled = off
            | features.security-reasons.nidac.csd.enabled = off
            | features.security-reasons.nidac.red.enabled = off
            | features.security-reasons.nidac.mod.enabled = off
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config
      ).availableReasonsForSecurity() shouldBe ReasonForSecurity.ntas
        ++ ReasonForSecurity.niru
    }

    "show all reasons for security" in {

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.nidac.mdp.enabled = on
            | features.security-reasons.nidac.mdl.enabled = on
            | features.security-reasons.nidac.cep.enabled = on
            | features.security-reasons.nidac.csd.enabled = on
            | features.security-reasons.nidac.red.enabled = on
            | features.security-reasons.nidac.mod.enabled = on
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config
      ).availableReasonsForSecurity() shouldBe ReasonForSecurity.ntas
        ++ ReasonForSecurity.niru
        ++ ReasonForSecurity.nidac

    }
  }

}
