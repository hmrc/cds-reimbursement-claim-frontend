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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.EndUseRelief
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity.InwardProcessingRelief

class ReasonForSecurityHelperSpec extends AnyWordSpec with Matchers {

  "ReasonForSecurityHelper" should {

    "show only public options (ntas) when all other reasons are hidden" in {

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.ntas = public
            | features.security-reasons.niru = hidden
            | features.security-reasons.niru-opr = hidden
            | features.security-reasons.niru-csdr = hidden
            | features.security-reasons.nidac = hidden
            | features.security-reasons.nidac-mdp = hidden
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config,
        userHasSecuritiesAccess = true
      ).avalaibleReasonsForSecurity() shouldBe ReasonForSecurity.ntas

    }

    "show only all options when user has private beta access to all" in {

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.ntas = private
            | features.security-reasons.niru = private
            | features.security-reasons.niru-opr = private
            | features.security-reasons.niru-csdr = private
            | features.security-reasons.nidac = private
            | features.security-reasons.nidac-mdp = hidden
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config,
        userHasSecuritiesAccess = true
      ).avalaibleReasonsForSecurity() shouldBe ReasonForSecurity.values

    }

    "show public (ntas) and private (only the enabled niru options) when user has private beta access" in {

      val expectedOptions = Set(EndUseRelief, InwardProcessingRelief) ++ ReasonForSecurity.ntas

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.ntas = public
            | features.security-reasons.niru = private
            | features.security-reasons.niru-opr = hidden
            | features.security-reasons.niru-csdr = hidden
            | features.security-reasons.nidac = hidden
            | features.security-reasons.nidac-mdp = hidden
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config,
        userHasSecuritiesAccess = true
      ).avalaibleReasonsForSecurity() shouldBe expectedOptions

    }

    "show public (ntas, nidac-mdp)" in {

      val expectedOptions = Set(ReasonForSecurity.MissingPreferenceCertificate) ++ ReasonForSecurity.ntas

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.ntas = public
            | features.security-reasons.niru = hidden
            | features.security-reasons.niru-opr = hidden
            | features.security-reasons.niru-csdr = hidden
            | features.security-reasons.nidac = hidden
            | features.security-reasons.nidac-mdp = public
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config,
        userHasSecuritiesAccess = false
      ).avalaibleReasonsForSecurity() shouldBe expectedOptions

    }

    "show public ntas and private nidac-mdp" in {

      val expectedOptions = Set(ReasonForSecurity.MissingPreferenceCertificate) ++ ReasonForSecurity.ntas

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.ntas = public
            | features.security-reasons.niru = hidden
            | features.security-reasons.niru-opr = hidden
            | features.security-reasons.niru-csdr = hidden
            | features.security-reasons.nidac = hidden
            | features.security-reasons.nidac-mdp = private
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config,
        userHasSecuritiesAccess = true
      ).avalaibleReasonsForSecurity() shouldBe expectedOptions

    }

    "show only public (ntas) when user does not have private beta access" in {

      val expectedOptions = ReasonForSecurity.ntas

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.ntas = public
            | features.security-reasons.niru = private
            | features.security-reasons.niru-opr = hidden
            | features.security-reasons.niru-csdr = hidden
            | features.security-reasons.nidac = hidden
            | features.security-reasons.nidac-mdp = hidden
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config,
        userHasSecuritiesAccess = false
      ).avalaibleReasonsForSecurity() shouldBe expectedOptions

    }

    "show no options when flag values are not recognised" in {

      val config = Configuration(
        ConfigFactory.parseString(
          """
            | features.security-reasons.ntas = foo
            | features.security-reasons.niru = privaterrrr
            | features.security-reasons.niru-opr = bar
            | features.security-reasons.niru-csdr = blah
            | features.security-reasons.nidac = poublic
            | features.security-reasons.nidac-mdp = hiden
            |""".stripMargin
        )
      )

      new ReasonForSecurityHelper(
        configuration = config,
        userHasSecuritiesAccess = true
      ).avalaibleReasonsForSecurity() shouldBe Set.empty

    }

  }
}
