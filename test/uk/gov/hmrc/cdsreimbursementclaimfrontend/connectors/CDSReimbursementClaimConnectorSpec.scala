/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.libs.json.JsValue
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.JsValueGen._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global

class CDSReimbursementClaimConnectorSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with HttpSupport
    with ConnectorSpec {

  val config: Configuration      = Configuration(
    ConfigFactory.parseString(
      """
        | self {
        |   url = host1.com
        |  },
        |  microservice {
        |    services {
        |      cds-reimbursement-claim {
        |        protocol = http
        |        host     = localhost
        |        port     = 7501
        |      }
        |      bank-account-reputation {
        |        protocol = http
        |        host = localhost
        |        port = 9871
        |        business = /business/v2/assess
        |        personal = /personal/v3/assess
        |     }
        |
        |   }
        |}
        |""".stripMargin
    )
  )
  implicit val hc: HeaderCarrier = HeaderCarrier()

  val connector = new DefaultCDSReimbursementClaimConnector(mockHttp, new ServicesConfig(config))

  "CDS Reimbursement Connector" when {

    val mrn = sample[MRN]
    val url = s"http://localhost:7501/cds-reimbursement-claim/declaration/${mrn.value}"

    "handling requests to get a declaration" must {
      behave like connectorBehaviour(
        mockGet(url)(_),
        () => connector.getDeclaration(mrn)
      )
    }

    "handling requests to verify a business bank account" must {
      val url     = "http://localhost:9871/business/v2/assess"
      val jsValue = sample[JsValue]

      behave like connectorBehaviour(
        mockPost(url, Seq.empty, *)(_),
        () => connector.getBusinessReputation(jsValue)
      )
    }

    "handling requests to verify a personal bank account" must {
      val url     = "http://localhost:9871/personal/v3/assess"
      val jsValue = sample[JsValue]

      behave like connectorBehaviour(
        mockPost(url, Seq.empty, *)(_),
        () => connector.getPersonalReputation(jsValue)
      )
    }
  }
}
