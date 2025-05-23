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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.http.NotFoundException
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URL
import scala.concurrent.ExecutionContext.Implicits.global

class VerifiedEmailAddressConnectorSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with HttpV2Support
    with ConnectorSpec {

  val configuration: Configuration = Configuration(
    ConfigFactory.parseString(
      """
        | self {
        |   url = host1.com
        |  },
        |  microservice {
        |    services {
        |    customs-data-store {
        |        protocol = http
        |        host = localhost
        |        port = 9893
        |        email-by-eori = "/customs-data-store/eori/verified-email"
        |    }
        |   }
        |}
        |""".stripMargin
    )
  )

  val connector = new DefaultVerifiedEmailAddressConnector(mockHttp, new ServicesConfig(configuration))

  "Custom Data Store Connector" must {

    val eori = sample[Eori]
    val url  = URL(s"http://localhost:9893/customs-data-store/eori/verified-email")

    "handling requests to submit claim" must {
      behave like connectorBehaviour(
        mockHttpGetSuccess[HttpResponse](url)(_),
        mockHttpGetFailure(url)(new NotFoundException("error")),
        () => connector.getVerifiedEmailAddress(eori)
      )
    }

  }

}
