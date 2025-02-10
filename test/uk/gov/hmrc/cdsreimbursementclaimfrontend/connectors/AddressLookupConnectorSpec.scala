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
import play.api.libs.json.Json
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.AddressLookupConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.lookup.AddressLookupRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.ContactAddressGen.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.http.NotFoundException
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URL
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global

class AddressLookupConnectorSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with HttpV2Support
    with ConnectorSpec {

  val config: Configuration = Configuration(
    ConfigFactory.parseString(
      """
        |self {
        | url = host1.com
        |},
        |microservice {
        |  services {
        |    address-lookup-frontend {
        |      protocol = http
        |      host = localhost
        |      port = 9028
        |      init-endpoint = "/api/init"
        |      address-retrieve-endpoint = "/api/confirmed"
        |    }
        |  }
        |}
        |""".stripMargin
    )
  )

  val servicesConfig = new ServicesConfig(config)

  val connector = new DefaultAddressLookupConnector(mockHttp, new AddressLookupConfig(servicesConfig))

  "The address lookup connector" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "handling requests to submit claim" must {
      val url         = "http://localhost:9028/api/init"
      val request     = sample[AddressLookupRequest]
      val jsonRequest = Json.toJson(request)

      behave like connectorBehaviour(
        mockHttpPostSuccess[HttpResponse](url, jsonRequest)(_),
        mockHttpPostWithException(url, jsonRequest)(new NotFoundException("error")),
        () => connector.initiate(request)
      )
    }

    "Retrieve address" must {
      val uuid = UUID.randomUUID()
      val url  = URL(s"http://localhost:9028/api/confirmed?id=$uuid")

      behave like connectorBehaviour(
        mockHttpGetSuccess[HttpResponse](url)(_),
        mockHttpGetFailure(url)(new NotFoundException("error")),
        () => connector.retrieveAddress(uuid)
      )
    }

  }
}
