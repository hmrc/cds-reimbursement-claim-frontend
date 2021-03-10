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
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.test.Helpers.{await, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Eori, Error}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global

class DefaultCustomsDataStoreConnectorSpec extends AnyWordSpec with MockFactory with HttpSupport {

  val config: Configuration = Configuration(
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
        |        email-by-eori = "/customs-data-store/eori/:eori/verified-email"
        |    }
        |   }
        |}
        |""".stripMargin
    )
  )

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val connector = new DefaultCustomsDataStoreConnector(mockHttp, new ServicesConfig(config))

  "Get Email By Eori" must {

    val testEori = Eori("GB123456789012")
    val url      = s"http://localhost:9893/customs-data-store/eori/${testEori.value}/verified-email"

    "handle good responses" in {
      val httpResponse = HttpResponse(200, "The Response")
      mockGet(url)(Right(httpResponse))
      val declaration  = await(connector.getEmailByEori(testEori).value)
      declaration shouldBe Right(httpResponse)
    }

    "handle no email responses" in {
      val httpResponse = HttpResponse(404, "No Email")
      mockGet(url)(Right(httpResponse))
      val declaration  = await(connector.getEmailByEori(testEori).value)
      declaration shouldBe Right(httpResponse)
    }

    "handle internal server error" in {
      val httpResponse = HttpResponse(500, "Ups")
      mockGet(url)(Right(httpResponse))
      val declaration  = await(connector.getEmailByEori(testEori).value)
      declaration shouldBe Right(httpResponse)
    }

    "fail when there is an error" in {
      val error       = new Exception("Socket connection error")
      mockGet(url)(Left(error))
      val declaration = await(connector.getEmailByEori(testEori).value)
      declaration shouldBe Left(Error(error))
    }
  }

}
