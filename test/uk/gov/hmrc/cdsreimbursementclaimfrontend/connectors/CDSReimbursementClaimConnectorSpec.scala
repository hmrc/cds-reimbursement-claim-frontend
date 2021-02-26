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
import play.api.libs.json.JsString
import play.api.test.Helpers.{await, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global

class CDSReimbursementClaimConnectorSpec extends AnyWordSpec with MockFactory with HttpSupport {

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

  "DeclarationInfoConnector" when {

    val mrn        = MRN("21GBIDMSXBLNR06016")
    val backEndUrl = s"http://localhost:7501/cds-reimbursement-claim/declaration/${mrn.value}"

    "handling request for declaration" must {
      "do a post http call and get the ACC14 API response" in {
        val httpResponse = HttpResponse(200, "The Response")
        mockGet(backEndUrl)(Right(httpResponse))
        val declaration  = await(connector.getDeclarationDetails(mrn).value)
        declaration shouldBe Right(httpResponse)
      }
    }

    "return an error" when {
      "the call fails" in {
        val error       = new Exception("Socket connection error")
        mockGet(backEndUrl)(Left(error))
        val declaration = await(connector.getDeclarationDetails(mrn).value)
        declaration shouldBe Left(Error(error))
      }
    }
  }

  "Business Bank Account Reputation" must {

    val url         = s"http://localhost:9871/business/v2/assess"
    val testRequest = JsString("Hello")

    "handle good responses" in {
      val httpResponse = HttpResponse(200, "The Response")
      mockPost(url, Seq.empty, *)(Right(httpResponse))
      val declaration  = await(connector.getBusinessReputation(testRequest).value)
      declaration shouldBe Right(httpResponse)
    }

    "fail when there is an error" in {
      val error       = new Exception("Socket connection error")
      mockPost(url, Seq.empty, *)(Left(error))
      val declaration = await(connector.getBusinessReputation(testRequest).value)
      declaration shouldBe Left(Error(error))
    }
  }

  "Personal Bank Account Reputation" must {

    val url         = s"http://localhost:9871/personal/v3/assess"
    val testRequest = JsString("Hello")

    "handle good responses" in {
      val httpResponse = HttpResponse(200, "The Response")
      mockPost(url, Seq.empty, *)(Right(httpResponse))
      val declaration  = await(connector.getPersonalReputation(testRequest).value)
      declaration shouldBe Right(httpResponse)
    }

    "fail when there is an error" in {
      val error       = new Exception("Socket connection error")
      mockPost(url, Seq.empty, *)(Left(error))
      val declaration = await(connector.getPersonalReputation(testRequest).value)
      declaration shouldBe Left(Error(error))
    }
  }
}
