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
import controllers.Assets.ACCEPT_LANGUAGE
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.i18n.Lang
import play.api.libs.json.JsString
import play.api.test.Helpers.{await, _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimRequest
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.claim.SubmitClaimRequest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.SubmitClaimGen._
import scala.concurrent.ExecutionContext.Implicits.global

class ClaimConnectorSpec extends AnyWordSpec with Matchers with MockFactory with HttpSupport {

  val config: Configuration = Configuration(
    ConfigFactory.parseString(
      """
        | self {
        |   url = host1.com
        |  },
        |  microservice {
        |    services {
        |      cds-reimbursement-claim {
        |        protocol = http
        |        host     = host3
        |        port     = 123
        |      }
        |   }
        |}
        |""".stripMargin
    )
  )

  val connector             = new DefaultClaimConnector(mockHttp, new ServicesConfig(config))
  private val emptyJsonBody = "{}"

  "Claim Connector" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()
    val defaultLanguage            = Lang.defaultLang
    val submitClaimRequest         = sample[SubmitClaimRequest]

    val backEndUrl = "http://host3:123/cds-reimbursement-claim/claim"

    "handling request to submit claim" must {

      "do a post http call and get the TPI-05 API response" in {
        List(
          HttpResponse(200, emptyJsonBody),
          HttpResponse(200, JsString("claim response"), Map[String, Seq[String]]().empty)
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPost(backEndUrl, Seq(ACCEPT_LANGUAGE -> defaultLanguage.language), submitClaimRequest)(
              Right(httpResponse)
            )
            await(connector.submitClaim(submitClaimRequest, defaultLanguage).value) shouldBe Right(httpResponse)
          }
        }
      }
    }

    "return an error" when {
      "internal server error" in {
        List(
          HttpResponse(500, emptyJsonBody)
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPost(backEndUrl, Seq(ACCEPT_LANGUAGE -> defaultLanguage.language), submitClaimRequest)(
              Right(httpResponse)
            )
            await(connector.submitClaim(submitClaimRequest, defaultLanguage).value).isLeft shouldBe true
          }
        }
      }
      "the future fails" in {
        mockPost(backEndUrl, Seq(ACCEPT_LANGUAGE -> defaultLanguage.language), submitClaimRequest)(
          Left(new Throwable("boom"))
        )
        await(connector.submitClaim(submitClaimRequest, defaultLanguage).value).isLeft shouldBe true
      }
    }

  }
}
