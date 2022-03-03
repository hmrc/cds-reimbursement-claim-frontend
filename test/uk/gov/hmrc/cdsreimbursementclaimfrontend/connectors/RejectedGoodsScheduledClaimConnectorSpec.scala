/*
 * Copyright 2022 HM Revenue & Customs
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

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsScheduledJourneyGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import play.api.test.Helpers._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.util.Try
import scala.util.Failure
import org.scalamock.handlers.CallHandler
import scala.concurrent.Future

class RejectedGoodsScheduledClaimConnectorSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with HttpSupport
    with BeforeAndAfterAll {

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
        |        host     = host-2
        |        port     = 312
        |        retryIntervals = [5ms,25ms] 
        |        context-path = "/foo-claim-scheduled"
        |      }
        |   }
        |}
        |""".stripMargin
    )
  )

  val actorSystem = ActorSystem("test-RejectedGoodsScheduledClaimConnector")

  override protected def afterAll(): Unit =
    actorSystem.terminate()

  val connector =
    new RejectedGoodsScheduledClaimConnector(mockHttp, new ServicesConfig(config), config, actorSystem)

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val expectedUrl = "http://host-2:312/foo-claim-scheduled/claims/rejected-goods-scheduled"

  val requestGen = for {
    journey <- RejectedGoodsScheduledJourneyGenerators.completeJourneyGen
  } yield RejectedGoodsScheduledClaimConnector.Request(
    journey.toOutput.getOrElse(fail("Could not generate journey output!"))
  )

  val sampleRequest: RejectedGoodsScheduledClaimConnector.Request = sample(requestGen)
  val validResponseBody                                           = """{"caseNumber":"ABC312"}"""

  val givenServiceReturns: Option[HttpResponse] => CallHandler[Future[HttpResponse]] =
    mockPost(expectedUrl, Seq("Accept-Language" -> "en"), sampleRequest) _

  "RejectedGoodsScheduledClaimConnector" must {
    "have retries defined" in {
      connector.retryIntervals shouldBe Seq(FiniteDuration(5, "ms"), FiniteDuration(25, "ms"))
    }

    "return caseNumber when successful call" in {
      givenServiceReturns(Some(HttpResponse(200, validResponseBody))).once()
      await(connector.submitClaim(sampleRequest)) shouldBe RejectedGoodsScheduledClaimConnector.Response("ABC312")
    }

    "throw exception when empty response" in {
      givenServiceReturns(Some(HttpResponse(200, ""))).once()
      a[RejectedGoodsScheduledClaimConnector.Exception] shouldBe thrownBy {
        await(connector.submitClaim(sampleRequest))
      }
    }

    "throw exception when invalid response" in {
      givenServiceReturns(Some(HttpResponse(200, """{"case":"ABC312"}"""))).once()
      a[RejectedGoodsScheduledClaimConnector.Exception] shouldBe thrownBy {
        await(connector.submitClaim(sampleRequest))
      }
    }

    "throw exception when invalid success response status" in {
      givenServiceReturns(Some(HttpResponse(201, validResponseBody))).once()
      a[RejectedGoodsScheduledClaimConnector.Exception] shouldBe thrownBy {
        await(connector.submitClaim(sampleRequest))
      }
    }

    "throw exception when 4xx response status" in {
      givenServiceReturns(Some(HttpResponse(404, "case not found"))).once()
      Try(await(connector.submitClaim(sampleRequest))) shouldBe Failure(
        new RejectedGoodsScheduledClaimConnector.Exception(
          "Request to POST http://host-2:312/foo-claim-scheduled/claims/rejected-goods-scheduled failed because of HttpResponse status=404 case not found"
        )
      )
    }

    "throw exception when 5xx response status in the third attempt" in {
      givenServiceReturns(Some(HttpResponse(500, ""))).repeat(3)
      givenServiceReturns(Some(HttpResponse(200, validResponseBody))).never()
      a[RejectedGoodsScheduledClaimConnector.Exception] shouldBe thrownBy {
        await(connector.submitClaim(sampleRequest))
      }
    }

    "accept valid response in a second attempt" in {
      givenServiceReturns(Some(HttpResponse(500, ""))).once()
      givenServiceReturns(Some(HttpResponse(200, validResponseBody))).once()
      await(connector.submitClaim(sampleRequest)) shouldBe RejectedGoodsScheduledClaimConnector.Response("ABC312")
    }

    "accept valid response in a third attempt" in {
      givenServiceReturns(Some(HttpResponse(500, ""))).repeat(2)
      givenServiceReturns(Some(HttpResponse(200, validResponseBody))).once()
      await(connector.submitClaim(sampleRequest)) shouldBe RejectedGoodsScheduledClaimConnector.Response("ABC312")
    }

  }
}
