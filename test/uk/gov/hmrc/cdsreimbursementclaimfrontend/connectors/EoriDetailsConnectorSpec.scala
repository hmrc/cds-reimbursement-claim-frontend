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
import org.apache.pekko.actor.ActorSystem
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.Failure
import scala.util.Try

class EoriDetailsConnectorSpec
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
        |        host     = host3
        |        port     = 123
        |        retryIntervals = [10ms,50ms] 
        |        context-path = "/foo-claim"
        |      }
        |   }
        |}
        |""".stripMargin
    )
  )

  val actorSystem = ActorSystem("test-EoriDetailsConnector")

  override protected def afterAll(): Unit =
    actorSystem.terminate()

  val connector =
    new DefaultEoriDetailsConnector(mockHttp, new ServicesConfig(config), config, actorSystem)

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val expectedUrl = "http://host3:123/foo-claim/eori"

  val givenServiceReturns: Option[HttpResponse] => CallHandler[Future[HttpResponse]] =
    mockGet(expectedUrl)(_)

  "EoriDetailsConnector" must {
    "have retries defined" in {
      connector.retryIntervals shouldBe Seq(FiniteDuration(10, "ms"), FiniteDuration(50, "ms"))
    }

    "return details when 200 with eoriXI" in {
      givenServiceReturns(
        Some(HttpResponse(200, """{"eoriGB":"GB0123456789","eoriXI": "XI0123456789","fullName":"Foo Bar"}"""))
      ).once()
      await(connector.getEoriDetails) shouldBe Some(
        EoriDetailsConnector
          .Response(
            eoriGB = Eori("GB0123456789"),
            eoriXI = Some(Eori("XI0123456789")),
            fullName = "Foo Bar",
            eoriEndDate = None
          )
      )
    }

    "return details when 200 without eoriXI" in {
      givenServiceReturns(
        Some(HttpResponse(200, """{"eoriGB":"GB0123456789","eoriXI":null,"fullName":"Foo Bar"}"""))
      ).once()
      await(connector.getEoriDetails) shouldBe Some(
        EoriDetailsConnector
          .Response(
            eoriGB = Eori("GB0123456789"),
            eoriXI = None,
            fullName = "Foo Bar",
            eoriEndDate = None
          )
      )
    }

    "return empty when 204" in {
      givenServiceReturns(
        Some(HttpResponse(204, """{"eoriGB":"GB0123456789","eoriXI": "XI0123456789","fullName":"Foo Bar"}"""))
      ).once()
      await(connector.getEoriDetails) shouldBe None
    }

    "throw exception when empty response" in {
      givenServiceReturns(Some(HttpResponse(200, ""))).once()
      a[EoriDetailsConnector.Exception] shouldBe thrownBy {
        await(connector.getEoriDetails)
      }
    }

    "throw exception when invalid response" in {
      givenServiceReturns(Some(HttpResponse(200, """{"eoriGB":"ABC123"}"""))).once()
      a[EoriDetailsConnector.Exception] shouldBe thrownBy {
        await(connector.getEoriDetails)
      }
    }

    "throw exception when invalid success response status" in {
      givenServiceReturns(
        Some(HttpResponse(201, """{"eoriGB":"GB0123456789","eoriXI": "XI0123456789","fullName":"Foo Bar"}"""))
      ).once()
      a[EoriDetailsConnector.Exception] shouldBe thrownBy {
        await(connector.getEoriDetails)
      }
    }

    "throw exception when 4xx response status" in {
      givenServiceReturns(Some(HttpResponse(404, "case not found"))).once()
      Try(await(connector.getEoriDetails)) shouldBe Failure(
        new EoriDetailsConnector.Exception(
          "Request to POST http://host3:123/foo-claim/eori failed because of 404 case not found"
        )
      )
    }

    "throw exception when 5xx response status in the third attempt" in {
      givenServiceReturns(Some(HttpResponse(500, ""))).repeat(3)
      givenServiceReturns(
        Some(HttpResponse(200, """{"eoriGB":"GB0123456789","eoriXI": "XI0123456789","fullName":"Foo Bar"}"""))
      ).never()
      a[EoriDetailsConnector.Exception] shouldBe thrownBy {
        await(connector.getEoriDetails)
      }
    }

    "accept valid response in a second attempt" in {
      givenServiceReturns(Some(HttpResponse(500, ""))).once()
      givenServiceReturns(Some(HttpResponse(200, """{"eoriGB":"GB0123456789","eoriXI":null,"fullName":"Foo Bar"}""")))
        .once()
      await(connector.getEoriDetails) shouldBe Some(
        EoriDetailsConnector
          .Response(
            eoriGB = Eori("GB0123456789"),
            eoriXI = None,
            fullName = "Foo Bar",
            eoriEndDate = None
          )
      )
    }

    "accept valid response in a third attempt" in {
      givenServiceReturns(Some(HttpResponse(500, ""))).repeat(2)
      givenServiceReturns(Some(HttpResponse(200, """{"eoriGB":"GB0123456789","eoriXI":null,"fullName":"Foo Bar"}""")))
        .once()
      await(connector.getEoriDetails) shouldBe Some(
        EoriDetailsConnector
          .Response(
            eoriGB = Eori("GB0123456789"),
            eoriXI = None,
            fullName = "Foo Bar",
            eoriEndDate = None
          )
      )
    }

  }
}
