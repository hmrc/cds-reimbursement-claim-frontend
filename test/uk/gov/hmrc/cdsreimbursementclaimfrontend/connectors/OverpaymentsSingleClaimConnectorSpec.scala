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
import play.api.libs.json.Json
import play.api.test.Helpers.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourneyGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EvidenceDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URL
import java.time.Instant
import java.time.ZoneId
import java.time.ZonedDateTime
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.Failure
import scala.util.Try

class OverpaymentsSingleClaimConnectorSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with HttpV2Support
    with BeforeAndAfterAll
    with WafErrorMitigationTestHelper {

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

  val actorSystem = ActorSystem("test-OverpaymentsSingleClaimConnector")

  override protected def afterAll(): Unit =
    actorSystem.terminate()

  val connector =
    new OverpaymentsSingleClaimConnectorImpl(
      http = mockHttp,
      servicesConfig = new ServicesConfig(config),
      configuration = config,
      actorSystem = actorSystem,
      uploadDocumentsConnector = mockUploadDocumentsConnector
    )

  val expectedUrl = "http://host3:123/foo-claim/claims/overpayments-single"

  val requestGen =
    for journey <- OverpaymentsSingleJourneyGenerators.completeJourneyGen
    yield OverpaymentsSingleClaimConnector.Request(
      journey.toOutput.getOrElse(fail("Could not generate journey output!"))
    )

  val sampleRequest: OverpaymentsSingleClaimConnector.Request = sample(requestGen)
  val validResponseBody                                       = """{"caseNumber":"ABC123"}"""

  def givenServiceReturns: HttpResponse => CallHandler[Future[HttpResponse]] =
    mockHttpPostSuccess(expectedUrl, Json.toJson(sampleRequest), hasHeaders = true)(_)

  "OverpaymentsSingleClaimConnector" must {
    "have retries defined" in {
      connector.retryIntervals shouldBe Seq(FiniteDuration(10, "ms"), FiniteDuration(50, "ms"))
    }

    "return caseNumber when successful call" in {
      givenServiceReturns(HttpResponse(200, validResponseBody)).once()
      await(connector.submitClaim(sampleRequest, false)) shouldBe OverpaymentsSingleClaimConnector.Response("ABC123")
    }

    "throw exception when empty response" in {
      givenServiceReturns(HttpResponse(200, "")).once()
      a[OverpaymentsSingleClaimConnector.Exception] shouldBe thrownBy {
        await(connector.submitClaim(sampleRequest, false))
      }
    }

    "throw exception when invalid response" in {
      givenServiceReturns(HttpResponse(200, """{"case":"ABC123"}""")).once()
      a[OverpaymentsSingleClaimConnector.Exception] shouldBe thrownBy {
        await(connector.submitClaim(sampleRequest, false))
      }
    }

    "throw exception when invalid success response status" in {
      givenServiceReturns(HttpResponse(201, validResponseBody)).once()
      a[OverpaymentsSingleClaimConnector.Exception] shouldBe thrownBy {
        await(connector.submitClaim(sampleRequest, false))
      }
    }

    "throw exception when 404 response status" in {
      givenServiceReturns(HttpResponse(404, "case not found")).once()
      Try(await(connector.submitClaim(sampleRequest, false))) shouldBe Failure(
        new OverpaymentsSingleClaimConnector.Exception(
          "Request to POST http://host3:123/foo-claim/claims/overpayments-single failed because of HttpResponse status=404 case not found"
        )
      )
    }

    "throw exception when 403 response status" in {
      givenServiceReturns(HttpResponse(403, "forbidden")).once()
      Try(await(connector.submitClaim(sampleRequest, false))) shouldBe Failure(
        new OverpaymentsSingleClaimConnector.Exception(
          "Request to POST http://host3:123/foo-claim/claims/overpayments-single failed because of HttpResponse status=403 forbidden"
        )
      )
    }

    "throw exception when 5xx response status in the third attempt" in {
      givenServiceReturns(HttpResponse(500, "")).once()
      givenServiceReturns(HttpResponse(500, "")).once()
      givenServiceReturns(HttpResponse(500, "")).once()

      a[OverpaymentsSingleClaimConnector.Exception] shouldBe thrownBy {
        await(connector.submitClaim(sampleRequest, false))
      }
    }

    "accept valid response in a second attempt" in {
      givenServiceReturns(HttpResponse(500, "")).once()
      givenServiceReturns(HttpResponse(200, validResponseBody)).once()
      await(connector.submitClaim(sampleRequest, false)) shouldBe OverpaymentsSingleClaimConnector.Response("ABC123")
    }

    "accept valid response in a third attempt" in {
      givenServiceReturns(HttpResponse(500, "")).once()
      givenServiceReturns(HttpResponse(500, "")).once()
      givenServiceReturns(HttpResponse(200, validResponseBody)).once()
      await(connector.submitClaim(sampleRequest, false)) shouldBe OverpaymentsSingleClaimConnector.Response("ABC123")
    }

    "retry claim submission with a free text input extracted as a separate files when 403 FORBIDDEN" in {
      val uploadedFile = UploadedFile(
        upscanReference = s"upscan-reference-123",
        fileName = s"test.txt",
        downloadUrl = s"https://foo.bar/test.txt",
        uploadTimestamp = ZonedDateTime.ofInstant(Instant.ofEpochMilli(0L), ZoneId.of("Europe/London")),
        checksum = "A" * 64,
        fileMimeType = s"text/plain",
        fileSize = Some(12)
      )
      mockInitializeCall().once()
      mockUploadFileCall(uploadedFile).once()

      givenServiceReturns(HttpResponse(403, "forbidden"))
      mockHttpPost(URL(expectedUrl)).once()
      mockRequestBuilderWithBody(
        Json.toJson(
          sampleRequest.copy(claim =
            sampleRequest.claim
              .excludeFreeTextInputs()
              ._2
              .copy(supportingEvidences =
                sampleRequest.claim.supportingEvidences :+ EvidenceDocument.from(uploadedFile)
              )
          )
        )
      ).once()
      mockRequestBuilderTransform().once()
      mockRequestBuilderExecuteWithoutException(HttpResponse(200, validResponseBody)).once()

      await(connector.submitClaim(sampleRequest, true)) shouldBe OverpaymentsSingleClaimConnector.Response("ABC123")
    }
  }

  import OverpaymentsSingleClaimConnector.*

  "serialize and deserialize request" in {
    val request      = sampleRequest
    val serialized   = Json.toJson(request)
    val deserialized = serialized.as[Request]
    deserialized shouldBe request
  }

  "serialize and deserialize response" in {
    val response     = Response("ABC123")
    val serialized   = Json.toJson(response)
    val deserialized = serialized.as[Response]
    deserialized shouldBe response
  }

}
