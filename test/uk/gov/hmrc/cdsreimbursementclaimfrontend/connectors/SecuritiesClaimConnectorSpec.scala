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
import org.scalacheck.Gen
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.libs.json.Json
import play.api.test.Helpers.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourneyGenerators
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators.Generators.sample
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.Failure
import scala.util.Try
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EvidenceDocument
import java.net.URL
import java.time.ZonedDateTime
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import java.time.Instant
import java.time.ZoneId

class SecuritiesClaimConnectorSpec
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

  val actorSystem = ActorSystem("test-SecuritiesClaimConnector")

  override protected def afterAll(): Unit =
    actorSystem.terminate()

  val connector =
    new SecuritiesClaimConnectorImpl(
      http = mockHttp,
      servicesConfig = new ServicesConfig(config),
      configuration = config,
      actorSystem = actorSystem,
      uploadDocumentsConnector = mockUploadDocumentsConnector
    )

  val expectedUrl = "http://host3:123/foo-claim/claims/securities"

  val requestGen: Gen[SecuritiesClaimConnector.Request] =
    for journey <- SecuritiesJourneyGenerators.completeJourneyGen
    yield SecuritiesClaimConnector.Request(
      journey.toOutput.getOrElse(fail("Could not generate journey output!"))
    )

  val sampleRequest: SecuritiesClaimConnector.Request = sample(requestGen)
  val validResponseBody                               = """{"caseNumber":"ABC123"}"""

  val givenServiceReturns: HttpResponse => CallHandler[Future[HttpResponse]] =
    mockHttpPostSuccess(expectedUrl, Json.toJson(sampleRequest), hasHeaders = true)(_)

  def givenServiceReturnsRequest(
    request: SecuritiesClaimConnector.Request
  ): HttpResponse => CallHandler[Future[HttpResponse]] =
    mockHttpPostSuccess(expectedUrl, Json.toJson(request), hasHeaders = true)(_)

  "SecuritiesClaimConnector" must {
    "have retries defined" in {
      connector.retryIntervals shouldBe Seq(FiniteDuration(10, "ms"), FiniteDuration(50, "ms"))
    }

    "return caseNumber when successful call" in {
      givenServiceReturns(HttpResponse(200, validResponseBody)).once()
      await(connector.submitClaim(sampleRequest, false)) shouldBe SecuritiesClaimConnector.Response("ABC123")
    }

    "throw exception when empty response" in {
      givenServiceReturns(HttpResponse(200, "")).once()
      a[SecuritiesClaimConnector.Exception] shouldBe thrownBy {
        await(connector.submitClaim(sampleRequest, false))
      }
    }

    "throw exception when invalid response" in {
      givenServiceReturns(HttpResponse(200, """{"case":"ABC123"}""")).once()
      a[SecuritiesClaimConnector.Exception] shouldBe thrownBy {
        await(connector.submitClaim(sampleRequest, false))
      }
    }

    "throw exception when invalid success response status" in {
      givenServiceReturns(HttpResponse(201, validResponseBody)).once()
      a[SecuritiesClaimConnector.Exception] shouldBe thrownBy {
        await(connector.submitClaim(sampleRequest, false))
      }
    }

    "throw exception when 4xx response status" in {
      givenServiceReturns(HttpResponse(404, "case not found")).once()
      Try(await(connector.submitClaim(sampleRequest, false))) shouldBe Failure(
        SecuritiesClaimConnector.Exception(
          "Request to POST http://host3:123/foo-claim/claims/securities failed because of HttpResponse status=404 case not found"
        )
      )
    }

    "throw exception when 5xx response status in the third attempt" in {
      givenServiceReturns(HttpResponse(500, "")).once()
      givenServiceReturns(HttpResponse(500, "")).once()
      givenServiceReturns(HttpResponse(500, "")).once()

      a[SecuritiesClaimConnector.Exception] shouldBe thrownBy {
        await(connector.submitClaim(sampleRequest, false))
      }
    }

    "accept valid response in a second attempt" in {
      givenServiceReturns(HttpResponse(500, "")).once()
      givenServiceReturns(HttpResponse(200, validResponseBody)).once()
      await(connector.submitClaim(sampleRequest, false)) shouldBe SecuritiesClaimConnector.Response("ABC123")
    }

    "accept valid response in a third attempt" in {
      givenServiceReturns(HttpResponse(500, "")).once()
      givenServiceReturns(HttpResponse(500, "")).once()
      givenServiceReturns(HttpResponse(200, validResponseBody)).once()
      await(connector.submitClaim(sampleRequest, false)) shouldBe SecuritiesClaimConnector.Response("ABC123")
    }

    "retry claim submission with a free text input extracted as a separate files when 403 FORBIDDEN" in {
      val request      = sampleRequest.copy(claim = sampleRequest.claim.copy(additionalDetails = Some("foo")))
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

      givenServiceReturnsRequest(request)(HttpResponse(403, "forbidden"))
      mockHttpPost(URL(expectedUrl)).once()
      mockRequestBuilderWithBody(
        Json.toJson(
          request.copy(claim =
            request.claim
              .excludeFreeTextInputs()
              ._2
              .copy(supportingEvidences = request.claim.supportingEvidences :+ EvidenceDocument.from(uploadedFile))
          )
        )
      ).once()
      mockRequestBuilderTransform().once()
      mockRequestBuilderExecuteWithoutException(HttpResponse(200, validResponseBody)).once()

      await(connector.submitClaim(request, true)) shouldBe SecuritiesClaimConnector.Response("ABC123")
    }

  }
}
