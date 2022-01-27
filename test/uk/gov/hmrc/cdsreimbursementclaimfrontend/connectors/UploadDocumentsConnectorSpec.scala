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
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.UploadDocumentsConfig
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

class UploadDocumentsConnectorSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with HttpSupport
    with BeforeAndAfterAll {

  val config: Configuration = Configuration(
    ConfigFactory.parseString(
      """
        |appName = "foo-123-frontend"
        |self {
        |   url = host1.com
        |  },
        |  microservice {
        |    services {
        |       upload-documents-frontend {
        |        protocol = http
        |        host     = host3
        |        port     = 124
        |        retryIntervals = [15ms,30ms] 
        |        context-path = "/foo-upload"
        |      }
        |   }
        |}
        |""".stripMargin
    )
  )

  val actorSystem = ActorSystem("test-UploadDocumentsConnectorSpec")

  override protected def afterAll(): Unit =
    actorSystem.terminate()

  val connector =
    new UploadDocumentsConnectorImpl(
      mockHttp,
      new UploadDocumentsConfig(new ServicesConfig(config), config),
      config,
      actorSystem
    )

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val expectedUrl = "http://host3:124/foo-upload/initialize"

  val uploadDocumentsParameters: UploadDocumentsSessionConfig =
    UploadDocumentsSessionConfig(
      continueUrl = "/foo",
      backlinkUrl = "/bar",
      callbackUrl = "/baz",
      nonce = Nonce.random,
      cargo = UploadDocumentType.LetterOfAuthority,
      newFileDescription = "New file",
      continueWhenFullUrl = "/ful",
      maximumNumberOfFiles = 5,
      initialNumberOfEmptyRows = 1,
      content = UploadDocumentsSessionConfig.Content(
        serviceName = "service.title",
        title = "choose-files.rejected-goods.title",
        descriptionHtml = "descriptionHtml",
        serviceUrl = "homePageUrl",
        accessibilityStatementUrl = "accessibilityStatementUrl",
        phaseBanner = "alpha",
        phaseBannerUrl = "serviceFeedBackUrl",
        signOutUrl = "signOutUrl",
        timedOutUrl = "ggTimedOutUrl",
        keepAliveUrl = "ggKeepAliveUrl",
        timeoutSeconds = 900,
        countdownSeconds = 120,
        showLanguageSelection = false,
        pageTitleClasses = "govuk-heading-xl"
      )
    )

  val initializationRequest: UploadDocumentsConnector.Request =
    UploadDocumentsConnector.Request(uploadDocumentsParameters, Seq.empty)

  val givenServiceReturns: Option[HttpResponse] => CallHandler[Future[HttpResponse]] =
    mockPost(
      expectedUrl,
      Seq.empty,
      UploadDocumentsConnector.Request(
        uploadDocumentsParameters
          .copy(serviceId = Some("foo-123-frontend")),
        Seq.empty
      )
    ) _

  val responseHeaders: Map[String, Seq[String]] =
    Map("Location" -> Seq("http://foo.bar/zoo"))

  val expectedResponse: Some[String]            =
    Some("http://foo.bar/zoo")

  "UploadDocumentsConnector" must {
    "have retries defined" in {
      connector.uploadDocumentsConfig.retryIntervals shouldBe Seq(FiniteDuration(15, "ms"), FiniteDuration(30, "ms"))
    }

    "return caseNumber when successful call" in {
      givenServiceReturns(Some(HttpResponse(201, "", responseHeaders))).once()
      await(connector.initialize(initializationRequest)) shouldBe expectedResponse
    }

    "throw exception when invalid success response status" in {
      givenServiceReturns(Some(HttpResponse(200, ""))).once()
      a[Exception] shouldBe thrownBy {
        await(connector.initialize(initializationRequest))
      }
    }

    "throw exception when 4xx response status" in {
      givenServiceReturns(Some(HttpResponse(404, "case not found"))).once()
      a[Exception] shouldBe thrownBy {
        await(connector.initialize(initializationRequest))
      }
    }

    "throw exception when 5xx response status in the third attempt" in {
      givenServiceReturns(Some(HttpResponse(500, ""))).repeat(3)
      givenServiceReturns(Some(HttpResponse(200, ""))).never()
      a[Exception] shouldBe thrownBy {
        await(connector.initialize(initializationRequest))
      }
    }

    "accept valid response in a second attempt" in {
      givenServiceReturns(Some(HttpResponse(500, ""))).once()
      givenServiceReturns(Some(HttpResponse(201, "", responseHeaders))).once()
      await(connector.initialize(initializationRequest)) shouldBe expectedResponse
    }

    "accept valid response in a third attempt" in {
      givenServiceReturns(Some(HttpResponse(500, ""))).repeat(2)
      givenServiceReturns(Some(HttpResponse(201, "", responseHeaders))).once()
      await(connector.initialize(initializationRequest)) shouldBe expectedResponse
    }

  }
}
