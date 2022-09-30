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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.UploadDocumentsConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
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
    new ExternalUploadDocumentsConnector(
      mockHttp,
      new UploadDocumentsConfig(new ServicesConfig(config), config),
      config,
      actorSystem
    )

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val expectedInitializationUrl = "http://host3:124/internal/initialize"
  val expectedWipeOutUrl        = "http://host3:124/internal/wipe-out"

  val uploadDocumentsParameters: UploadDocumentsSessionConfig =
    UploadDocumentsSessionConfig(
      continueUrl = "/foo",
      backlinkUrl = "/bar",
      callbackUrl = "/baz",
      nonce = Nonce.random,
      cargo = Some(UploadDocumentType.LetterOfAuthority),
      newFileDescription = Some("New file"),
      continueAfterYesAnswerUrl = Some("/yes"),
      continueWhenFullUrl = "/ful",
      minimumNumberOfFiles = 0,
      maximumNumberOfFiles = 5,
      initialNumberOfEmptyRows = 1,
      maximumFileSizeBytes = 10L * 1024L * 1024L,
      allowedContentTypes = "image/png",
      allowedFileExtensions = "*.png",
      content = UploadDocumentsSessionConfig
        .Content(
          serviceName = "service.title",
          title = "choose-files.rejected-goods.title",
          descriptionHtml = "descriptionHtml",
          serviceUrl = "homePageUrl",
          accessibilityStatementUrl = "accessibilityStatementUrl",
          phaseBanner = "beta",
          phaseBannerUrl = "serviceFeedBackUrl",
          signOutUrl = "signOutUrl",
          timedOutUrl = "ggTimedOutUrl",
          keepAliveUrl = "ggKeepAliveUrl",
          timeoutSeconds = 900,
          countdownSeconds = 120,
          pageTitleClasses = "govuk-heading-xl",
          allowedFilesTypesHint = "PNG",
          fileUploadedProgressBarLabel = "uploadfileUploadedProgressBarLabeled",
          chooseFirstFileLabel = "chooseFirstFileLabel",
          chooseNextFileLabel = Some("chooseNextFileLabel"),
          addAnotherDocumentButtonText = Some("addAnotherDocumentButtonText"),
          yesNoQuestionText = Some("yesNoQuestionText"),
          yesNoQuestionRequiredError = Some("yesNoQuestionRequiredError")
        ),
      features = UploadDocumentsSessionConfig
        .Features(
          showUploadMultiple = true,
          showLanguageSelection = false,
          showAddAnotherDocumentButton = false,
          showYesNoQuestionBeforeContinue = true
        )
    )

  val initializationRequest: UploadDocumentsConnector.Request =
    UploadDocumentsConnector.Request(uploadDocumentsParameters, Seq.empty, None)

  val givenInitializationCallReturns: Option[HttpResponse] => CallHandler[Future[HttpResponse]] =
    mockPost(
      expectedInitializationUrl,
      Seq.empty,
      UploadDocumentsConnector.Request(
        uploadDocumentsParameters,
        Seq.empty,
        None
      )
    ) _

  val givenWipeOutCallReturns: Option[HttpResponse] => CallHandler[Future[HttpResponse]] =
    mockPost(
      expectedWipeOutUrl,
      Seq.empty,
      ""
    ) _

  val responseHeaders: Map[String, Seq[String]] =
    Map("Location" -> Seq("http://foo.bar/zoo"))

  val expectedResponse: Some[String]            =
    Some("http://foo.bar/zoo")

  "UploadDocumentsConnector" must {
    "have retries defined" in {
      connector.uploadDocumentsConfig.retryIntervals shouldBe Seq(FiniteDuration(15, "ms"), FiniteDuration(30, "ms"))
    }

    "return redirect URL when successful call" in {
      givenInitializationCallReturns(Some(HttpResponse(201, "", responseHeaders))).once()
      await(connector.initialize(initializationRequest)) shouldBe expectedResponse
    }

    "throw exception when invalid success response status" in {
      givenInitializationCallReturns(Some(HttpResponse(200, ""))).once()
      a[Exception] shouldBe thrownBy {
        await(connector.initialize(initializationRequest))
      }
    }

    "throw exception when 4xx response status" in {
      givenInitializationCallReturns(Some(HttpResponse(404, "case not found"))).once()
      a[Exception] shouldBe thrownBy {
        await(connector.initialize(initializationRequest))
      }
    }

    "throw exception when 5xx response status in the third attempt" in {
      givenInitializationCallReturns(Some(HttpResponse(500, ""))).repeat(3)
      givenInitializationCallReturns(Some(HttpResponse(200, ""))).never()
      a[Exception] shouldBe thrownBy {
        await(connector.initialize(initializationRequest))
      }
    }

    "accept valid response in a second attempt" in {
      givenInitializationCallReturns(Some(HttpResponse(500, ""))).once()
      givenInitializationCallReturns(Some(HttpResponse(201, "", responseHeaders))).once()
      await(connector.initialize(initializationRequest)) shouldBe expectedResponse
    }

    "accept valid response in a third attempt" in {
      givenInitializationCallReturns(Some(HttpResponse(500, ""))).repeat(2)
      givenInitializationCallReturns(Some(HttpResponse(201, "", responseHeaders))).once()
      await(connector.initialize(initializationRequest)) shouldBe expectedResponse
    }

    "successfully call wipe-out endpoint" in {
      givenWipeOutCallReturns(Some(HttpResponse(204, "", responseHeaders))).once()
      await(connector.wipeOut) shouldBe (())
    }

    "do not break on error when calling wipe-out endpoint" in {
      givenWipeOutCallReturns(Some(HttpResponse(466, "", responseHeaders))).once()
      await(connector.wipeOut) shouldBe (())
    }

    "retry wipe-out call" in {
      givenWipeOutCallReturns(Some(HttpResponse(501, "", responseHeaders))).repeat(3)
      await(connector.wipeOut) shouldBe (())
    }

  }
}
