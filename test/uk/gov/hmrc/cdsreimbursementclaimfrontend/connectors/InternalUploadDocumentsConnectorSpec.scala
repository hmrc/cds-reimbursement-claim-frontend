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
import play.api.test.Helpers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionSupport
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionModel
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocumentType
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.time.Instant
import java.time.ZoneId
import java.time.ZonedDateTime
import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.routes

class InternalUploadDocumentsConnectorSpec extends AnyWordSpec with Matchers with MockFactory with SessionSupport {

  val config: Configuration = Configuration(
    ConfigFactory.parseString(
      """
        |appName = "foo-123-frontend"
        |self {
        |   url = host1.com
        |}
        |microservice {
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

  val connector =
    new InternalUploadDocumentsConnector(mockSessionCache, new ServicesConfig(config))

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val uploadDocumentsParameters: UploadDocumentsSessionConfig =
    UploadDocumentsSessionConfig(
      continueUrl = "/foo",
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

  val expectedResponse: Some[String] =
    Some(
      s"host1.com${routes.UploadDocumentsController.show().url}"
    )

  "InternalUploadDocumentsConnector" must {

    "configure empty upload session and return redirect URL when successful call" in {
      inSequence {
        mockGetSession(SessionData(journeyStatus = None))
        mockStoreSession(
          SessionData(
            journeyStatus = None,
            uploadDocumentsSessionModel = Some(UploadDocumentsSessionModel(uploadDocumentsParameters, Seq.empty, "abc"))
          )
        )(Right(()))
      }
      await(
        connector.initialize(
          UploadDocumentsConnector.Request(uploadDocumentsParameters, Seq.empty, Some("abc"))
        )
      ) shouldBe expectedResponse
    }

    "configure upload session with existing files and return redirect URL when successful call" in {
      val uploadedFiles = Seq(uploadedFile(1), uploadedFile(2))

      inSequence {
        mockGetSession(SessionData(journeyStatus = None))
        mockStoreSession(
          SessionData(
            journeyStatus = None,
            uploadDocumentsSessionModel =
              Some(UploadDocumentsSessionModel(uploadDocumentsParameters, uploadedFiles, "upload-documents"))
          )
        )(Right(()))
      }

      await(
        connector.initialize(
          UploadDocumentsConnector.Request(uploadDocumentsParameters, uploadedFiles, None)
        )
      ) shouldBe expectedResponse
    }

    "do nothing if session not available" in {
      inSequence {
        mockGetSession(Right(None))
      }
      await(
        connector.initialize(
          UploadDocumentsConnector.Request(uploadDocumentsParameters, Seq.empty, Some("123"))
        )
      ) shouldBe None
    }

  }

  def uploadedFile(id: Int): UploadedFile =
    UploadedFile(
      upscanReference = s"upscan-reference-$id",
      fileName = s"file-name-$id",
      downloadUrl = s"download-url-$id",
      uploadTimestamp = ZonedDateTime.ofInstant(Instant.ofEpochMilli(0L), ZoneId.of("Europe/London")),
      checksum = "A" * 64,
      fileMimeType = s"application/$id",
      fileSize = Some(12345)
    )
}
