/*
 * Copyright 2025 HM Revenue & Customs
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

import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.EvidenceDocument
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentsSessionConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Nonce
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.UploadDocumentsConnector.FileToUpload
import java.nio.charset.StandardCharsets

trait WafErrorMitigationHelper {

  val uploadDocumentsConnector: UploadDocumentsConnector

  def uploadFreeTextsAsSeparateFiles(freeTexts: Seq[(String, String)])(using
    HeaderCarrier,
    ExecutionContext
  ): Future[Seq[EvidenceDocument]] =
    uploadDocumentsConnector
      .initialize(
        UploadDocumentsConnector
          .Request(
            config = uploadDocumentsSessionConfig,
            existingFiles = Seq.empty
          )
      )
      .flatMap { _ =>
        Future
          .sequence(
            freeTexts
              .filter(_._2.trim().nonEmpty)
              .map((prefix, text) =>
                uploadDocumentsConnector
                  .uploadFile(
                    FileToUpload(
                      uploadId = s"$prefix",
                      name = s"$prefix.txt",
                      contentType = "text/plain",
                      content = text.getBytes(StandardCharsets.UTF_8)
                    )
                  )
                  .map(
                    _.map(uf =>
                      EvidenceDocument
                        .from(uf)
                        .copy(fileMimeType = "text/plain")
                    )
                  )
              )
          )
          .map(_.collect { case Some(x) => x })
      }

  private def uploadDocumentsSessionConfig: UploadDocumentsSessionConfig =
    UploadDocumentsSessionConfig(
      nonce = Nonce.random,
      continueUrl = "https://www.gov.uk",
      continueWhenFullUrl = "https://www.gov.uk",
      callbackUrl = "https://www.gov.uk",
      minimumNumberOfFiles = 1,
      maximumNumberOfFiles = 1,
      initialNumberOfEmptyRows = 1,
      maximumFileSizeBytes = 10000000,
      allowedContentTypes = "text/plain",
      allowedFileExtensions = ".txt",
      cargo = None,
      newFileDescription = None,
      content = uploadDocumentsContent,
      features = UploadDocumentsSessionConfig.Features(
        showUploadMultiple = true,
        showLanguageSelection = true,
        showAddAnotherDocumentButton = false,
        showYesNoQuestionBeforeContinue = false,
        enableMultipleFilesPicker = false
      )
    )

  private def uploadDocumentsContent =
    UploadDocumentsSessionConfig.Content(
      serviceName = "",
      title = "",
      descriptionHtml = "",
      serviceUrl = "",
      accessibilityStatementUrl = "",
      phaseBanner = "beta",
      phaseBannerUrl = "https://www.gov.uk",
      signOutUrl = "https://www.gov.uk",
      timedOutUrl = "https://www.gov.uk",
      keepAliveUrl = "https://www.gov.uk",
      timeoutSeconds = 0,
      countdownSeconds = 0,
      pageTitleClasses = "",
      allowedFilesTypesHint = "",
      fileUploadedProgressBarLabel = "",
      chooseFirstFileLabel = "",
      fileUploadRequiredError = None
    )

}
