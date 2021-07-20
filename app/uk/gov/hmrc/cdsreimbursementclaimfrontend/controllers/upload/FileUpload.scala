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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.upload

import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.{ScheduledDocument, UploadReference}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence.{routes => evidenceRoutes}

trait FileUpload[A] {
  def reviewPage: Call
  def uploadErrorPage: Call
  def handleUploadCallback(uploadReference: UploadReference): Call
  def hasReachedUploadThreshold(maybeAnswer: Option[A]): Boolean
}

@Singleton
class FileUploadServices @Inject() (config: Configuration) {

  implicit object SupportingEvidenceUpload extends FileUpload[SupportingEvidencesAnswer] {
    private lazy val max: Int =
      config.underlying.getInt(s"microservice.services.upscan-initiate.max-uploads")

    def hasReachedUploadThreshold(maybeAnswer: Option[SupportingEvidencesAnswer]): Boolean =
      maybeAnswer.exists(_.length >= max)

    def reviewPage: Call = evidenceRoutes.SupportingEvidenceController.checkYourAnswers()
    def uploadErrorPage: Call = evidenceRoutes.SupportingEvidenceController.handleUpscanErrorRedirect()
    def handleUploadCallback(uploadReference: UploadReference): Call = _
  }

  implicit object ScheduledDocumentUpload extends FileUpload[ScheduledDocument] {
    def hasReachedUploadThreshold(maybeAnswer: Option[ScheduledDocument]): Boolean =
      maybeAnswer.isDefined

    def reviewPage: Call = ???
    def uploadErrorPage: Call = ???
    def handleUploadCallback(uploadReference: UploadReference): Call = ???
  }
}
