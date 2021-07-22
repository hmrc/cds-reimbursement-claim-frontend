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

import cats.implicits.{catsSyntaxEq, catsSyntaxOptionId}
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.{ScheduledDocument, SupportingEvidence, UploadReference, UpscanUpload}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence.{routes => evidenceRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanSuccess

trait FileUpload[A] {

  val key: String

  val maxUploads: Int

  def hasReachedUploadThreshold(maybeAnswer: Option[A]): Boolean

  def hasReference(maybeAnswer: Option[A], uploadReference: UploadReference): Boolean

  def add(
    upload: UpscanUpload,
    callback: UpscanSuccess,
    maybeAnswer: Option[A],
    claim: FillingOutClaim
  ): FillingOutClaim

  def reviewPage: Call

  def uploadErrorPage: Call

  def handleUploadCallback(uploadReference: UploadReference): Call

  def scanSuccessPage(uploadReference: UploadReference): Call

  def scanErrorPage: Call
}

@Singleton
class FileUploadServices @Inject() (config: Configuration) {

  implicit object SupportingEvidenceUpload extends FileUpload[SupportingEvidencesAnswer] {

    val key: String     = "supporting-evidence"
    val maxUploads: Int = config.readMaxUploadsValue(key)

    def hasReachedUploadThreshold(maybeAnswer: Option[SupportingEvidencesAnswer]): Boolean =
      maybeAnswer.exists(_.length >= maxUploads)

    def hasReference(maybeAnswer: Option[SupportingEvidencesAnswer], uploadReference: UploadReference): Boolean =
      maybeAnswer.exists(_.exists(_.uploadReference === uploadReference))

    def add(
      upload: UpscanUpload,
      callback: UpscanSuccess,
      maybeAnswer: Option[SupportingEvidencesAnswer],
      fillingOutClaim: FillingOutClaim
    ): FillingOutClaim = {

      val newEvidence = SupportingEvidence(
        upload.uploadReference,
        upload.upscanUploadMeta,
        upload.uploadedOn,
        callback,
        callback.fileName,
        None
      )

      val evidences = maybeAnswer.map(_ :+ newEvidence) orElse Some(SupportingEvidencesAnswer(newEvidence))

      FillingOutClaim.of(fillingOutClaim)(_.copy(supportingEvidencesAnswer = evidences))
    }

    def uploadErrorPage: Call = evidenceRoutes.SupportingEvidenceController.handleUpscanErrorRedirect()

    def handleUploadCallback(uploadReference: UploadReference): Call =
      evidenceRoutes.SupportingEvidenceController.scanProgress(uploadReference)

    def scanErrorPage: Call = evidenceRoutes.SupportingEvidenceController.handleUpscanCallBackFailures()

    def scanSuccessPage(uploadReference: UploadReference): Call =
      evidenceRoutes.SupportingEvidenceController.chooseSupportingEvidenceDocumentType(uploadReference)

    def reviewPage: Call = evidenceRoutes.SupportingEvidenceController.checkYourAnswers()
  }

  implicit object ScheduledDocumentUpload extends FileUpload[ScheduledDocument] {

    val key: String     = "schedule-of-mrn"
    val maxUploads: Int = config.readMaxUploadsValue(key)

    def hasReachedUploadThreshold(maybeAnswer: Option[ScheduledDocument]): Boolean =
      maybeAnswer.toList.length >= maxUploads

    def hasReference(maybeAnswer: Option[ScheduledDocument], uploadReference: UploadReference): Boolean =
      maybeAnswer.exists(_.uploadReference === uploadReference)

    def add(
      upload: UpscanUpload,
      callback: UpscanSuccess,
      maybeAnswer: Option[ScheduledDocument],
      fillingOutClaim: FillingOutClaim
    ): FillingOutClaim = {

      val scheduledDocument = ScheduledDocument(
        callback.fileName,
        upload.uploadReference,
        upload.uploadedOn,
        upload.upscanUploadMeta,
        callback
      )

      FillingOutClaim.of(fillingOutClaim)(_.copy(scheduledDocumentAnswer = scheduledDocument.some))
    }

    def uploadErrorPage: Call =
      evidenceRoutes.SupportingEvidenceController.handleUpscanErrorRedirect() // TODO: implement in the next ticket

    def handleUploadCallback(uploadReference: UploadReference): Call =
      evidenceRoutes.SupportingEvidenceController.scanProgress(uploadReference) // TODO: implement in the next ticket

    def scanSuccessPage(uploadReference: UploadReference): Call = ???

    def scanErrorPage: Call = ???

    def reviewPage: Call = ???
  }
}
