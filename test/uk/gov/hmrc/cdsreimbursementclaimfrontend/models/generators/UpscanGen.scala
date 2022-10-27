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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import cats.data.NonEmptyList
import org.scalacheck.magnolia._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.OptionValues
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ScheduledDocumentAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswerList
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UploadDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanFailure
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanSuccess
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile

import java.time.ZonedDateTime
import java.time.ZoneOffset

object UpscanGen extends OptionValues {

  def arbitrarySupportingEvidencesAnswerOfN(n: Int): Typeclass[Option[SupportingEvidencesAnswer]] =
    Arbitrary(Gen.listOfN(n, arbitraryUploadedFile.arbitrary).map(NonEmptyList.fromList))

  def arbitrarySupportingEvidencesAnswerListOfN(n: Int): Typeclass[Option[SupportingEvidencesAnswerList]] =
    Arbitrary(Gen.listOfN(n, arbitraryUploadedFile.arbitrary).map(Option(_)))

  lazy val genEvidenceDocumentType: Gen[UploadDocumentType] =
    Gen.oneOf(UploadDocumentType.c285DocumentTypes)

  implicit lazy val arbitraryUploadRequestGen: Typeclass[UploadRequest] = gen[UploadRequest]

  implicit lazy val arbitraryUploadDetails: Typeclass[UploadDetails] = gen[UploadDetails]

  implicit lazy val arbitraryUpscanSuccess: Typeclass[UpscanSuccess] = gen[UpscanSuccess]

  implicit lazy val arbitraryUpscanFailure: Typeclass[UpscanFailure] = gen[UpscanFailure]

  implicit lazy val arbitraryUpscanUploadMeta: Typeclass[UpscanUploadMeta] = gen[UpscanUploadMeta]

  implicit lazy val arbitraryUpscanUpload: Typeclass[UpscanUpload] = gen[UpscanUpload]

  implicit lazy val arbitrarySupportingEvidenceDocumentType: Typeclass[UploadDocumentType] =
    Arbitrary(genEvidenceDocumentType)

  implicit lazy val arbitrarySupportingEvidenceAnswer: Typeclass[SupportingEvidencesAnswer] = Arbitrary(
    for {
      n         <- Gen.chooseNum(1, 9)
      evidences <- arbitrarySupportingEvidencesAnswerOfN(n).arbitrary
    } yield evidences.value
  )

  implicit lazy val arbitrarySupportingEvidenceAnswerList: Typeclass[SupportingEvidencesAnswerList] = Arbitrary(
    for {
      n         <- Gen.chooseNum(1, 9)
      evidences <- arbitrarySupportingEvidencesAnswerListOfN(n).arbitrary
    } yield evidences.value
  )

  implicit lazy val arbitrarySupportingEvidencesAnswerOpt: Typeclass[Option[SupportingEvidencesAnswer]] =
    Arbitrary(Gen.option(arbitrarySupportingEvidenceAnswer.arbitrary))

  implicit lazy val arbitrarySupportingEvidencesAnswerListOpt: Typeclass[Option[SupportingEvidencesAnswerList]] =
    Arbitrary(Gen.option(arbitrarySupportingEvidenceAnswerList.arbitrary))

  implicit lazy val arbitraryUploadedFile: Typeclass[UploadedFile] = Arbitrary {
    for {
      uploadReference <- gen[UploadReference].arbitrary
      upscanSuccess   <- arbitraryUpscanSuccess.arbitrary
      name            <- genStringWithMaxSizeOfN(6)
      extension       <- Gen.oneOf("pdf", "doc", "csv")
      documentType    <- arbitrarySupportingEvidenceDocumentType.arbitrary
    } yield UploadedFile(
      upscanReference = uploadReference.value,
      downloadUrl = upscanSuccess.downloadUrl,
      uploadTimestamp = ZonedDateTime.ofInstant(upscanSuccess.uploadDetails.uploadTimestamp, ZoneOffset.UTC),
      checksum = upscanSuccess.uploadDetails.checksum,
      fileName = s"$name.$extension",
      fileMimeType = upscanSuccess.uploadDetails.fileMimeType,
      fileSize = Some(upscanSuccess.uploadDetails.size),
      cargo = Some(documentType)
    )
  }

  lazy val genScheduledDocument: Gen[ScheduledDocumentAnswer] =
    arbitraryUploadedFile.arbitrary.map(doc =>
      ScheduledDocumentAnswer(doc.copy(cargo = Some(UploadDocumentType.ScheduleOfMRNs)))
    )

  implicit lazy val arbitraryScheduledDocumentAnswer: Typeclass[ScheduledDocumentAnswer] =
    Arbitrary(genScheduledDocument)
}
