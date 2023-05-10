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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import cats.data.NonEmptyList
import org.scalacheck.magnolia._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.OptionValues
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadedFile
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.ScheduledDocumentAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswerList
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.UploadDocumentType

import java.time.ZoneOffset
import java.time.ZonedDateTime

object UpscanGen extends OptionValues {

  def arbitrarySupportingEvidencesAnswerOfN(n: Int): Typeclass[Option[SupportingEvidencesAnswer]] =
    Arbitrary(Gen.listOfN(n, arbitraryUploadedFile.arbitrary).map(NonEmptyList.fromList))

  def arbitrarySupportingEvidencesAnswerListOfN(n: Int): Typeclass[Option[SupportingEvidencesAnswerList]] =
    Arbitrary(Gen.listOfN(n, arbitraryUploadedFile.arbitrary).map(Option(_)))

  lazy val genEvidenceDocumentType: Gen[UploadDocumentType] =
    Gen.oneOf(UploadDocumentType.c285DocumentTypes)

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
      uploadReference <- genStringWithMaxSizeOfN(30)
      name            <- genStringWithMaxSizeOfN(6)
      fileMimeType    <- Gen.oneOf("text/plain", "image/jpeg", "image/png", "application/pdf")
      size            <- Gen.choose(1L, 1000000L)
      checksum        <- genStringWithMaxSizeOfN(32)
      downloadUrl     <- genStringWithMaxSizeOfN(128)
      uploadTimestamp <- Gen.const(java.time.Instant.parse("2011-12-03T10:15:30Z"))
      extension       <- Gen.oneOf("pdf", "doc", "csv")
      documentType    <- arbitrarySupportingEvidenceDocumentType.arbitrary
    } yield UploadedFile(
      upscanReference = uploadReference,
      downloadUrl = downloadUrl,
      uploadTimestamp = ZonedDateTime.ofInstant(uploadTimestamp, ZoneOffset.UTC),
      checksum = checksum,
      fileName = s"$name.$extension",
      fileMimeType = fileMimeType,
      fileSize = Some(size),
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
