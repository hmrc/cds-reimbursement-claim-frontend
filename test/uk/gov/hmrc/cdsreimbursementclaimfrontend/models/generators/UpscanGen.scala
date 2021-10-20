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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import cats.data.NonEmptyList
import org.scalacheck.magnolia._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.OptionValues
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.{UploadDetails, UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._

object UpscanGen extends OptionValues {

  def genEvidenceDocumentType: Gen[UploadDocumentType] =
    Gen.oneOf(UploadDocumentType.getListOfEvidenceTypes)

  implicit val arbitraryUploadRequestGen: Typeclass[UploadRequest] = gen[UploadRequest]

  implicit val arbitraryUploadDetails: Typeclass[UploadDetails] = gen[UploadDetails]

  implicit val arbitraryUpscanSuccess: Typeclass[UpscanSuccess] = gen[UpscanSuccess]

  implicit val arbitraryUpscanFailure: Typeclass[UpscanFailure] = gen[UpscanFailure]

  implicit val arbitraryUpscanUploadMeta: Typeclass[UpscanUploadMeta] = gen[UpscanUploadMeta]

  implicit val arbitraryUpscanUpload: Typeclass[UpscanUpload] = gen[UpscanUpload]

  implicit val arbitrarySupportingEvidenceDocumentType: Typeclass[UploadDocumentType] = Arbitrary {
    genEvidenceDocumentType
  }

  implicit val arbitrarySupportingEvidence: Typeclass[UploadDocument] = Arbitrary {
    for {
      uploadReference  <- gen[UploadReference].arbitrary
      upscanUploadMeta <- arbitraryUpscanUploadMeta.arbitrary
      uploadedOn       <- genLocalDateTime
      upscanSuccess    <- arbitraryUpscanSuccess.arbitrary
      name             <- genStringWithMaxSizeOfN(6)
      extension        <- Gen.oneOf("pdf", "doc", "csv")
      documentType     <- arbitrarySupportingEvidenceDocumentType.arbitrary
    } yield UploadDocument(
      uploadReference = uploadReference,
      upscanUploadMeta = upscanUploadMeta,
      uploadedOn = uploadedOn,
      upscanSuccess = upscanSuccess,
      fileName = s"$name.$extension",
      documentType = Some(documentType)
    )
  }

  implicit val arbitrarySupportingEvidenceAnswer: Typeclass[SupportingEvidencesAnswer] = Arbitrary(
    for {
      n         <- Gen.chooseNum(1, 9)
      evidences <- arbitrarySupportingEvidencesAnswerOfN(n).arbitrary
    } yield evidences.value
  )

  def arbitrarySupportingEvidencesAnswerOpt: Typeclass[Option[SupportingEvidencesAnswer]] =
    Arbitrary(Gen.option(arbitrarySupportingEvidenceAnswer.arbitrary))

  def arbitrarySupportingEvidencesAnswerOfN(n: Int): Typeclass[Option[SupportingEvidencesAnswer]] =
    Arbitrary(Gen.listOfN(n, arbitrarySupportingEvidence.arbitrary).map(NonEmptyList.fromList))
}
