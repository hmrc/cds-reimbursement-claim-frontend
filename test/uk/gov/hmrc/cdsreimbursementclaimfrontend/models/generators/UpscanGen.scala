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
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.magnolia._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.{UploadDetails, UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import org.scalatest.OptionValues
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidenceAnswer

object UpscanGen extends OptionValues {

  implicit val arbitraryUploadRequestGen: Typeclass[UploadRequest] = gen[UploadRequest]

  implicit val arbitraryUploadDetails: Typeclass[UploadDetails] = gen[UploadDetails]

  implicit val arbitraryUpscanSuccess: Typeclass[UpscanSuccess] = gen[UpscanSuccess]

  implicit val arbitraryUpscanFailure: Typeclass[UpscanFailure] = gen[UpscanFailure]

  implicit val arbitraryUpscanUploadMeta: Typeclass[UpscanUploadMeta] = gen[UpscanUploadMeta]

  implicit val arbitraryUpscanUpload: Typeclass[UpscanUpload] = gen[UpscanUpload]

  implicit val arbitrarySupportingDocumentType: Typeclass[SupportingEvidenceDocumentType] =
    gen[SupportingEvidenceDocumentType]

  implicit val arbitrarySupportingEvidence: Typeclass[SupportingEvidence] = gen[SupportingEvidence]

  implicit val arbitrarySupportingEvidenceAnswer: Typeclass[SupportingEvidenceAnswer] = Arbitrary(
    for {
      n         <- Gen.chooseNum(1, 9)
      evidences <- arbitrarySupportingEvidenceAnswerOfN(n).arbitrary
    } yield evidences.value
  )

  def arbitrarySupportingEvidenceAnswerOpt: Typeclass[Option[SupportingEvidenceAnswer]] =
    Arbitrary(Gen.option(arbitrarySupportingEvidenceAnswer.arbitrary))

  def arbitrarySupportingEvidenceAnswerOfN(n: Int): Typeclass[Option[SupportingEvidenceAnswer]] =
    Arbitrary(Gen.listOfN(n, arbitrarySupportingEvidence.arbitrary).map(NonEmptyList.fromList))
}
