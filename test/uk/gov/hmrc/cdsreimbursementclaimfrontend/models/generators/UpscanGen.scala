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
import collection.JavaConverters._
import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.{UploadDetails, UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.supportingevidence.SupportingEvidenceController.SupportingEvidenceAnswers

object UpscanGen extends GenUtils {

  implicit val signedInUserDetailsGen: Gen[SignedInUserDetails] = gen[SignedInUserDetails]

  implicit val supportingEvidenceGen: Gen[SupportingEvidence] =
    gen[SupportingEvidence]

  implicit val supportingEvidenceAnswersGen: Gen[SupportingEvidenceAnswers] =
    Gen.chooseNum(1, 9).flatMap(genSupportingEvidenceAnswersOfN)

  implicit val uploadRequestGen: Gen[UploadRequest] = gen[UploadRequest]

  implicit val upscanUploadGen: Gen[UpscanUpload] = gen[UpscanUpload]

  implicit val upscanSuccessGen: Gen[UpscanSuccess] = gen[UpscanSuccess]

  implicit val upscanFailureGen: Gen[UpscanFailure] = gen[UpscanFailure]

  implicit val upscanUploadMetaGen: Gen[UpscanUploadMeta] = gen[UpscanUploadMeta]

  implicit val supportingDocumentTypeGen: Gen[SupportingEvidenceDocumentType] = gen[SupportingEvidenceDocumentType]

  implicit val uploadDetailsGen: Gen[UploadDetails] = gen[UploadDetails]

  def genSupportingEvidenceAnswersOfN(n: Int): Gen[SupportingEvidenceAnswers] =
    Gen.listOfN(n, supportingEvidenceGen).map(NonEmptyList.fromList)

  def genSupportingEvidenceAnswersOf(gen: Gen[SupportingEvidence]*): Gen[SupportingEvidenceAnswers] =
    Gen.sequence(gen).map(evidences => NonEmptyList.fromList(evidences.asScala.toList))

  def genSupportingEvidenceAnswersOpt: Gen[SupportingEvidenceAnswers] =
    Gen.option(Gen.nonEmptyListOf(supportingEvidenceGen)).map(_.flatMap(NonEmptyList.fromList))
}
