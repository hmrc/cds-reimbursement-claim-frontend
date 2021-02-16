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

import org.scalacheck.Gen
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.SupportingEvidenceAnswers.{CompleteSupportingEvidenceAnswers, IncompleteSupportingEvidenceAnswers}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.{UploadDetails, UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import org.scalacheck.ScalacheckShapeless._

object UpscanGen extends GenUtils {

  implicit val signedInUserDetailsGen: Gen[SignedInUserDetails] = gen[SignedInUserDetails]

  implicit val completeUploadSupportingEvidenceAnswersGen: Gen[CompleteSupportingEvidenceAnswers] =
    gen[CompleteSupportingEvidenceAnswers]

  implicit val incompleteUploadSupportingEvidenceAnswersGen: Gen[IncompleteSupportingEvidenceAnswers] =
    gen[IncompleteSupportingEvidenceAnswers]

  implicit val supportingEvidenceGen: Gen[SupportingEvidence] =
    gen[SupportingEvidence]

  implicit val uploadRequestGen: Gen[UploadRequest] = gen[UploadRequest]

  implicit val upscanUploadGen: Gen[UpscanUpload] = gen[UpscanUpload]

  implicit val upscanSuccessGen: Gen[UpscanSuccess] = gen[UpscanSuccess]

  implicit val upscanFailureGen: Gen[UpscanFailure] = gen[UpscanFailure]

  implicit val upscanUploadMetaGen: Gen[UpscanUploadMeta] = gen[UpscanUploadMeta]

  implicit val supportingDocumentTypeGen: Gen[SupportingEvidenceDocumentType] = gen[SupportingEvidenceDocumentType]

  implicit val uploadDetailsGen: Gen[UploadDetails] = gen[UploadDetails]
}
