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

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.SupportingEvidencesAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanSuccess
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanUpload

trait FileUpload[A] {

  def addToClaim(
    claim: FillingOutClaim,
    answer: A,
    upscanUpload: UpscanUpload,
    upscanCallBack: UpscanSuccess
  ): FillingOutClaim

  def removeFromClaim(claim: FillingOutClaim, document: A): FillingOutClaim

}

object FileUpload {

  implicit object SupportingEvidenceUpload extends FileUpload[SupportingEvidencesAnswer] {

    override def addToClaim(
      claim: FillingOutClaim,
      answer: SupportingEvidencesAnswer,
      upscanUpload: UpscanUpload,
      upscanCallBack: UpscanSuccess
    ): FillingOutClaim = ???

    override def removeFromClaim(claim: FillingOutClaim, document: SupportingEvidencesAnswer): FillingOutClaim = ???
  }
}
