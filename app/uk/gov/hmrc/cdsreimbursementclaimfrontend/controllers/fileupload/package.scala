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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadReference

package object fileupload {

  implicit class FileUploadOps[A](val maybeAnswer: Option[A]) extends AnyVal {

    def hasReachedUploadThreshold(implicit fileUpload: FileUploadHelper[A]): Boolean =
      fileUpload.hasReachedUploadThreshold(maybeAnswer)

    def containsReference(uploadReference: UploadReference)(implicit fileUpload: FileUploadHelper[A]): Boolean =
      fileUpload.hasReference(maybeAnswer, uploadReference)
  }
}
