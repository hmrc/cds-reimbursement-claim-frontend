package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

package object upload {

  implicit class UploadOps[A](val maybeAnswer: Option[A]) extends AnyVal {
    def hasReachedUploadThreshold(implicit fileUpload: FileUpload[A]): Boolean =
      fileUpload.hasReachedUploadThreshold(maybeAnswer)
  }
}
