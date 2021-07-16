package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanSuccess

import java.time.LocalDateTime

trait UploadDocument {
  val fileName: String
  val uploadReference: UploadReference
  val uploadedOn: LocalDateTime
  val upscanUploadMeta: UpscanUploadMeta
  val upscanSuccess: UpscanSuccess
}
