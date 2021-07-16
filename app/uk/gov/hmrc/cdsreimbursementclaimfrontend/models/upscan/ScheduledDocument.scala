package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan

import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UpscanCallBack.UpscanSuccess

import java.time.LocalDateTime

final case class ScheduledDocument(
  fileName: String,
  uploadReference: UploadReference,
  uploadedOn: LocalDateTime,
  upscanUploadMeta: UpscanUploadMeta,
  upscanSuccess: UpscanSuccess
) extends UploadDocument

object ScheduledDocument {
  implicit val format: OFormat[ScheduledDocument] = Json.format
}
