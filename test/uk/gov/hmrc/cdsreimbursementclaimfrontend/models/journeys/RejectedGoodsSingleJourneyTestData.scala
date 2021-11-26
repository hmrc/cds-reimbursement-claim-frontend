package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journeys

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._

import java.time.Instant
import java.time.LocalDateTime
import org.scalacheck.Gen

trait RejectedGoodsSingleJourneyTestData {

  //val completeJourneyGen: Gen[RejectedGoodsSingleJourney] = ???

  val uploadDocument = UploadDocument(
    uploadReference = UploadReference("upload-reference"),
    upscanUploadMeta = UpscanUploadMeta(
      reference = "reference",
      uploadRequest = UploadRequest(
        href = "upload-request-ref",
        fields = Map("field-a" -> "a", "field-b" -> "b")
      )
    ),
    uploadedOn = LocalDateTime.parse("2007-12-03T10:15:30"),
    upscanSuccess = UpscanCallBack.UpscanSuccess(
      reference = "upscan-reference",
      fileStatus = "upscan-file-status",
      downloadUrl = "upscan-download-url",
      uploadDetails = UpscanCallBack.UploadDetails(
        fileName = "",
        fileMimeType = "",
        uploadTimestamp = Instant.ofEpochMilli(0L),
        checksum = "",
        size = 1L
      )
    ),
    fileName = "file-name",
    documentType = None
  )

  val uploadDocumentJson =
    """{"uploadReference": "upload-reference",
    |    "upscanUploadMeta": {
    |        "reference": "reference",
    |        "uploadRequest": {
    |            "href": "upload-request-ref",
    |            "fields": {
    |                "field-a": "a",
    |                "field-b": "b"
    |            }
    |        }
    |    },
    |    "uploadedOn": "2007-12-03T10:15:30",
    |    "upscanSuccess": {
    |        "reference": "upscan-reference",
    |        "fileStatus": "upscan-file-status",
    |        "downloadUrl": "upscan-download-url",
    |        "uploadDetails": {
    |            "fileName": "",
    |            "fileMimeType": "",
    |            "uploadTimestamp": "1970-01-01T00:00:00Z",
    |            "checksum": "",
    |            "size": 1
    |        }
    |    },
    |    "fileName": "file-name"
            |}""".stripMargin
}
