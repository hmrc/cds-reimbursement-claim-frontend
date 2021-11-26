package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journeys
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.JsonFormatTest
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journeys.RejectedGoodsSingleJourney._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan.UploadDocument
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DocumentTypeRejectedGoods
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.upscan._
import java.time.LocalDateTime
import java.time.Instant

class RejectedGoodsSingleJourneyFormatSpec
    extends AnyWordSpec
    with JsonFormatTest
    with Matchers
    with RejectedGoodsSingleJourneyTestData {

  "RejectedGoodsSingleJourney.Answers" should {
    "serialize into json format and back" in {
      validateJsonFormat("{}", Answers())
      validateJsonFormat(
        """{"movementReferenceNumber":"19GB03I52858027001"}""",
        Answers(movementReferenceNumber = Some(MRN("19GB03I52858027001")))
      )
      validateJsonFormat(
        """{"reimbursementClaims":{"A00":12.99}}""",
        Answers(reimbursementClaims = Some(Map(TaxCode.A00 -> Some(BigDecimal("12.99")))))
      )
      validateJsonFormat(
        """{"reimbursementClaims":{"A00":12.99,"A40":null}}""",
        Answers(reimbursementClaims = Some(Map(TaxCode.A00 -> Some(BigDecimal("12.99")), TaxCode.A40 -> None)))
      )
      validateJsonFormat(
        """{"reimbursementClaims":{}}""",
        Answers(reimbursementClaims = Some(Map()))
      )

      validateJsonFormat(
        s"""{"supportingEvidences":{"entry__0":{"k":$uploadDocumentJson,"v":{"Foo":{}}}}}""",
        Answers(supportingEvidences = Some(Map(uploadDocument -> Some(DocumentTypeRejectedGoods.Foo))))
      )
      validateJsonFormat(
        """{"supportingEvidences":{}}""",
        Answers(supportingEvidences = Some(Map()))
      )
    }
  }

  "RejectedGoodsSingleJourney" should {
    "serialize into json format and back" in {
      validateJsonFormat("{}", RejectedGoodsSingleJourney())
    }
  }

}

trait RejectedGoodsSingleJourneyTestData {

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
