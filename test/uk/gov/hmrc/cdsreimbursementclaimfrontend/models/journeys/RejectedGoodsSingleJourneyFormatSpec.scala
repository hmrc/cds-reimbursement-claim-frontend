package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journeys

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DocumentTypeRejectedGoods
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journeys.RejectedGoodsSingleJourney._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.support.JsonFormatTest

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
      validateJsonFormat("{}", RejectedGoodsSingleJourney.empty)
      validateJsonFormat(
        """{"movementReferenceNumber":"19GB03I52858027001"}""",
        RejectedGoodsSingleJourney.empty.submitMovementReferenceNumber(MRN("19GB03I52858027001"))
      )
    }
  }

}
