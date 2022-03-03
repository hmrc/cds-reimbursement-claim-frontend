package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration

trait AddAcc14 {
  def addAcc14(
    journey: RejectedGoodsMultipleJourney,
    acc14Declaration: DisplayDeclaration
  ): Either[String, RejectedGoodsMultipleJourney] = {
    val nextIndex           = journey.getMovementReferenceNumbers.map(_.size).getOrElse(0)
    val adjustedDeclaration = journey.getDeclarantEoriFromACC14
      .fold(acc14Declaration) { eori =>
        val declarant = acc14Declaration.getDeclarantDetails.copy(declarantEORI = eori.value)
        val drd       = acc14Declaration.displayResponseDetail.copy(declarantDetails = declarant)
        acc14Declaration.copy(displayResponseDetail = drd)
      }
    journey
      .submitMovementReferenceNumberAndDeclaration(nextIndex, adjustedDeclaration.getMRN, adjustedDeclaration)
  }
}
