package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.Declaration

sealed trait MrnJourney extends Product with Serializable

object MrnJourney {
  final case class MrnImporter(declaration: Declaration) extends MrnJourney
  final case class ThirdPartyImporter(declaration: Declaration) extends MrnJourney
}
