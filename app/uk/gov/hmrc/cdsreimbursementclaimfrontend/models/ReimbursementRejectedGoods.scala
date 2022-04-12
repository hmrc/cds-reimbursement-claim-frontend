package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import cats.implicits.catsSyntaxEq
import cats.kernel.Semigroup
import play.api.libs.json.Json
import play.api.libs.json.OFormat

final case class ReimbursementRejectedGoods(paidAmount: BigDecimal, claimAmount: BigDecimal) {
  lazy val refundTotal: BigDecimal = claimAmount

  lazy val isUnclaimed: Boolean = paidAmount === 0 && claimAmount === 0

  lazy val isValid: Boolean = claimAmount >= 0 && claimAmount < paidAmount
}

object ReimbursementRejectedGoods {

  val unclaimed: ReimbursementRejectedGoods = ReimbursementRejectedGoods(paidAmount = 0, claimAmount = 0)

  implicit val reimbursementSemigroup: Semigroup[ReimbursementRejectedGoods] = (x: ReimbursementRejectedGoods, y: ReimbursementRejectedGoods) =>
    ReimbursementRejectedGoods(
      paidAmount = x.paidAmount + y.paidAmount,
      claimAmount = x.claimAmount + y.claimAmount
    )

  implicit val format: OFormat[ReimbursementRejectedGoods] = Json.format[ReimbursementRejectedGoods]
}