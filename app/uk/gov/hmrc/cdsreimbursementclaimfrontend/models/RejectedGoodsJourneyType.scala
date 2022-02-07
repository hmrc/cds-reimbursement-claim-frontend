package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.EnumerationFormat

sealed trait RejectedGoodsJourneyType

  object RejectedGoodsJourneyType extends EnumerationFormat[RejectedGoodsJourneyType] {
  
   case object Single extends RejectedGoodsJourneyType
    case object Multiple extends RejectedGoodsJourneyType
    case object Scheduled extends RejectedGoodsJourneyType

  override val values: Set[RejectedGoodsJourneyType] = Set(Single, Multiple, Scheduled)

    private[models] val rejectedGoodsJourneyTypeStringMap: Map[String, RejectedGoodsJourneyType] =
      values.map(a => a.toString -> a).toMap

    def has(journeyType: String): Boolean                                            =
      rejectedGoodsJourneyTypeStringMap.contains(journeyType)

    def find(journeyType: String): Option[RejectedGoodsJourneyType] =
      rejectedGoodsJourneyTypeStringMap.get(journeyType)

    def findUnsafe(journeyType: String): RejectedGoodsJourneyType =
      rejectedGoodsJourneyTypeStringMap(journeyType)
}
