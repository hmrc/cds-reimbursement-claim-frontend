package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

sealed  abstract class DutyCategory (val value : String) extends Product  with Serializable {
  override def toString: String = value
}
