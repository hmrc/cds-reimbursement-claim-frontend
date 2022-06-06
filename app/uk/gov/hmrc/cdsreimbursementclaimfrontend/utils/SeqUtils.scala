package uk.gov.hmrc.cdsreimbursementclaimfrontend.utils

object SeqUtils {
  implicit class SeqOps[A](val seq: Seq[A]) {
    def containsEachItemOf(other: Seq[A]): Boolean =
      seq.forall(other.contains(_))
  }
}
