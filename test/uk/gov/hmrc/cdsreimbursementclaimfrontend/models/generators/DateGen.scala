package uk.gov.hmrc.cdsreimbursementclaimfrontend.models.generators

import org.scalacheck.{Arbitrary, Gen}
import java.time.LocalDate

object DateGen {

  implicit lazy val arbitraryDate = Arbitrary(date)

  lazy val genDate = arbitraryDate.arbitrary

  def date: Gen[LocalDate] = {
    val rangeStart  = LocalDate.now.minusMonths(6).toEpochDay
    val currentYear = LocalDate.now.getYear
    val rangeEnd    = LocalDate.of(currentYear, 12, 31).toEpochDay
    Gen.choose(rangeStart, rangeEnd).map(t => LocalDate.ofEpochDay(t))
  }

}
