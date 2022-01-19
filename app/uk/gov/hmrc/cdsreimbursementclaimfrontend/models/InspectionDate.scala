package uk.gov.hmrc.cdsreimbursementclaimfrontend.models

import play.api.libs.functional.syntax._
import play.api.libs.json.Format

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Try

final case class InspectionDate(value: LocalDate) extends AnyVal

object InspectionDate {

  def displayFormat(date: String): Option[String] = {
    val result = for {
      t <- Try(LocalDate.parse(date, DateTimeFormatter.ofPattern("u-M-d")))
      f <- Try(DateTimeFormatter.ofPattern("d MMMM u").format(t))
    } yield f
    result.toOption
  }
  implicit class InspectionDateOps(private val inspectionDate: InspectionDate) {
    def checkYourDetailsDisplayFormat: String = displayFormat(inspectionDate.value.toString).getOrElse("")
  }

  implicit val format: Format[InspectionDate] =
    implicitly[Format[LocalDate]].inmap(InspectionDate(_), _.value)

}
