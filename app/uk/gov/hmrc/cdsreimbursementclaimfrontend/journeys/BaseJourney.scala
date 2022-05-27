package uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys

import com.github.arturopala.validator.Validator._

abstract class BaseJourney[A : Validate] {
  self: A =>

  def caseNumber: Option[String]

  val validate: Validate[A] = implicitly[Validate[A]]

  /** Check if the journey is ready to finalize, i.e. to get the output. */
  def hasCompleteAnswers: Boolean =
    validate(this).isValid

  def isFinalized: Boolean = caseNumber.isDefined

  def whileJourneyIsAmendable(body: => A): A =
    if (isFinalized) this else body

  def whileJourneyIsAmendable(
    body: => Either[String, A]
  ): Either[String, A] =
    if (isFinalized) Left(JourneyValidationErrors.JOURNEY_ALREADY_FINALIZED) else body

}
