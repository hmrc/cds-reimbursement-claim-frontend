/*
 * Copyright 2021 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{JourneyBindable, routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.MrnImporter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BasisOfClaim, MrnJourney}

trait JourneyTypeRoutes extends Product with Serializable {
  val subKey: Option[String]
  val journeyBindable: JourneyBindable

  def nextPageForBasisForClaim(basisOfClaim: BasisOfClaim): Call =
    basisOfClaim match {
      case BasisOfClaim.DuplicateEntry =>
        claimRoutes.EnterDuplicateMovementReferenceNumberController.enterDuplicateMrn(journeyBindable)
      case _                           =>
        claimRoutes.EnterCommoditiesDetailsController.enterCommoditiesDetails(journeyBindable)
    }

  def nextPageForCommoditiesDetails(isAmend: Boolean): Call =
    isAmend match {
      case true  => claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers()
      case false => claimRoutes.SelectDutiesController.selectDuties()
    }

  def submitUrlForCommoditiesDetails(isAmend: Boolean): Call =
    isAmend match {
      case true  => claimRoutes.EnterCommoditiesDetailsController.changeCommoditiesDetailsSubmit(journeyBindable)
      case false => claimRoutes.EnterCommoditiesDetailsController.enterCommoditiesDetailsSubmit(journeyBindable)
    }

  def nextPageForWhoIsMakingTheClaim(isAmend: Boolean): Call =
    isAmend match {
      case true  => claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers()
      case false => claimRoutes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds()
    }

  def submitUrlForWhoIsMakingTheClaim(isAmend: Boolean): Call =
    isAmend match {
      case true  => claimRoutes.SelectWhoIsMakingTheClaimController.changeDeclarantTypeSubmit(journeyBindable)
      case false => claimRoutes.SelectWhoIsMakingTheClaimController.selectDeclarantTypeSubmit(journeyBindable)
    }

  def nextPageForForClaimNorthernIreland(isAmend: Boolean, isAnswerChanged: Boolean): Call =
    if (!isAmend) {
      claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
    } else {
      if (isAnswerChanged) claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
      else claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers()
    }

  def submitUrlForClaimNorthernIreland(isAmend: Boolean): Call =
    if (isAmend)
      claimRoutes.ClaimNorthernIrelandController.changeNorthernIrelandClaimSubmit(journeyBindable)
    else claimRoutes.ClaimNorthernIrelandController.selectNorthernIrelandClaimSubmit(journeyBindable)
}
trait SingleRoutes extends JourneyTypeRoutes {
  override val subKey: Option[String] = None
  override val journeyBindable        = JourneyBindable.Single
}
trait BulkRoutes extends JourneyTypeRoutes {
  override val subKey: Option[String] = Some("bulk")
  override val journeyBindable        = JourneyBindable.Bulk
}
trait ScheduledRoutes extends JourneyTypeRoutes {
  override val subKey: Option[String] = Some("scheduled")
  override val journeyBindable        = JourneyBindable.Scheduled
}

trait ReferenceNumberTypeRoutes extends Product with Serializable {
  val refNumberKey: Option[String]
  def nextPageForEnterMRN(importer: MrnJourney): Call
  def nextPageForDuplicateMRN(importer: MrnJourney): Call
}
trait MRNRoutes extends ReferenceNumberTypeRoutes {
  val refNumberKey                                        = Some("mrn")
  def nextPageForEnterMRN(importer: MrnJourney): Call     = importer match {
    case _: MrnImporter => claimRoutes.CheckDeclarationDetailsController.checkDetails()
    case _              => claimRoutes.EnterImporterEoriNumberController.enterImporterEoriNumber()
  }
  def nextPageForDuplicateMRN(importer: MrnJourney): Call = importer match {
    case _: MrnImporter => claimRoutes.CheckDeclarationDetailsController.checkDuplicateDetails()
    case _              => claimRoutes.EnterImporterEoriNumberController.enterImporterEoriNumber()
  }
}
trait EntryNumberRoutes extends ReferenceNumberTypeRoutes {
  val refNumberKey                                        = Some("entry")
  def nextPageForEnterMRN(importer: MrnJourney): Call     =
    claimRoutes.EnterDeclarationDetailsController.enterDeclarationDetails()
  def nextPageForDuplicateMRN(importer: MrnJourney): Call =
    claimRoutes.EnterDeclarationDetailsController.enterDuplicateDeclarationDetails()
}
object ReimbursementRoutes {
  type ReimbursementRoutes = JourneyTypeRoutes with ReferenceNumberTypeRoutes
}

case object MRNSingleRoutes extends MRNRoutes with SingleRoutes
case object EntrySingleRoutes extends EntryNumberRoutes with SingleRoutes
case object MRNBulkRoutes extends MRNRoutes with BulkRoutes
case object EntryBulkRoutes extends EntryNumberRoutes with BulkRoutes
case object MRNScheduledRoutes extends MRNRoutes with ScheduledRoutes
case object EntryScheduledRoutes extends EntryNumberRoutes with ScheduledRoutes

case object JourneyNotDetectedRoutes extends JourneyTypeRoutes with ReferenceNumberTypeRoutes {
  val refNumberKey                    = None
  override val subKey: Option[String] = None
  override val journeyBindable        = JourneyBindable.Single

  val selectNumberOfClaimsPage: Call                      = claimRoutes.SelectNumberOfClaimsController.show()
  def nextPageForEnterMRN(importer: MrnJourney): Call     = controllers.routes.IneligibleController.ineligible()
  def nextPageForDuplicateMRN(importer: MrnJourney): Call = controllers.routes.IneligibleController.ineligible()
}
