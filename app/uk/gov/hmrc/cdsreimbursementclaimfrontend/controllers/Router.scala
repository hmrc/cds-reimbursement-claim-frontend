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

import cats.syntax.eq._
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController.{CheckDeclarationDetailsAnswer, DeclarationAnswersAreCorrect, DeclarationAnswersAreIncorrect}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{JourneyBindable, routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.{routes => uploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.MrnImporter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BasisOfClaim, MrnJourney}

trait SubmitRoutes extends Product with Serializable {
  val journeyBindable: JourneyBindable

  def submitUrlForCheckDeclarationDetails(): Call =
    claimRoutes.CheckDeclarationDetailsController.submit(journeyBindable)

  def submitUrlForCheckDuplicateDeclarationDetails(): Call =
    claimRoutes.CheckDuplicateDeclarationDetailsController.submit(journeyBindable)

  def submitUrlForBasisOfClaim(isAmend: Boolean): Call =
    if (isAmend)
      claimRoutes.SelectBasisForClaimController.changeBasisForClaimSubmit(journeyBindable)
    else claimRoutes.SelectBasisForClaimController.selectBasisForClaimSubmit(journeyBindable)

  def submitUrlForCommoditiesDetails(isAmend: Boolean): Call =
    isAmend match {
      case true  => claimRoutes.EnterCommoditiesDetailsController.changeCommoditiesDetailsSubmit(journeyBindable)
      case false => claimRoutes.EnterCommoditiesDetailsController.enterCommoditiesDetailsSubmit(journeyBindable)
    }

  def submitUrlForWhoIsMakingTheClaim(isAmend: Boolean): Call =
    isAmend match {
      case true  => claimRoutes.SelectWhoIsMakingTheClaimController.changeDeclarantTypeSubmit(journeyBindable)
      case false => claimRoutes.SelectWhoIsMakingTheClaimController.selectDeclarantTypeSubmit(journeyBindable)
    }

  def submitUrlForClaimNorthernIreland(isAmend: Boolean): Call =
    if (isAmend)
      claimRoutes.ClaimNorthernIrelandController.changeNorthernIrelandClaimSubmit(journeyBindable)
    else claimRoutes.ClaimNorthernIrelandController.selectNorthernIrelandClaimSubmit(journeyBindable)

}

trait JourneyTypeRoutes extends Product with Serializable {
  val subKey: Option[String]
  val journeyBindable: JourneyBindable

  def nextPageForCheckDeclarationDetails(checkDeclarationDetailsAnswer: CheckDeclarationDetailsAnswer): Call =
    checkDeclarationDetailsAnswer match {
      case DeclarationAnswersAreCorrect   =>
        if (journeyBindable === JourneyBindable.Scheduled)
          uploadRoutes.ScheduleOfMrnDocumentController.uploadScheduledDocument()
        else claimRoutes.SelectWhoIsMakingTheClaimController.selectDeclarantType(journeyBindable)
      case DeclarationAnswersAreIncorrect =>
        claimRoutes.EnterMovementReferenceNumberController.enterJourneyMrn(journeyBindable)
    }

  def nextPageForCheckDuplicateDeclarationDetails(): Call =
    claimRoutes.EnterCommoditiesDetailsController.enterCommoditiesDetails(journeyBindable)

  def nextPageForBasisForClaim(basisOfClaim: BasisOfClaim): Call =
    basisOfClaim match {
      case BasisOfClaim.DuplicateEntry =>
        claimRoutes.EnterDuplicateMovementReferenceNumberController.enterDuplicateMrn(journeyBindable)
      case _                           =>
        claimRoutes.EnterCommoditiesDetailsController.enterCommoditiesDetails(journeyBindable)
    }

  def nextPageForBasisForClaim(basisOfClaim: BasisOfClaim, isAmend: Boolean): Call =
    if (isAmend) {
      claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers()
    } else
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

  def nextPageForWhoIsMakingTheClaim(isAmend: Boolean): Call =
    isAmend match {
      case true  => claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers()
      case false => claimRoutes.CheckClaimantDetailsController.show(journeyBindable)
    }

  def nextPageForForClaimNorthernIreland(isAmend: Boolean, isAnswerChanged: Boolean): Call =
    if (!isAmend) {
      claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
    } else {
      if (isAnswerChanged) claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
      else claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers()
    }

  def nextPageForClaimantDetails(): Call =
    claimRoutes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds()
  //TODO put this back after removing the EnterDetailsRegisteredWithCdsController
  //    featureSwitch.NorthernIreland.isEnabled() match {
  //      case true  => claimRoutes.ClaimNorthernIrelandController.selectNorthernIrelandClaim(journeyBindable)
  //      case false => claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
  //    }

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
  val journeyBindable: JourneyBindable
  def nextPageForEnterMRN(importer: MrnJourney): Call     = importer match {
    case _: MrnImporter => claimRoutes.CheckDeclarationDetailsController.show(journeyBindable)
    case _              => claimRoutes.EnterImporterEoriNumberController.enterImporterEoriNumber()
  }
  def nextPageForDuplicateMRN(importer: MrnJourney): Call = importer match {
    case _: MrnImporter => claimRoutes.CheckDuplicateDeclarationDetailsController.show(journeyBindable)
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
  type ReimbursementRoutes = JourneyTypeRoutes with ReferenceNumberTypeRoutes with SubmitRoutes
}

case object MRNSingleRoutes extends MRNRoutes with SingleRoutes with SubmitRoutes
case object EntrySingleRoutes extends EntryNumberRoutes with SingleRoutes with SubmitRoutes
case object MRNBulkRoutes extends MRNRoutes with BulkRoutes with SubmitRoutes
case object EntryBulkRoutes extends EntryNumberRoutes with BulkRoutes with SubmitRoutes
case object MRNScheduledRoutes extends MRNRoutes with ScheduledRoutes with SubmitRoutes
case object EntryScheduledRoutes extends EntryNumberRoutes with ScheduledRoutes with SubmitRoutes

case object JourneyNotDetectedRoutes extends JourneyTypeRoutes with ReferenceNumberTypeRoutes with SubmitRoutes {
  val refNumberKey    = None
  val subKey          = None
  val journeyBindable = JourneyBindable.Single

  val selectNumberOfClaimsPage: Call                      = claimRoutes.SelectNumberOfClaimsController.show()
  def nextPageForEnterMRN(importer: MrnJourney): Call     = controllers.routes.IneligibleController.ineligible()
  def nextPageForDuplicateMRN(importer: MrnJourney): Call = controllers.routes.IneligibleController.ineligible()
}
