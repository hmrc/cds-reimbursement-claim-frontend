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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckContactDetailsMrnController.{CheckClaimantDetailsAnswer, NoClaimantDetailsAnswer, YesClaimantDetailsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckDeclarationDetailsController.{CheckDeclarationDetailsAnswer, DeclarationAnswersAreCorrect, DeclarationAnswersAreIncorrect}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.JourneyBindable.Scheduled
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{JourneyBindable, routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.reimbursement.{routes => reimbursementRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.{routes => uploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.MrnImporter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.{EntryNumber, MRN}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{BasisOfClaim, DeclarantTypeAnswer, MrnJourney}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

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

  def submitPageForClaimantDetails(isChange: Boolean): Call = {
    val controller = claimRoutes.CheckContactDetailsMrnController
    if (isChange) controller.submit(journeyBindable)
    else controller.addDetails(journeyBindable)
  }

  def submitUrlForChangeMrnContactDetails(): Call =
    claimRoutes.EnterContactDetailsMrnController.changeMrnContactDetailsSubmit(journeyBindable)

  def submitUrlForEnterMrnContactDetails(): Call =
    claimRoutes.EnterContactDetailsMrnController.enterMrnContactDetailsSubmit(journeyBindable)

  def submitUrlForEnterOrChangeBankAccountDetails(isChange: Boolean): Call =
    if (isChange) claimRoutes.BankAccountController.changeBankAccountDetailsSubmit(journeyBindable)
    else claimRoutes.BankAccountController.enterBankAccountDetailsSubmit(journeyBindable)

  def submitUrlForSelectBankAccountType(): Call =
    claimRoutes.SelectBankAccountTypeController.selectBankAccountTypeSubmit(journeyBindable)

  def submitDetailsRegisteredWithCds(isAmend: Boolean): Call =
    if (isAmend) claimRoutes.EnterDetailsRegisteredWithCdsController.changeDetailsRegisteredWithCdsSubmit()
    else claimRoutes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCdsSubmit()

}

trait JourneyTypeRoutes extends Product with Serializable {
  val subKey: Option[String]
  val journeyBindable: JourneyBindable

  def mergeWithSubKey(rootKey: String): String =
    subKey.toList.foldLeft(rootKey)((root, sub) => s"$root.$sub")

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
      basisOfClaim match {
        case BasisOfClaim.DuplicateEntry =>
          claimRoutes.EnterDuplicateMovementReferenceNumberController.enterDuplicateMrn(journeyBindable)
        case _                           =>
          claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(journeyBindable)
      }
    } else
      basisOfClaim match {
        case BasisOfClaim.DuplicateEntry =>
          claimRoutes.EnterDuplicateMovementReferenceNumberController.enterDuplicateMrn(journeyBindable)
        case _                           =>
          claimRoutes.EnterCommoditiesDetailsController.enterCommoditiesDetails(journeyBindable)
      }

  def nextPageForCommoditiesDetails(isAmend: Boolean): Call =
    isAmend match {
      case true  => claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(journeyBindable)
      case false =>
        if (journeyBindable === Scheduled) {
          reimbursementRoutes.SelectDutyTypesController.showDutyTypes()
        } else {
          claimRoutes.SelectDutiesController.selectDuties()
        }
    }

  def nextPageForWhoIsMakingTheClaim(mrnOrEntryNumber: Option[Either[EntryNumber, MRN]], isAmend: Boolean): Call =
    mrnOrEntryNumber match {
      case Some(Right(_)) =>
        if (isAmend)
          claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(journeyBindable)
        else claimRoutes.CheckContactDetailsMrnController.show(journeyBindable)
      case _              =>
        claimRoutes.EnterDetailsRegisteredWithCdsController.enterDetailsRegisteredWithCds()
    }

  def nextPageForForClaimNorthernIreland(isAmend: Boolean, isAnswerChanged: Boolean): Call =
    if (!isAmend) {
      claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
    } else {
      if (isAnswerChanged) claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
      else claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(journeyBindable)
    }

  def nextPageForAddClaimantDetails(answer: CheckClaimantDetailsAnswer, featureSwitch: FeatureSwitchService): Call =
    answer match {
      case YesClaimantDetailsAnswer =>
        claimRoutes.EnterContactDetailsMrnController.enterMrnContactDetails(journeyBindable)
      case NoClaimantDetailsAnswer  =>
        featureSwitch.NorthernIreland.isEnabled() match {
          case true  => claimRoutes.ClaimNorthernIrelandController.selectNorthernIrelandClaim(journeyBindable)
          case false => claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
        }
    }

  def nextPageForChangeClaimantDetails(
    answer: CheckClaimantDetailsAnswer,
    featureSwitch: FeatureSwitchService
  ): Call =
    answer match {
      case YesClaimantDetailsAnswer =>
        featureSwitch.NorthernIreland.isEnabled() match {
          case true  => claimRoutes.ClaimNorthernIrelandController.selectNorthernIrelandClaim(journeyBindable)
          case false => claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
        }
      case NoClaimantDetailsAnswer  =>
        claimRoutes.CheckContactDetailsMrnController.show(journeyBindable)
    }

  def nextPageForMrnContactDetails(isChange: Boolean): Call =
    if (isChange) claimRoutes.CheckContactDetailsMrnController.show(journeyBindable)
    else claimRoutes.CheckContactDetailsMrnController.changeAddress(journeyBindable)

  def nextPageForCheckBankAccountDetails(): Call =
    claimRoutes.SelectBankAccountTypeController.selectBankAccountType(journeyBindable)

  def nextPageForEnterBankAccountDetails(
    hasBankAccountType: Boolean
  ): Call =
    hasBankAccountType match {
      case false => claimRoutes.SelectBankAccountTypeController.selectBankAccountType(journeyBindable)
      case true  => claimRoutes.BankAccountController.checkBankAccountDetails(journeyBindable)
    }

  def nextPageForSelectBankAccountType(): Call =
    claimRoutes.BankAccountController.enterBankAccountDetails(journeyBindable)

  def nextPageForDetailsRegisteredWithCDS(
    declarantType: Option[DeclarantTypeAnswer]
  ): Call = nextPageForEntryNumberContactDetails(declarantType)

  def nextPageForEntryNumberContactDetails(declarantType: Option[DeclarantTypeAnswer]): Call =
    declarantType match {
      case Some(declarantType) =>
        declarantType match {
          case _ =>
            claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
        }
      case None                =>
        claimRoutes.SelectWhoIsMakingTheClaimController.selectDeclarantType(journeyBindable)
    }
}

trait SingleRoutes extends JourneyTypeRoutes {
  override val subKey: Option[String] = None
  override val journeyBindable        = JourneyBindable.Single
}

trait MultipleRoutes extends JourneyTypeRoutes {
  override val subKey: Option[String] = Some("bulk")
  override val journeyBindable        = JourneyBindable.Multiple
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
case object MRNMultipleRoutes extends MRNRoutes with MultipleRoutes with SubmitRoutes
case object EntryMultipleRoutes extends EntryNumberRoutes with MultipleRoutes with SubmitRoutes
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
