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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.{routes => uploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.MrnImporter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.{No, Yes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{DeclarantTypeAnswer, YesNo}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService

trait SubmitRoutes extends Product with Serializable {
  val journeyBindable: JourneyBindable

  def submitUrlForCheckDeclarationDetails(): Call =
    claimRoutes.CheckDeclarationDetailsController.submit(journeyBindable)

  def submitUrlForCheckDuplicateDeclarationDetails(): Call =
    claimRoutes.CheckDuplicateDeclarationDetailsController.submit(journeyBindable)

  def submitUrlForEnterImporterEoriNumber(): Call =
    claimRoutes.EnterImporterEoriNumberController.enterImporterEoriNumberSubmit(journeyBindable)

  def submitUrlForEnterDeclarantEoriNumber(): Call =
    claimRoutes.EnterDeclarantEoriNumberController.enterDeclarantEoriNumberSubmit(journeyBindable)

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

  def submitPageForClaimantDetails(isChange: Boolean): Call = {
    val controller = claimRoutes.CheckContactDetailsMrnController
    if (isChange) controller.submit(journeyBindable)
    else controller.addDetailsSubmit(journeyBindable)
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

  object CheckAnswers {
    def when(flag: Boolean)(alternatively: Call): Call =
      if (flag) claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(journeyBindable) else alternatively
  }

  def nextPageForCheckDeclarationDetails(
    whetherDeclarationDetailsCorrect: YesNo,
    hasAssociatedMrns: Boolean
  ): Call =
    whetherDeclarationDetailsCorrect match {
      case Yes =>
        journeyBindable match {
          case JourneyBindable.Scheduled =>
            uploadRoutes.ScheduleOfMrnDocumentController.uploadScheduledDocument()
          case JourneyBindable.Multiple  =>
            if (hasAssociatedMrns)
              claimRoutes.CheckMovementReferenceNumbersController.showMrns()
            else
              claimRoutes.EnterAssociatedMrnController.enterMrn(AssociatedMrnIndex.fromListIndex(0))
          case _                         =>
            claimRoutes.SelectWhoIsMakingTheClaimController.selectDeclarantType(journeyBindable)
        }
      case No  =>
        claimRoutes.EnterMovementReferenceNumberController.enterJourneyMrn(journeyBindable)
    }

  def nextPageForCheckDuplicateDeclarationDetails(): Call =
    claimRoutes.EnterCommoditiesDetailsController.enterCommoditiesDetails(journeyBindable)

  def nextPageForCommoditiesDetails(isAmend: Boolean): Call =
    isAmend match {
      case true  => claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(journeyBindable)
      case false =>
        if (journeyBindable === JourneyBindable.Scheduled) {
          claimRoutes.SelectDutyTypesController.showDutyTypes()
        } else if (journeyBindable === JourneyBindable.Multiple) {
          claimRoutes.SelectMultipleDutiesController.selectDuties(1)
        } else {
          claimRoutes.SelectDutiesController.selectDuties()
        }
    }

  def nextPageForWhoIsMakingTheClaim(
    isAmend: Boolean,
    mandatoryDataAvailable: Boolean
  ): Call =
    if (isAmend)
      claimRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(journeyBindable)
    else if (mandatoryDataAvailable) claimRoutes.CheckContactDetailsMrnController.show(journeyBindable)
    else claimRoutes.CheckContactDetailsMrnController.addDetailsShow(journeyBindable)

  def nextPageForAddClaimantDetails(answer: YesNo, featureSwitch: FeatureSwitchService): Call =
    answer match {
      case Yes =>
        claimRoutes.EnterContactDetailsMrnController.enterMrnContactDetails(journeyBindable)
      case No  =>
        if (featureSwitch.NorthernIreland.isEnabled())
          claimRoutes.ClaimNorthernIrelandController.selectNorthernIrelandClaim(journeyBindable)
        else claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
    }

  def nextPageForChangeClaimantDetails(
    answer: YesNo,
    featureSwitch: FeatureSwitchService
  ): Call =
    answer match {
      case Yes =>
        if (featureSwitch.NorthernIreland.isEnabled())
          claimRoutes.ClaimNorthernIrelandController.selectNorthernIrelandClaim(journeyBindable)
        else claimRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)

      case No =>
        claimRoutes.CheckContactDetailsMrnController.addDetailsShow(journeyBindable)
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
  override val subKey: Option[String] = Some("multiple")
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
    case _              => claimRoutes.EnterImporterEoriNumberController.enterImporterEoriNumber(journeyBindable)
  }
  def nextPageForDuplicateMRN(importer: MrnJourney): Call = importer match {
    case _: MrnImporter => claimRoutes.CheckDuplicateDeclarationDetailsController.show(journeyBindable)
    case _              => claimRoutes.EnterImporterEoriNumberController.enterImporterEoriNumber(journeyBindable)
  }
}

object ReimbursementRoutes {

  type ReimbursementRoutes = JourneyTypeRoutes with ReferenceNumberTypeRoutes with SubmitRoutes

  def apply(journey: JourneyBindable): ReimbursementRoutes =
    journey match {
      case JourneyBindable.Single    => MRNSingleRoutes
      case JourneyBindable.Multiple  => MRNMultipleRoutes
      case JourneyBindable.Scheduled => MRNScheduledRoutes
    }
}

case object MRNSingleRoutes extends MRNRoutes with SingleRoutes with SubmitRoutes
case object MRNMultipleRoutes extends MRNRoutes with MultipleRoutes with SubmitRoutes
case object MRNScheduledRoutes extends MRNRoutes with ScheduledRoutes with SubmitRoutes

case object JourneyNotDetectedRoutes extends JourneyTypeRoutes with ReferenceNumberTypeRoutes with SubmitRoutes {
  val refNumberKey    = None
  val subKey          = None
  val journeyBindable = JourneyBindable.Single

  val selectNumberOfClaimsPage: Call                      = claimRoutes.SelectTypeOfClaimController.show()
  def nextPageForEnterMRN(importer: MrnJourney): Call     = controllers.routes.IneligibleController.ineligible()
  def nextPageForDuplicateMRN(importer: MrnJourney): Call = controllers.routes.IneligibleController.ineligible()
}
