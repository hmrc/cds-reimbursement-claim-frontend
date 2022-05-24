/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.OverpaymentsRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.{routes => overpaymentsSingleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.{routes => overpaymentsMultipleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.{routes => overpaymentsScheduledRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.{routes => commonRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.MrnImporter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DeclarantTypeAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.No
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.YesNo.Yes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex

trait SubmitRoutes extends Product with Serializable {
  val journeyBindable: JourneyBindable

  def submitUrlForEnterBankAccountDetails(): Call =
    claimRoutes.BankAccountController.enterBankAccountDetailsSubmit(journeyBindable)

  def submitUrlForEnterMovementReferenceNumber(): Call =
    claims.OverpaymentsRoutes.EnterMovementReferenceNumberController.enterMrnSubmit(journeyBindable)

  def submitUrlForCheckDeclarationDetails(): Call =
    claimRoutes.CheckDeclarationDetailsController.submit(journeyBindable)

  def submitUrlForCheckDuplicateDeclarationDetails(): Call =
    claimRoutes.CheckDuplicateDeclarationDetailsController.submit(journeyBindable)

  def submitUrlForEnterImporterEoriNumber(): Call =
    claimRoutes.EnterImporterEoriNumberController.enterImporterEoriNumberSubmit(journeyBindable)

  def submitUrlForEnterDeclarantEoriNumber(): Call =
    claimRoutes.EnterDeclarantEoriNumberController.enterDeclarantEoriNumberSubmit(journeyBindable)

  def submitUrlForChangeMrnContactDetails(): Call =
    claimRoutes.EnterContactDetailsMrnController.changeMrnContactDetailsSubmit(journeyBindable)

  def submitUrlForEnterMrnContactDetails(): Call =
    claimRoutes.EnterContactDetailsMrnController.enterMrnContactDetailsSubmit(journeyBindable)

  def submitUrlForSelectBankAccountType(): Call =
    claimRoutes.SelectBankAccountTypeController.selectBankAccountTypeSubmit(journeyBindable)

  def submitUrlForSelectDutyTypes(): Call =
    claims.OverpaymentsRoutes.SelectDutyTypesController.submitDutyTypes(journeyBindable)
}

trait JourneyTypeRoutes extends Product with Serializable {
  val subKey: Option[String]
  val journeyBindable: JourneyBindable

  object CheckAnswers {
    def when(flag: Boolean)(alternatively: => Call): Call =
      if (flag)
        controllers.claims.OverpaymentsRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers(journeyBindable)
      else alternatively
  }

  def nextPageForCheckDeclarationDetails(
    whetherDeclarationDetailsCorrect: YesNo,
    hasAssociatedMrns: Boolean
  ): Call =
    whetherDeclarationDetailsCorrect match {
      case Yes =>
        journeyBindable match {
          case JourneyBindable.Scheduled =>
            overpaymentsScheduledRoutes.UploadMrnListController.show
          case JourneyBindable.Multiple  =>
            if (hasAssociatedMrns)
              overpaymentsMultipleRoutes.CheckMovementReferenceNumbersController.showMrns
            else
              overpaymentsMultipleRoutes.EnterAssociatedMrnController.enterMrn(AssociatedMrnIndex.fromListIndex(0))
          case _                         =>
            claimRoutes.CheckContactDetailsMrnController.show(journeyBindable)
        }
      case No  =>
        claims.OverpaymentsRoutes.EnterMovementReferenceNumberController.enterJourneyMrn(journeyBindable)
    }

  def nextPageForCheckDuplicateDeclarationDetails(): Call =
    claims.OverpaymentsRoutes.EnterAdditionalDetailsController.show(journeyBindable)

  def nextPageForMrnContactDetails(isChange: Boolean): Call =
    if (isChange) claimRoutes.CheckContactDetailsMrnController.show(journeyBindable)
    else claimRoutes.CheckContactDetailsMrnController.redirectToALF(journeyBindable)

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
        claimRoutes.CheckContactDetailsMrnController.show(journeyBindable)
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

  val selectNumberOfClaimsPage: Call                      = commonRoutes.SelectTypeOfClaimController.show()
  def nextPageForEnterMRN(importer: MrnJourney): Call     = controllers.routes.IneligibleController.ineligible()
  def nextPageForDuplicateMRN(importer: MrnJourney): Call = controllers.routes.IneligibleController.ineligible()
}
