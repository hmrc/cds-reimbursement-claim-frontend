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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common.{routes => commonRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.{routes => overpaymentsMultipleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.{routes => overpaymentsScheduledRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.{routes => overpaymentsSingleRoutes}
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
    OverpaymentsRoutes.BankAccountController.enterBankAccountDetailsSubmit(journeyBindable)

  def submitUrlForEnterMovementReferenceNumber(): Call =
    OverpaymentsRoutes.EnterMovementReferenceNumberController.enterMrnSubmit(journeyBindable)

  def submitUrlForCheckDeclarationDetails(): Call =
    OverpaymentsRoutes.CheckDeclarationDetailsController.submit(journeyBindable)

  def submitUrlForCheckDuplicateDeclarationDetails(): Call =
    overpaymentsSingleRoutes.CheckDuplicateDeclarationDetailsController.submit

  def submitUrlForChangeContactDetails(): Call =
    claimRoutes.EnterContactDetailsMrnController.changeMrnContactDetailsSubmit(journeyBindable)

  def submitUrlForEnterContactDetails(): Call =
    claimRoutes.EnterContactDetailsMrnController.enterMrnContactDetailsSubmit(journeyBindable)

  def submitUrlForSelectBankAccountType(): Call =
    OverpaymentsRoutes.SelectBankAccountTypeController.show(journeyBindable)

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
            OverpaymentsRoutes.CheckContactDetailsController.show(journeyBindable)
        }
      case No  =>
        OverpaymentsRoutes.EnterMovementReferenceNumberController.enterJourneyMrn(journeyBindable)
    }

  def nextPageForCheckDuplicateDeclarationDetails(): Call =
    OverpaymentsRoutes.EnterAdditionalDetailsController.show(journeyBindable)

  def nextPageForContactDetails(isChange: Boolean): Call =
    if (isChange) OverpaymentsRoutes.CheckContactDetailsController.show(journeyBindable)
    else OverpaymentsRoutes.CheckContactDetailsController.redirectToALF(journeyBindable)

  def nextPageForSelectBankAccountType(): Call =
    OverpaymentsRoutes.BankAccountController.enterBankAccountDetails(journeyBindable)

  def nextPageForDetailsRegisteredWithCDS(
    declarantType: Option[DeclarantTypeAnswer]
  ): Call = nextPageForEntryNumberContactDetails(declarantType)

  def nextPageForEntryNumberContactDetails(declarantType: Option[DeclarantTypeAnswer]): Call =
    declarantType match {
      case Some(declarantType) =>
        declarantType match {
          case _ =>
            OverpaymentsRoutes.SelectBasisForClaimController.selectBasisForClaim(journeyBindable)
        }
      case None                =>
        OverpaymentsRoutes.CheckContactDetailsController.show(journeyBindable)
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
    case _: MrnImporter => OverpaymentsRoutes.CheckDeclarationDetailsController.show(journeyBindable)
    case _              => OverpaymentsRoutes.EnterImporterEoriNumberController.enterImporterEoriNumber(journeyBindable)
  }
  def nextPageForDuplicateMRN(importer: MrnJourney): Call = importer match {
    case _: MrnImporter => overpaymentsSingleRoutes.CheckDuplicateDeclarationDetailsController.show
    case _              => OverpaymentsRoutes.EnterImporterEoriNumberController.enterImporterEoriNumber(journeyBindable)
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
