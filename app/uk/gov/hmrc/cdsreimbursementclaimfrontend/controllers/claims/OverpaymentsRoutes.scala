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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable

import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle.{routes => overpaymentsSingleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.{routes => overpaymentsMultipleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled.{routes => overpaymentsScheduledRoutes}
import play.api.mvc.Call

object OverpaymentsRoutes {

  object ChooseFileTypeController {
    def show(journey: JourneyBindable): Call = journey match {
      case JourneyBindable.Single    => overpaymentsSingleRoutes.ChooseFileTypeController.show
      case JourneyBindable.Multiple  => overpaymentsMultipleRoutes.ChooseFileTypeController.show
      case JourneyBindable.Scheduled => overpaymentsScheduledRoutes.ChooseFileTypeController.show
    }
  }

  object UploadFilesController {
    def summary(journey: JourneyBindable): Call = journey match {
      case JourneyBindable.Single    => overpaymentsSingleRoutes.UploadFilesController.summary
      case JourneyBindable.Multiple  => overpaymentsMultipleRoutes.UploadFilesController.summary
      case JourneyBindable.Scheduled => overpaymentsScheduledRoutes.UploadFilesController.summary
    }
  }

  object CheckYourAnswersAndSubmitController {
    def checkAllAnswers(journey: JourneyBindable): Call = journey match {
      case JourneyBindable.Single    => overpaymentsSingleRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers
      case JourneyBindable.Multiple  => overpaymentsMultipleRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers
      case JourneyBindable.Scheduled => overpaymentsScheduledRoutes.CheckYourAnswersAndSubmitController.checkAllAnswers
    }

    def confirmationOfSubmission(journey: JourneyBindable): Call = journey match {
      case JourneyBindable.Single    =>
        overpaymentsSingleRoutes.CheckYourAnswersAndSubmitController.confirmationOfSubmission
      case JourneyBindable.Multiple  =>
        overpaymentsMultipleRoutes.CheckYourAnswersAndSubmitController.confirmationOfSubmission
      case JourneyBindable.Scheduled =>
        overpaymentsScheduledRoutes.CheckYourAnswersAndSubmitController.confirmationOfSubmission
    }

    def submissionError(journey: JourneyBindable): Call = journey match {
      case JourneyBindable.Single    =>
        overpaymentsSingleRoutes.CheckYourAnswersAndSubmitController.submissionError
      case JourneyBindable.Multiple  =>
        overpaymentsMultipleRoutes.CheckYourAnswersAndSubmitController.submissionError
      case JourneyBindable.Scheduled =>
        overpaymentsScheduledRoutes.CheckYourAnswersAndSubmitController.submissionError
    }
  }

  object EnterMovementReferenceNumberController {
    def enterJourneyMrn(journey: JourneyBindable): Call = journey match {
      case JourneyBindable.Single    => overpaymentsSingleRoutes.EnterMovementReferenceNumberController.enterJourneyMrn
      case JourneyBindable.Multiple  => overpaymentsMultipleRoutes.EnterMovementReferenceNumberController.enterJourneyMrn
      case JourneyBindable.Scheduled =>
        overpaymentsScheduledRoutes.EnterMovementReferenceNumberController.enterJourneyMrn
    }

    def enterMrnSubmit(journey: JourneyBindable): Call = journey match {
      case JourneyBindable.Single    => overpaymentsSingleRoutes.EnterMovementReferenceNumberController.enterMrnSubmit
      case JourneyBindable.Multiple  => overpaymentsMultipleRoutes.EnterMovementReferenceNumberController.enterMrnSubmit
      case JourneyBindable.Scheduled =>
        overpaymentsScheduledRoutes.EnterMovementReferenceNumberController.enterMrnSubmit
    }
  }

  object SelectBankAccountTypeController {
    def show(journey: JourneyBindable): Call = journey match {
      case JourneyBindable.Single    =>
        overpaymentsSingleRoutes.SelectBankAccountTypeController.show()
      case JourneyBindable.Multiple  =>
        overpaymentsMultipleRoutes.SelectBankAccountTypeController.show()
      case JourneyBindable.Scheduled =>
        overpaymentsScheduledRoutes.SelectBankAccountTypeController.show()
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  object EnterDuplicateMovementReferenceNumberController {
    def enterDuplicateMrnSubmit(journey: JourneyBindable): Call = journey match {
      case JourneyBindable.Single    =>
        overpaymentsSingleRoutes.EnterDuplicateMovementReferenceNumberController.enterDuplicateMrnSubmit
      case JourneyBindable.Multiple  =>
        throw new Exception("Route enterDuplicateMrnSubmit not defined for multiple journey")
      case JourneyBindable.Scheduled =>
        throw new Exception("Route enterDuplicateMrnSubmit not defined for scheduled journey")
    }

    def enterDuplicateMrn(journey: JourneyBindable): Call = journey match {
      case JourneyBindable.Single    =>
        overpaymentsSingleRoutes.EnterDuplicateMovementReferenceNumberController.enterDuplicateMrn
      case JourneyBindable.Multiple  => throw new Exception("Route enterDuplicateMrn not defined for multiple journey")
      case JourneyBindable.Scheduled => throw new Exception("Route enterDuplicateMrn not defined for scheduled journey")
    }
  }

}
