/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentssingle_v2

import com.github.arturopala.validator.Validator.Validate
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterImporterEoriNumberMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsSingleJourney.Checks._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_importer_eori_number

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class EnterImporterEoriNumberOfDuplicateDeclaration @Inject() (
  val jcc: JourneyControllerComponents,
  val enterImporterEoriNumber: enter_importer_eori_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsSingleJourneyBaseController
    with EnterImporterEoriNumberMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsSingleJourney]] =
    Some(
      hasMRNAndDisplayDeclaration &
        declarantOrImporterEoriMatchesUserOrHasBeenVerified &
        needsDuplicateMrnAndDeclaration &
        hasDuplicateMRNAndDisplayDeclaration
    )

  final override val postAction: Call =
    routes.EnterImporterEoriNumberOfDuplicateDeclaration.submit

  final override val continueAction: Call =
    routes.EnterDeclarantEoriNumberOfDuplicateDeclarationController.show

  final override val whenEoriInputNotRequiredAction: Call =
    routes.EnterAdditionalDetailsController.show

  final override def needsEoriSubmission(journey: Journey): Boolean =
    journey.needsDeclarantAndConsigneeEoriCheckForDuplicateDeclaration

  final override def getEoriNumberAnswer(journey: Journey): Option[Eori] =
    None

  final override def modifyJourney(journey: Journey, eori: Eori): Either[String, Journey] =
    journey.checkConsigneeEoriNumberWithDuplicateDeclaration(eori)

}
