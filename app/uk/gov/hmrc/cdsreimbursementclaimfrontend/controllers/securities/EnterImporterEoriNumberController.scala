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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import com.github.arturopala.validator.Validator.Validate
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.EnterImporterEoriNumberMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.SecuritiesJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.enter_importer_eori_number

import scala.concurrent.ExecutionContext

@Singleton
class EnterImporterEoriNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  val enterImporterEoriNumber: enter_importer_eori_number
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController
    with EnterImporterEoriNumberMixin {

  import SecuritiesJourney.Checks._

  // Allow actions only if the MRN, RfS and ACC14 declaration are in place, and TPI04 check has been made.
  override val actionPrecondition: Option[Validate[SecuritiesJourney]] =
    Some(hasMRNAndDisplayDeclarationAndRfS)

  final override val postAction: Call =
    routes.EnterImporterEoriNumberController.submit()

  final override val continueAction: Call =
    routes.EnterDeclarantEoriNumberController.show()

  final override val whenEoriInputNotRequiredAction: Call =
    routes.SelectSecuritiesController.showFirst()

  final override def modifyJourney(journey: Journey, eori: Eori): Either[String, Journey] =
    journey.submitConsigneeEoriNumber(eori)

}
