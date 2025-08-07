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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsscheduled

import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Validator.Validate
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins.SelectScheduledDutiesMixin
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_duty_codes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_excise_categories
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_excise_duty_codes

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class SelectDutiesController @Inject() (
  val jcc: JourneyControllerComponents,
  val selectDutyCodesPage: select_duty_codes,
  val selectExciseCategoriesPage: select_excise_categories,
  val selectExciseDutyCodesPage: select_excise_duty_codes
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsScheduledJourneyBaseController
    with SelectScheduledDutiesMixin {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsScheduledJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  final val routesPack = SelectScheduledDutiesMixin.RoutesPack(
    showSelectDutyTypes = routes.SelectDutyTypesController.show,
    showEnterClaim = routes.EnterClaimController.show,
    submitDutyType = routes.SelectDutiesController.submit,
    submitExciseCategories = routes.SelectDutiesController.submitExciseCategories,
    showExciseDuties = routes.SelectDutiesController.showExciseDuties,
    submitExciseDuties = routes.SelectDutiesController.submitExciseDuties
  )

  val selectAndReplaceExciseCodeCategories: Journey => Seq[ExciseCategory] => Either[String, Journey] =
    (journey: Journey) => journey.selectAndReplaceExciseCodeCategories

  val selectAndReplaceTaxCodeSetForDutyType: Journey => (DutyType, Seq[TaxCode]) => Either[String, Journey] =
    (journey: Journey) => journey.selectAndReplaceTaxCodeSetForDutyType

  val selectAndReplaceTaxCodeSetForExciseCategory
    : Journey => (ExciseCategory, Seq[TaxCode]) => Either[String, Journey] =
    (journey: Journey) => journey.selectAndReplaceTaxCodeSetForExciseCategory

}
