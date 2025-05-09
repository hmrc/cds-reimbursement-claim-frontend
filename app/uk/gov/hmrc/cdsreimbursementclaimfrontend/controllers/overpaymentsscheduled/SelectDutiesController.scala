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

import com.github.arturopala.validator.Validator.Validate
import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutyCodesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectExciseCategoriesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.OverpaymentsScheduledJourney.Checks.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_duty_codes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_excise_categories
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_excise_duty_codes

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutyTypesForm

@Singleton
class SelectDutiesController @Inject() (
  val jcc: JourneyControllerComponents,
  selectDutyCodesPage: select_duty_codes,
  selectExciseCategoriesPage: select_excise_categories,
  selectExciseDutyCodesPage: select_excise_duty_codes
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends OverpaymentsScheduledJourneyBaseController {

  // Allow actions only if the MRN and ACC14 declaration are in place, and the EORI has been verified.
  final override val actionPrecondition: Option[Validate[OverpaymentsScheduledJourney]] =
    Some(hasMRNAndDisplayDeclaration & declarantOrImporterEoriMatchesUserOrHasBeenVerified)

  def show(dutyType: DutyType): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    ExciseCategory.all.foreach { ec =>
      ec.taxCodes.foreach { tc =>
        println(s"""case object NI$tc extends TaxCode("$tc", DutyType.Excise, Some(ExciseCategory.$ec))""")
      }
    }

    if journey.isDutyTypeSelected then {
      if dutyType == DutyType.Excise then {
        val postAction: Call                                   = routes.SelectDutiesController.submitExciseCategories
        val maybeExciseCategories: Option[Seq[ExciseCategory]] = journey.getSelectedExciseCategories
        val form: Form[Seq[ExciseCategory]]                    = selectExciseCategoriesForm.withDefault(maybeExciseCategories)
        Ok(selectExciseCategoriesPage(form, postAction)).asFuture
      } else {
        val postAction: Call                     = routes.SelectDutiesController.submit(dutyType)
        val maybeTaxCodes: Option[List[TaxCode]] = Option(journey.getSelectedDuties(dutyType).toList)
        val form: Form[List[TaxCode]]            = selectDutyCodesForm.withDefault(maybeTaxCodes)
        Ok(selectDutyCodesPage(dutyType, form, postAction, journey.isSubsidyOnlyJourney)).asFuture
      }
    } else {
      Redirect(routes.SelectDutyTypesController.show).asFuture
    }
  }

  def submit(currentDuty: DutyType): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    val postAction: Call = routes.SelectDutiesController.submit(currentDuty)
    if journey.isDutyTypeSelected then {
      Future.successful(
        selectDutyCodesForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              (
                journey,
                BadRequest(selectDutyCodesPage(currentDuty, formWithErrors, postAction, journey.isSubsidyOnlyJourney))
              ),
            selectedTaxCodes =>
              journey
                .selectAndReplaceTaxCodeSetForReimbursement(currentDuty, selectedTaxCodes)
                .fold(
                  errors => {
                    logger.error(s"Error updating tax codes selection - $errors")
                    (
                      journey,
                      BadRequest(
                        selectDutyCodesPage(currentDuty, selectDutyCodesForm, postAction, journey.isSubsidyOnlyJourney)
                      )
                    )
                  },
                  updatedJourney =>
                    (
                      updatedJourney,
                      selectedTaxCodes.headOption.fold(
                        BadRequest(
                          selectDutyCodesPage(
                            currentDuty,
                            selectDutyCodesForm,
                            postAction,
                            journey.isSubsidyOnlyJourney
                          )
                        )
                      )(taxCode => Redirect(routes.EnterClaimController.show(currentDuty, taxCode)))
                    )
                )
          )
      )
    } else {
      (journey, Redirect(routes.SelectDutyTypesController.show)).asFuture
    }

  }

  def submitExciseCategories: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    val postAction: Call = routes.SelectDutiesController.submitExciseCategories
    if journey.isDutyTypeSelected then {
      Future.successful(
        selectExciseCategoriesForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              (
                journey,
                BadRequest(selectExciseCategoriesPage(formWithErrors, postAction))
              ),
            selectedExciseCategories =>
              journey
                .selectAndReplaceExciseCodeCategories(selectedExciseCategories)
                .fold(
                  errors => {
                    logger.error(s"Error updating excise categories selection - $errors")
                    (
                      journey,
                      BadRequest(
                        selectExciseCategoriesPage(selectExciseCategoriesForm, postAction)
                      )
                    )
                  },
                  updatedJourney =>
                    (
                      updatedJourney,
                      selectedExciseCategories.headOption.fold(
                        BadRequest(
                          selectExciseCategoriesPage(selectExciseCategoriesForm, postAction)
                        )
                      )(exciseCategory => Redirect(routes.SelectDutiesController.showExciseDuties(exciseCategory)))
                    )
                )
          )
      )
    } else {
      (journey, Redirect(routes.SelectDutyTypesController.show)).asFuture
    }
  }

  def showExciseDuties(exciseCategory: ExciseCategory): Action[AnyContent] = actionReadJourney {
    implicit request => journey =>
      if journey.isDutyTypeSelected then {
        val postAction: Call                     = routes.SelectDutiesController.submitExciseDuties(exciseCategory)
        val maybeTaxCodes: Option[List[TaxCode]] = Option(
          journey
            .getSelectedDuties(DutyType.Excise)
            .filter(tc => ExciseCategory.categoryOf(tc) == exciseCategory)
            .toList
        )
        val form: Form[List[TaxCode]]            = selectDutyCodesForm.withDefault(maybeTaxCodes)
        Ok(selectExciseDutyCodesPage(exciseCategory, form, postAction)).asFuture
      } else {
        Redirect(routes.SelectDutyTypesController.show).asFuture
      }
  }

  def submitExciseDuties(exciseCategory: ExciseCategory): Action[AnyContent] = actionReadWriteJourney {
    implicit request => journey =>
      val postAction: Call = routes.SelectDutiesController.submitExciseDuties(exciseCategory)
      if journey.isDutyTypeSelected then {
        Future.successful(
          selectDutyCodesForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                (
                  journey,
                  BadRequest(selectExciseDutyCodesPage(exciseCategory, formWithErrors, postAction))
                ),
              selectedTaxCodes =>
                journey
                  .selectAndReplaceTaxCodeSetForReimbursement(exciseCategory, selectedTaxCodes)
                  .fold(
                    errors => {
                      logger.error(s"Error updating tax codes selection - $errors")
                      (
                        journey,
                        BadRequest(
                          selectExciseDutyCodesPage(exciseCategory, selectDutyCodesForm, postAction)
                        )
                      )
                    },
                    updatedJourney =>
                      (
                        updatedJourney,
                        selectedTaxCodes.headOption.fold(
                          BadRequest(
                            selectExciseDutyCodesPage(exciseCategory, selectDutyCodesForm, postAction)
                          )
                        )(taxCode => Redirect(routes.EnterClaimController.show(DutyType.Excise, taxCode)))
                      )
                  )
            )
        )
      } else {
        (journey, Redirect(routes.SelectDutyTypesController.show)).asFuture
      }

  }

}
