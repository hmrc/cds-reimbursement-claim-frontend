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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.mixins

import play.api.data.Form
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutyCodesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectExciseCategoriesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.Journey
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_duty_codes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_excise_categories
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_excise_duty_codes

import scala.concurrent.Future

object SelectScheduledDutiesMixin {
  final case class RoutesPack(
    showSelectDutyTypes: Call,
    showEnterClaim: (DutyType, TaxCode) => Call,
    submitDutyType: DutyType => Call,
    submitExciseCategories: Call,
    showExciseDuties: ExciseCategory => Call,
    submitExciseDuties: ExciseCategory => Call
  )
}

trait SelectScheduledDutiesMixin extends JourneyBaseController {

  type Journey <: journeys.Journey & journeys.JourneyBase & journeys.ScheduledVariantProperties

  val selectDutyCodesPage: select_duty_codes
  val selectExciseCategoriesPage: select_excise_categories
  val selectExciseDutyCodesPage: select_excise_duty_codes

  val routesPack: SelectScheduledDutiesMixin.RoutesPack

  val selectAndReplaceExciseCodeCategories: Journey => Seq[ExciseCategory] => Either[String, Journey]
  val selectAndReplaceTaxCodeSetForDutyType: Journey => (DutyType, Seq[TaxCode]) => Either[String, Journey]
  val selectAndReplaceTaxCodeSetForExciseCategory: Journey => (ExciseCategory, Seq[TaxCode]) => Either[String, Journey]

  final def show(dutyType: DutyType): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    if journey.isDutyTypeSelected then {
      if dutyType == DutyType.Excise then {
        val postAction: Call                                   = routesPack.submitExciseCategories
        val maybeExciseCategories: Option[Seq[ExciseCategory]] = journey.getSelectedExciseCategories
        val form: Form[Seq[ExciseCategory]]                    = selectExciseCategoriesForm.withDefault(maybeExciseCategories)
        Ok(selectExciseCategoriesPage(form, postAction)).asFuture
      } else {
        val postAction: Call                     = routesPack.submitDutyType(dutyType)
        val maybeTaxCodes: Option[List[TaxCode]] = Option(journey.getSelectedDuties(dutyType).toList)
        val form: Form[List[TaxCode]]            = selectDutyCodesForm.withDefault(maybeTaxCodes)
        Ok(selectDutyCodesPage(dutyType, form, postAction)).asFuture
      }
    } else {
      Redirect(routesPack.showSelectDutyTypes).asFuture
    }
  }

  final def submit(currentDuty: DutyType): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    val postAction: Call = routesPack.submitDutyType(currentDuty)
    if journey.isDutyTypeSelected then {
      Future.successful(
        selectDutyCodesForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              (
                journey,
                BadRequest(selectDutyCodesPage(currentDuty, formWithErrors, postAction))
              ),
            selectedTaxCodes =>
              selectAndReplaceTaxCodeSetForDutyType(journey)(currentDuty, selectedTaxCodes)
                .fold(
                  errors => {
                    logger.error(s"Error updating tax codes selection - $errors")
                    (
                      journey,
                      BadRequest(
                        selectDutyCodesPage(currentDuty, selectDutyCodesForm, postAction)
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
                            postAction
                          )
                        )
                      )(taxCode => Redirect(routesPack.showEnterClaim(currentDuty, taxCode)))
                    )
                )
          )
      )
    } else {
      (journey, Redirect(routesPack.showSelectDutyTypes)).asFuture
    }

  }

  final def submitExciseCategories: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    val postAction: Call = routesPack.submitExciseCategories
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
              selectAndReplaceExciseCodeCategories(journey)(selectedExciseCategories)
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
                      )(exciseCategory => Redirect(routesPack.showExciseDuties(exciseCategory)))
                    )
                )
          )
      )
    } else {
      (journey, Redirect(routesPack.showSelectDutyTypes)).asFuture
    }
  }

  final def showExciseDuties(exciseCategory: ExciseCategory): Action[AnyContent] = actionReadJourney {
    implicit request => journey =>
      if journey.isDutyTypeSelected then {
        val postAction: Call                     = routesPack.submitExciseDuties(exciseCategory)
        val maybeTaxCodes: Option[List[TaxCode]] = Option(
          journey
            .getSelectedDuties(DutyType.Excise)
            .filter(tc => ExciseCategory.categoryOf(tc) == exciseCategory)
            .toList
        )
        val form: Form[List[TaxCode]]            = selectDutyCodesForm.withDefault(maybeTaxCodes)
        Ok(selectExciseDutyCodesPage(exciseCategory, form, postAction)).asFuture
      } else {
        Redirect(routesPack.showSelectDutyTypes).asFuture
      }
  }

  final def submitExciseDuties(exciseCategory: ExciseCategory): Action[AnyContent] = actionReadWriteJourney {
    implicit request => journey =>
      val postAction: Call = routesPack.submitExciseDuties(exciseCategory)
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
                selectAndReplaceTaxCodeSetForExciseCategory(journey)(exciseCategory, selectedTaxCodes)
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
                        )(taxCode => Redirect(routesPack.showEnterClaim(DutyType.Excise, taxCode)))
                      )
                  )
            )
        )
      } else {
        (journey, Redirect(routesPack.showSelectDutyTypes)).asFuture
      }

  }

}
