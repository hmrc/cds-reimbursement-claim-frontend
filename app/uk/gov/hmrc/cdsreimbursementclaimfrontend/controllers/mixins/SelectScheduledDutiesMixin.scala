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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ClaimBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.claims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutyType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ExciseCategory
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_duty_codes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_excise_categories
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_excise_duty_codes

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

trait SelectScheduledDutiesMixin extends ClaimBaseController {

  type Claim <: claims.Claim & claims.ClaimBase & claims.ScheduledVariantProperties

  val selectDutyCodesPage: select_duty_codes
  val selectExciseCategoriesPage: select_excise_categories
  val selectExciseDutyCodesPage: select_excise_duty_codes

  val routesPack: SelectScheduledDutiesMixin.RoutesPack

  val selectAndReplaceExciseCodeCategories: Claim => Seq[ExciseCategory] => Either[String, Claim]
  val selectAndReplaceTaxCodeSetForDutyType: Claim => (DutyType, Seq[TaxCode]) => Either[String, Claim]
  val selectAndReplaceTaxCodeSetForExciseCategory: Claim => (ExciseCategory, Seq[TaxCode]) => Either[String, Claim]

  final def show(dutyType: DutyType): Action[AnyContent] = actionReadClaim { claim =>
    if claim.isDutyTypeSelected then {
      if dutyType == DutyType.Excise then {
        val postAction: Call                                   = routesPack.submitExciseCategories
        val maybeExciseCategories: Option[Seq[ExciseCategory]] = claim.getSelectedExciseCategories
        val form: Form[Seq[ExciseCategory]]                    = selectExciseCategoriesForm.withDefault(maybeExciseCategories)
        Ok(selectExciseCategoriesPage(form, postAction))
      } else {
        val postAction: Call                     = routesPack.submitDutyType(dutyType)
        val maybeTaxCodes: Option[List[TaxCode]] = Option(claim.getSelectedDuties(dutyType).toList)
        val form: Form[List[TaxCode]]            = selectDutyCodesForm.withDefault(maybeTaxCodes)
        Ok(selectDutyCodesPage(dutyType, form, postAction))
      }
    } else {
      Redirect(routesPack.showSelectDutyTypes)
    }
  }

  final def submit(currentDuty: DutyType): Action[AnyContent] = actionReadWriteClaim { claim =>
    val postAction: Call = routesPack.submitDutyType(currentDuty)
    if claim.isDutyTypeSelected then {

      selectDutyCodesForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              claim,
              BadRequest(selectDutyCodesPage(currentDuty, formWithErrors, postAction))
            ),
          selectedTaxCodes =>
            selectAndReplaceTaxCodeSetForDutyType(claim)(currentDuty, selectedTaxCodes)
              .fold(
                errors => {
                  logger.error(s"Error updating tax codes selection - $errors")
                  (
                    claim,
                    BadRequest(
                      selectDutyCodesPage(currentDuty, selectDutyCodesForm, postAction)
                    )
                  )
                },
                updatedClaim =>
                  (
                    updatedClaim,
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
    } else {
      (claim, Redirect(routesPack.showSelectDutyTypes))
    }

  }

  final def submitExciseCategories: Action[AnyContent] = actionReadWriteClaim { claim =>
    val postAction: Call = routesPack.submitExciseCategories
    if claim.isDutyTypeSelected then {

      selectExciseCategoriesForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            (
              claim,
              BadRequest(selectExciseCategoriesPage(formWithErrors, postAction))
            ),
          selectedExciseCategories =>
            selectAndReplaceExciseCodeCategories(claim)(selectedExciseCategories)
              .fold(
                errors => {
                  logger.error(s"Error updating excise categories selection - $errors")
                  (
                    claim,
                    BadRequest(
                      selectExciseCategoriesPage(selectExciseCategoriesForm, postAction)
                    )
                  )
                },
                updatedClaim =>
                  (
                    updatedClaim,
                    selectedExciseCategories.headOption.fold(
                      BadRequest(
                        selectExciseCategoriesPage(selectExciseCategoriesForm, postAction)
                      )
                    )(exciseCategory => Redirect(routesPack.showExciseDuties(exciseCategory)))
                  )
              )
        )
    } else {
      (claim, Redirect(routesPack.showSelectDutyTypes))
    }
  }

  final def showExciseDuties(exciseCategory: ExciseCategory): Action[AnyContent] =
    actionReadClaim { claim =>
      if claim.isDutyTypeSelected then {
        val postAction: Call                     = routesPack.submitExciseDuties(exciseCategory)
        val maybeTaxCodes: Option[List[TaxCode]] = Option(
          claim
            .getSelectedDuties(DutyType.Excise)
            .filter(tc => ExciseCategory.categoryOf(tc) == exciseCategory)
            .toList
        )
        val form: Form[List[TaxCode]]            = selectDutyCodesForm.withDefault(maybeTaxCodes)
        Ok(selectExciseDutyCodesPage(exciseCategory, form, postAction))
      } else {
        Redirect(routesPack.showSelectDutyTypes)
      }
    }

  final def submitExciseDuties(exciseCategory: ExciseCategory): Action[AnyContent] =
    actionReadWriteClaim { claim =>
      val postAction: Call = routesPack.submitExciseDuties(exciseCategory)
      if claim.isDutyTypeSelected then {

        selectDutyCodesForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              (
                claim,
                BadRequest(selectExciseDutyCodesPage(exciseCategory, formWithErrors, postAction))
              ),
            selectedTaxCodes =>
              selectAndReplaceTaxCodeSetForExciseCategory(claim)(exciseCategory, selectedTaxCodes)
                .fold(
                  errors => {
                    logger.error(s"Error updating tax codes selection - $errors")
                    (
                      claim,
                      BadRequest(
                        selectExciseDutyCodesPage(exciseCategory, selectDutyCodesForm, postAction)
                      )
                    )
                  },
                  updatedClaim =>
                    (
                      updatedClaim,
                      selectedTaxCodes.headOption.fold(
                        BadRequest(
                          selectExciseDutyCodesPage(exciseCategory, selectDutyCodesForm, postAction)
                        )
                      )(taxCode => Redirect(routesPack.showEnterClaim(DutyType.Excise, taxCode)))
                    )
                )
          )
      } else {
        (claim, Redirect(routesPack.showSelectDutyTypes))
      }

    }

}
