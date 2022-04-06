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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsscheduled

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.inspectionAddressTypeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.InspectionAddressType._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoods => pages}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import scala.concurrent.ExecutionContext

@Singleton
class ChooseInspectionAddressTypeController @Inject() (
  val jcc: JourneyControllerComponents,
  inspectionAddressPage: pages.choose_inspection_address_type
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, val errorHandler: ErrorHandler)
    extends RejectedGoodsScheduledJourneyBaseController {

  private val postAction = routes.ChooseInspectionAddressTypeController.submit()

  def show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    (journey.getPotentialInspectionAddresses match {
      case List() => Redirect("routes.ChooseInspectionAddressTypeController.redirectToALF()")
      case addresses =>
        Ok(
          inspectionAddressPage(
            addresses,
            inspectionAddressTypeForm,
            postAction
          )
        )
    }).asFuture
  }

  def submit: Action[AnyContent] = actionReadWriteJourney(
    { implicit request => journey =>
        inspectionAddressTypeForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              (
                journey,
                BadRequest(
                  inspectionAddressPage(
                    journey.getPotentialInspectionAddresses,
                    formWithErrors,
                    postAction
                  )
                )
              ),
            {
              case Other =>
                (
                  journey,
                  Redirect("routes.ChooseInspectionAddressTypeController.redirectToALF()")
                )
              case inspectionAddressType =>
                journey.getInspectionAddressForType(inspectionAddressType).map { address =>
                  (
                    journey.submitInspectionAddress(address),
                    Redirect("check-bank-details")
                  )
                }.getOrElse(
                  (
                    journey,
                    Redirect(baseRoutes.IneligibleController.ineligible())
                  )
                )
            }
          ).asFuture
    }
  )
}
