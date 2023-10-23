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
import play.api.mvc.Result
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.payeeTypeForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.PayeeType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.choose_payee_type_page

import scala.concurrent.Future

trait ChoosePayeeTypeMixin extends JourneyBaseController {
  def modifyJourney(journey: Journey, payeeType: PayeeType): Either[String, Journey]
  val choosePayeeTypePage: choose_payee_type_page
  val postAction: Call
  def nextPage(journey: Journey): Call

  final def submitPayeeType(payeeType: PayeeType)(implicit journey: Journey): Future[(Journey, Result)] =
    modifyJourney(journey, payeeType)
      .fold(
        e => {
          logger.warn(e)
          (journey, Redirect(baseRoutes.IneligibleController.ineligible()))
        },
        (_, Redirect(nextPage(journey)))
      )
      .asFuture

  final val show: Action[AnyContent] =
    actionReadWriteJourney { implicit request => implicit journey =>
      if (journey.declarantEoriMatchesConsignee) {
        submitPayeeType(PayeeType.Consignee)
      } else {
        val form: Form[PayeeType] =
          payeeTypeForm.withDefault(journey.answers.payeeType)
        (journey, Ok(choosePayeeTypePage(form, postAction))).asFuture
      }
    }

  final val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => implicit journey =>
      payeeTypeForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Future.successful(
              (
                journey,
                BadRequest(
                  choosePayeeTypePage(formWithErrors, postAction)
                )
              )
            ),
          submitPayeeType
        )
    }
}
