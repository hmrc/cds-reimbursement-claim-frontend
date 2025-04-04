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
import play.api.mvc.*
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.YesOrNoQuestionForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.routes as baseRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.CommonJourneyProperties
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.JourneyBase
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.YesNo.*
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration

import scala.concurrent.Future

trait CheckDeclarationDetailsMixin extends JourneyBaseController {

  type Journey <: journeys.Journey & JourneyBase & CommonJourneyProperties

  def getDisplayDeclaration(journey: Journey): Option[DisplayDeclaration]
  def continueRoute(journey: Journey): Call
  val enterMovementReferenceNumberRoute: Call

  implicit val errorHandler: ErrorHandler

  def viewTemplate: (DisplayDeclaration, Form[YesNo], Journey) => Request[?] => HtmlFormat.Appendable

  val checkDeclarationDetailsAnswerForm: Form[YesNo] =
    YesOrNoQuestionForm("check-declaration-details")

  final val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Future.successful(
      getDisplayDeclaration(journey).fold(Redirect(baseRoutes.IneligibleController.ineligible))(declaration =>
        Ok(
          viewTemplate(
            declaration,
            checkDeclarationDetailsAnswerForm,
            journey
          )(request)
        )
      )
    )
  }

  final val submit: Action[AnyContent] = simpleActionReadWriteJourney { implicit request => journey =>
    checkDeclarationDetailsAnswerForm
      .bindFromRequest()
      .fold(
        formWithErrors =>
          (
            journey,
            getDisplayDeclaration(journey)
              .map(declaration =>
                BadRequest(
                  viewTemplate(
                    declaration,
                    formWithErrors,
                    journey
                  )(request)
                )
              )
              .getOrElse(errorHandler.errorResult())
          ),
        answer =>
          (
            journey,
            Redirect(answer match {
              case Yes => continueRoute(journey)
              case No  => enterMovementReferenceNumberRoute
            })
          )
      )
  }

}
