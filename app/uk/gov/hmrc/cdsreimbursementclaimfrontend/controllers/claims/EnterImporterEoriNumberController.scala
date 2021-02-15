/*
 * Copyright 2021 HM Revenue & Customs
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

import cats.syntax.eq._

import javax.inject.{Inject, Singleton}
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.data._
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterImporterEoriNumberController.ImporterEoriNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Eori
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

@Singleton
class EnterImporterEoriNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  enterImporterEoriNumberPage: pages.enter_importer_eori_number
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def enterImporterEoriNumber(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok(
      enterImporterEoriNumberPage(
        EnterImporterEoriNumberController.eoriNumberForm.fill(ImporterEoriNumber(Eori("12345678912345617")))
      )
    )
  }

  def enterImporterEoriNumberSubmit(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Redirect(routes.EnterDeclarationDetailsController.enterDeclarationDetails())
  }

}

object EnterImporterEoriNumberController {

  final case class ImporterEoriNumber(value: Eori)

  val eoriNumberMapping: Mapping[Eori] =
    nonEmptyText
      .verifying("invalid.number", str => Eori.isValid(str))
      .transform[Eori](str => Eori(str), eori => eori.value)

  val eoriNumberForm: Form[ImporterEoriNumber] = Form(
    mapping(
      "enter-importer-eori-number" -> eoriNumberMapping
    )(ImporterEoriNumber.apply)(ImporterEoriNumber.unapply)
  )

  def processFormErrors(errors: Seq[FormError]): FormError =
    if (errors.exists(fe => fe.message === "error.required")) {
      FormError("enter-importer-eori-number", List("error.required"))
    } else if (errors.exists(fe => fe.message === "invalid.reference"))
      FormError("enter-importer-eori-number", List("invalid.reference"))
    else
      FormError("enter-importer-eori-number", List("invalid"))

}
