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

import cats.implicits.catsSyntaxEq
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

@Singleton
class WhoIsMakingTheClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  whoIsMakingTheClaimPage: pages.who_is_making_the_claim
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def chooseDeclarantType(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok(
      whoIsMakingTheClaimPage(
        WhoIsMakingTheClaimController.chooseDeclarantTypeForm,
        routes.CheckDeclarantDetailsController.checkDetails()
      )
    )
  }

  def chooseDeclarantTypeSubmit(): Action[AnyContent] = authenticatedActionWithSessionData {
    Redirect(routes.SelectReasonForClaimController.selectReasonForClaim())
  }

}

object WhoIsMakingTheClaimController {

  sealed trait DeclarantType extends Product with Serializable

  object DeclarantType {
    final case object Importer extends DeclarantType
    final case object AssociateWithImporterCompany extends DeclarantType
    final case object AssociatedWithRepresentativeCompany extends DeclarantType
  }

  val chooseDeclarantTypeForm: Form[DeclarantType] =
    Form(
      mapping(
        "choose-who-is-making-the-claim" -> number
          .verifying("invalid", a => a === 0 || a === 1 || a === 2)
          .transform[DeclarantType](
            value =>
              if (value === 0) DeclarantType.Importer
              else if (value === 1) DeclarantType.AssociateWithImporterCompany
              else DeclarantType.AssociatedWithRepresentativeCompany,
            {
              case DeclarantType.Importer                            => 0
              case DeclarantType.AssociateWithImporterCompany        => 1
              case DeclarantType.AssociatedWithRepresentativeCompany => 2
            }
          )
      )(identity)(Some(_))
    )
}
