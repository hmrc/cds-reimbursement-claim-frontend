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

import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterAssociatedMRNController.associatedMovementReferenceNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.AssociatedMrn
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

@Singleton
class EnterAssociatedMRNController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  enterAssociatedMrnPage: pages.enter_associated_mrn
)(implicit viewConfig: ViewConfig, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging {

  def enterMrn(index: Int): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok(enterAssociatedMrnPage(index, associatedMovementReferenceNumberForm))
  }

  def submitMrn(): Action[AnyContent] = authenticatedActionWithSessionData {
    Redirect(routes.CheckMovementReferenceNumbersController.showReferences())
  }
}

object EnterAssociatedMRNController {

  val enterAssociatedMrnKey = "enter-associated-mrn"

  val associatedMovementReferenceNumberForm: Form[AssociatedMrn] =
    Form(
      mapping(
        enterAssociatedMrnKey -> nonEmptyText
          .verifying("invalid.number", str => str.isEmpty || MRN.isValid(str))
          .transform[AssociatedMrn](MRN(_), _.value)
      )(identity)(Some(_))
    )
}
