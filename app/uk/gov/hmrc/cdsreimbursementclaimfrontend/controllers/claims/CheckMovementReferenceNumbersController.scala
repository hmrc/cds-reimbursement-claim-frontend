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

import cats.Eq
import cats.implicits.catsSyntaxEq
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{boolean, mapping, optional}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.CheckMovementReferenceNumbersController.{DoNotAddAnotherMrn, YesAddAnotherMrn, addAnotherMrnAnswerForm}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

@Singleton
class CheckMovementReferenceNumbersController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  checkMovementReferenceNumbersPage: pages.check_movement_reference_numbers
)(implicit viewConfig: ViewConfig, cc: MessagesControllerComponents)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with Logging {

  def showReferences(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.using({ case journey: FillingOutClaim =>
      Ok(checkMovementReferenceNumbersPage(journey.draftClaim.MRNs(), addAnotherMrnAnswerForm))
    })
  }

  def submitReferences(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    addAnotherMrnAnswerForm
      .bindFromRequest()
      .fold(
        formWithErrors => BadRequest(checkMovementReferenceNumbersPage(formWithErrors)),
        {
          case YesAddAnotherMrn   =>
            request.using({ case journey: FillingOutClaim =>
              Redirect(routes.EnterAssociatedMRNController.enterMrn(journey.draftClaim.MRNs.total + 1))
            })
          case DoNotAddAnotherMrn =>
            Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType(JourneyBindable.Multiple))
        }
      )
  }
}

object CheckMovementReferenceNumbersController {

  val checkMovementReferenceNumbersKey = "check-movement-reference-numbers"

  sealed trait AddAnotherMrnAnswer extends Product with Serializable
  case object YesAddAnotherMrn extends AddAnotherMrnAnswer
  case object DoNotAddAnotherMrn extends AddAnotherMrnAnswer

  implicit val addAnotherDocumentMrnEq: Eq[AddAnotherMrnAnswer] =
    Eq.fromUniversalEquals[AddAnotherMrnAnswer]

  val addAnotherMrnAnswerForm: Form[AddAnotherMrnAnswer] =
    Form(
      mapping(
        checkMovementReferenceNumbersKey -> optional(boolean)
          .verifying("invalid-answer", _.isDefined)
          .transform[AddAnotherMrnAnswer](
            opt => if (opt.exists(_ === true)) YesAddAnotherMrn else DoNotAddAnotherMrn,
            answer => Some(answer === YesAddAnotherMrn)
          )
      )(identity)(Some(_))
    )
}
