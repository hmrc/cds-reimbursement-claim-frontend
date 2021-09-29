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

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterAssociatedMrnController.associatedMovementReferenceNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.IntegerOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.{AssociatedMRNsAnswer, AssociatedMrn}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class EnterAssociatedMrnController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  sessionStore: SessionCache,
  enterAssociatedMrnPage: pages.enter_associated_mrn
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[AssociatedMRNsAnswer] = _.associatedMRNsAnswer

  def enterMRN(index: Int): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok(enterAssociatedMrnPage(index, associatedMovementReferenceNumberForm))
  }

  def submitMRN(index: Int): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[AssociatedMRNsAnswer] { (fillingOutClaim, associatedMRNs) =>
        associatedMovementReferenceNumberForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(enterAssociatedMrnPage(index, formWithErrors)),
            mrn => {
              val mrns      = associatedMRNs.map(_ :+ mrn) orElse AssociatedMRNsAnswer(mrn).some
              val claimCopy = FillingOutClaim.of(fillingOutClaim)(_.copy(associatedMRNsAnswer = mrns))

              EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = claimCopy.some)))
                .fold(
                  logAndDisplayError(s"Error persisting ${index.ordinalNaming} MRN: "),
                  _ => Redirect(routes.CheckMovementReferenceNumbersController.showMRNs())
                )
            }
          )
      }
    }
}

object EnterAssociatedMrnController {

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
