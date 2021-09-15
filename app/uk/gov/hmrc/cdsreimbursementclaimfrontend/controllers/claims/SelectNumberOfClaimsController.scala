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
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Error, SelectNumberOfClaimsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_number_of_claims
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class SelectNumberOfClaimsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  selectNumberOfClaimsPage: select_number_of_claims
)(implicit
  viewConfig: ViewConfig,
  ec: ExecutionContext,
  errorHandler: ErrorHandler,
  cc: MessagesControllerComponents
) extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftC285Claim => Option[SelectNumberOfClaimsAnswer] = _.selectNumberOfClaimsAnswer

  def show(): Action[AnyContent] = (featureSwitch.BulkClaim.hideIfNotEnabled andThen
    authenticatedActionWithSessionData).async { implicit request =>
    withAnswers[SelectNumberOfClaimsAnswer] { (_, answers) =>
      val emptyForm  = SelectNumberOfClaimsController.selectNumberOfClaimsAnswerForm
      val filledForm = answers.fold(emptyForm)(emptyForm.fill)
      Ok(selectNumberOfClaimsPage(filledForm))
    }
  }

  def submit(): Action[AnyContent] =
    (featureSwitch.BulkClaim.hideIfNotEnabled andThen authenticatedActionWithSessionData).async { implicit request =>
      withAnswers[SelectNumberOfClaimsAnswer] { (fillingOutClaim, _) =>
        SelectNumberOfClaimsController.selectNumberOfClaimsAnswerForm
          .bindFromRequest()
          .fold(
            formWithErros => BadRequest(selectNumberOfClaimsPage(formWithErros)),
            updatedAnswers => {
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(selectNumberOfClaimsAnswer = Some(updatedAnswers)))
              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))
                .fold(
                  logAndDisplayError("Could not capture select number of claims"),
                  _ => {
                    val redirectUrl = updatedAnswers match {
                      case SelectNumberOfClaimsAnswer.Individual => JourneyBindable.Single
                      case SelectNumberOfClaimsAnswer.Multiple   => JourneyBindable.Multiple
                      case SelectNumberOfClaimsAnswer.Scheduled  => JourneyBindable.Scheduled
                    }
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(redirectUrl))
                  }
                )
            }
          )
      }
    }

}

object SelectNumberOfClaimsController {

  val dataKey: String = "select-number-of-claims"

  val selectNumberOfClaimsAnswerForm: Form[SelectNumberOfClaimsAnswer] =
    Form(
      mapping(
        dataKey -> number
          .verifying("invalid", a => SelectNumberOfClaimsAnswer.allClaimsTypes.map(_.value).contains(a))
          .transform[SelectNumberOfClaimsAnswer](
            SelectNumberOfClaimsAnswer.allClaimsIntToType,
            SelectNumberOfClaimsAnswer.allClaimsTypeToInt
          )
      )(identity)(Some(_))
    )

}
