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
import cats.implicits._
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.Inject
import julienrf.json.derived
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.libs.json.OFormat
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectNumberOfClaimsController.SelectNumberOfClaimsType._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SelectNumberOfClaimsAnswer.{CompleteSelectNumberOfClaimsAnswer, IncompleteSelectNumberOfClaimsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, Error, SelectNumberOfClaimsAnswer, SessionData}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_number_of_claims
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.Singleton
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class SelectNumberOfClaimsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  selectNumberOfClaimsPage: select_number_of_claims
)(implicit
  viewConfig: ViewConfig,
  errorHandler: ErrorHandler,
  cc: MessagesControllerComponents
) extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withSelectNumberOfClaimsAnswers(
    f: (
      SessionData,
      FillingOutClaim,
      SelectNumberOfClaimsAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({ case (sessionData, fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)) =>
      draftClaim
        .fold(_.selectNumberOfClaimsAnswer)
        .fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteSelectNumberOfClaimsAnswer.empty)
        )(answer => f(sessionData, fillingOutClaim, answer))
    })

  def show(): Action[AnyContent] = (featureSwitch.BulkClaim.hideIfNotEnabled andThen
    authenticatedActionWithSessionData).async { implicit request =>
    withSelectNumberOfClaimsAnswers { (_, _, answers) =>
      val emptyForm  = SelectNumberOfClaimsController.selectNumberOfClaimsAnswerForm
      val filledForm = answers
        .fold(_.selectNumberOfClaimsChoice, _.selectNumberOfClaimsChoice.some)
        .fold(emptyForm)(emptyForm.fill(_))
      Ok(selectNumberOfClaimsPage(filledForm))
    }
  }

  def submit(): Action[AnyContent] =
    (featureSwitch.BulkClaim.hideIfNotEnabled andThen authenticatedActionWithSessionData).async { implicit request =>
      withSelectNumberOfClaimsAnswers { (_, fillingOutClaim, _) =>
        SelectNumberOfClaimsController.selectNumberOfClaimsAnswerForm
          .bindFromRequest()
          .fold(
            formWithErros => BadRequest(selectNumberOfClaimsPage(formWithErros)),
            formOk => {
              val updatedAnswers = CompleteSelectNumberOfClaimsAnswer(formOk)
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(selectNumberOfClaimsAnswer = Some(updatedAnswers)))
              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))
                .fold(
                  e => {
                    logger.warn("could not capture select number of claims", e)
                    errorHandler.errorResult()
                  },
                  _ =>
                    formOk match {
                      case Individual => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                      case Bulk       => Redirect(routes.EnterMrnMultiController.show())
                      case Scheduled  => Redirect(routes.EnterMrnScheduleController.show())
                    }
                )

            }
          )

      }
    }

}

object SelectNumberOfClaimsController {

  sealed abstract class SelectNumberOfClaimsType(val value: Int) extends Product with Serializable

  object SelectNumberOfClaimsType {
    case object Individual extends SelectNumberOfClaimsType(0)
    case object Bulk extends SelectNumberOfClaimsType(1)
    case object Scheduled extends SelectNumberOfClaimsType(2)
    case object CMA extends SelectNumberOfClaimsType(3)

    val allClaimsTypes: List[SelectNumberOfClaimsType]         = List(Individual, Bulk, Scheduled)
    val allClaimsIntToType: Map[Int, SelectNumberOfClaimsType] = allClaimsTypes.map(a => a.value -> a).toMap
    val allClaimsTypeToInt: Map[SelectNumberOfClaimsType, Int] = allClaimsTypes.map(a => a -> a.value).toMap

    implicit val selectNumberOfClaimsTypeFormat: OFormat[SelectNumberOfClaimsType] =
      derived.oformat[SelectNumberOfClaimsType]()
  }

  val dataKey: String = "select-number-of-claims"

  val selectNumberOfClaimsAnswerForm: Form[SelectNumberOfClaimsType] =
    Form(
      mapping(
        dataKey -> number
          .verifying("invalid", a => allClaimsTypes.map(_.value).contains(a))
          .transform[SelectNumberOfClaimsType](allClaimsIntToType, allClaimsTypeToInt)
      )(identity)(Some(_))
    )

}
