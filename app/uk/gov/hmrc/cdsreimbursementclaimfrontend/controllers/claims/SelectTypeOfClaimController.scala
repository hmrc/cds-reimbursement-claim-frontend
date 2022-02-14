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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims

import cats.data.EitherT
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.number
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsMultipleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_number_of_claims
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.{routes => rejectGoodsSingleRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple.{routes => rejectGoodsMultipleRoutes}

import javax.inject.Singleton
import scala.concurrent.ExecutionContext

@Singleton
class SelectTypeOfClaimController @Inject() (
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

  implicit val dataExtractor: DraftClaim => Option[TypeOfClaimAnswer] = _.typeOfClaim

  def show(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[TypeOfClaimAnswer] { (_, answers) =>
      val emptyForm  = SelectTypeOfClaimController.typeOfClaimForm
      val filledForm = answers.fold(emptyForm)(emptyForm.fill)
      Ok(selectNumberOfClaimsPage(filledForm))
    }
  }

  def submit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[TypeOfClaimAnswer] { (fillingOutClaim, _) =>
      SelectTypeOfClaimController.typeOfClaimForm
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(selectNumberOfClaimsPage(formWithErrors)),
          typeOfClaimAnswer => {

            val updatedJourney =
              FillingOutClaim.from(fillingOutClaim)(_.copy(typeOfClaim = Some(typeOfClaimAnswer)))

            EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
              .leftMap(_ => Error("could not update session"))
              .fold(
                logAndDisplayError("Could not capture select number of claims"),
                _ => {
                  val redirectUrl = typeOfClaimAnswer match {
                    case TypeOfClaimAnswer.Individual => JourneyBindable.Single
                    case TypeOfClaimAnswer.Multiple   => JourneyBindable.Multiple
                    case TypeOfClaimAnswer.Scheduled  => JourneyBindable.Scheduled
                  }

                  if (request.sessionData.exists(_.isRejectedGoods)) {
                    Redirect(rejectedGoods(sessionStore, request, typeOfClaimAnswer, redirectUrl))
                  } else {
                    Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(redirectUrl))
                  }
                }
              )
          }
        )
    }
  }

  private def rejectedGoods(
    sessionStore: SessionCache,
    request: RequestWithSessionData[_],
    typeOfClaimAnswer: TypeOfClaimAnswer,
    redirectUrl: JourneyBindable
  )(implicit hc: HeaderCarrier): Call =
    (request.sessionData, request.signedInUserDetails) match {
      case (_, Some(user)) =>
        typeOfClaimAnswer match {
          case Individual | Scheduled =>
            val _ = updateSession(sessionStore, request)(
              _.copy(rejectedGoodsSingleJourney = Some(RejectedGoodsSingleJourney.empty(user.eori)))
            )
            rejectGoodsSingleRoutes.EnterMovementReferenceNumberController.show()
          case Multiple               =>
            val _ = updateSession(sessionStore, request)(
              _.copy(rejectedGoodsMultipleJourney = Some(RejectedGoodsMultipleJourney.empty(user.eori)))
            )
            rejectGoodsMultipleRoutes.RejectedGoodsMultipleEnterMRNController.enterJourneyMrn()
        }
      case _               => rejectGoodsSingleRoutes.EnterMovementReferenceNumberController.show()
    }

}

object SelectTypeOfClaimController {

  val dataKey: String = "select-number-of-claims"

  val typeOfClaimForm: Form[TypeOfClaimAnswer] =
    Form(
      mapping(
        dataKey -> number
          .verifying("invalid", a => TypeOfClaimAnswer.allClaimsTypes.map(_.value).contains(a))
          .transform[TypeOfClaimAnswer](
            TypeOfClaimAnswer.allClaimsIntToType,
            TypeOfClaimAnswer.allClaimsTypeToInt
          )
      )(identity)(Some(_))
    )

}
