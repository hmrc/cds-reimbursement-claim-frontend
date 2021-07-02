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
import cats.data.EitherT
import cats.implicits._
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.Inject
import julienrf.json.derived
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.libs.json.OFormat
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectNumberOfClaimsController.SelectNumberOfClaimsType._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectNumberOfClaimsController.{SelectNumberOfClaimsType, selectNumberOfClaimsAnswerForm}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.InitialClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.journey.ClaimType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{FeatureSwitchService, SessionService}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.claims.select_number_of_claims
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SelectNumberOfClaimsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  sessionService: SessionService,
  selectNumberOfClaimsPage: select_number_of_claims
)(implicit
  viewConfig: ViewConfig,
  errorHandler: ErrorHandler,
  ec: ExecutionContext,
  cc: MessagesControllerComponents
) extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def show(): Action[AnyContent] = (featureSwitch.BulkClaim.hideIfNotEnabled andThen
    authenticatedActionWithSessionData).async { implicit request =>
    sessionService
      .getAnswers({ case claim: InitialClaim => claim.claimType })
      .fold(
        error => {
          logger.warn(error.message)
          errorHandler.errorResult()
        },
        answer => {
          val filledForm = answer.foldLeft(selectNumberOfClaimsAnswerForm)((form, answer) =>
            form.fill(SelectNumberOfClaimsType.map(answer))
          )
          Ok(selectNumberOfClaimsPage(filledForm))
        }
      )
  }

  def submit(): Action[AnyContent] =
    (featureSwitch.BulkClaim.hideIfNotEnabled andThen authenticatedActionWithSessionData).async { implicit request =>
      selectNumberOfClaimsAnswerForm
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(selectNumberOfClaimsPage(formWithErrors)),
          formOk => {
            val status = for {
              claim  <- sessionService.getAnswers({ case claim: InitialClaim => claim })
              updated = claim.copy(claimType = SelectNumberOfClaimsType.unmap(formOk).some)
              _      <- EitherT
                          .fromOption[Future](request.sessionData, Error("No session data"))
                          .flatMap(sessionData => sessionService.persist(sessionData.copy(journeyStatus = updated.some)))
            } yield ()

            status.fold(
              error => {
                logger.warn(error.message)
                errorHandler.errorResult()
              },
              _ => Redirect(routes.DummyReferenceNumberController.show())
            )
          }
        )
    }

}

object SelectNumberOfClaimsController {

  sealed abstract class SelectNumberOfClaimsType(val value: Int) extends Product with Serializable

  object SelectNumberOfClaimsType {
    case object Individual extends SelectNumberOfClaimsType(0)
    case object Bulk extends SelectNumberOfClaimsType(1)
    case object Scheduled extends SelectNumberOfClaimsType(2)

    val allClaimsTypes: List[SelectNumberOfClaimsType]         = List(Individual, Bulk, Scheduled)
    val allClaimsIntToType: Map[Int, SelectNumberOfClaimsType] = allClaimsTypes.map(a => a.value -> a).toMap
    val allClaimsTypeToInt: Map[SelectNumberOfClaimsType, Int] = allClaimsTypes.map(a => a -> a.value).toMap

    implicit val eq: Eq[SelectNumberOfClaimsType]                                  = Eq.fromUniversalEquals
    implicit val selectNumberOfClaimsTypeFormat: OFormat[SelectNumberOfClaimsType] =
      derived.oformat[SelectNumberOfClaimsType]()

    def map(claimType: ClaimType): SelectNumberOfClaimsType =
      claimType match {
        case ClaimType.Single   => Individual
        case ClaimType.Bulk     => Bulk
        case ClaimType.Schedule => Scheduled
      }

    def unmap(selectNumberOfClaimsType: SelectNumberOfClaimsType): ClaimType =
      selectNumberOfClaimsType match {
        case Individual => ClaimType.Single
        case Bulk       => ClaimType.Bulk
        case Scheduled  => ClaimType.Schedule
      }
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
