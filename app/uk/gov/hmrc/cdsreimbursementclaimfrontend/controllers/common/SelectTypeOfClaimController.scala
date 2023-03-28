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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.common

import cats.data.EitherT
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.Inject
import play.api.Environment
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.number
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Result
import shapeless.lens
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.TypeOfClaimAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.contactdetails.CdsVerifiedEmail
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.SignedInUserDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.VerifiedEmailAddressService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.common.select_number_of_claims
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class SelectTypeOfClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val featureSwitch: FeatureSwitchService,
  val verifiedEmailAddressService: VerifiedEmailAddressService,
  val servicesConfig: ServicesConfig,
  val env: Environment,
  selectNumberOfClaimsPage: select_number_of_claims
)(implicit
  viewConfig: ViewConfig,
  ec: ExecutionContext,
  errorHandler: ErrorHandler,
  val controllerComponents: MessagesControllerComponents
) extends FrontendBaseController
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  implicit val dataExtractor: DraftClaim => Option[TypeOfClaimAnswer] = _.typeOfClaim

  private val emailLens = lens[FillingOutClaim].signedInUserDetails.verifiedEmail

  def show(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAnswers[TypeOfClaimAnswer] { (_, answers) =>
      val emptyForm  = SelectTypeOfClaimController.typeOfClaimForm
      val filledForm = answers.fold(emptyForm)(emptyForm.fill)
      Ok(selectNumberOfClaimsPage(filledForm))
    }
  }

  def submit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSessionData { fillingOutClaim =>
      request.signedInUserDetails
        .map { user: SignedInUserDetails =>
          SelectTypeOfClaimController.typeOfClaimForm
            .bindFromRequest()
            .fold(
              formWithErrors => Future.successful(BadRequest(selectNumberOfClaimsPage(formWithErrors))),
              typeOfClaimAnswer => {
                import Logging._

                val logVerifiedEmailError: Error => Unit =
                  error => logger.warn("Error submitting a verified email", error)
                val logSelectClaimsError: Error => Unit  =
                  error => logger.warn("Could not capture select number of claims", error)
                val returnErrorPage: Unit => Result      = _ => errorHandler.errorResult()

                def saveSession(verifiedEmail: CdsVerifiedEmail): SessionData => SessionData = {
                  val updatedFillingOutClaim =
                    fillingOutClaim
                      .copy(draftClaim = fillingOutClaim.draftClaim.copy(typeOfClaim = Some(typeOfClaimAnswer)))
                  _.copy(journeyStatus = Some(emailLens.set(updatedFillingOutClaim)(verifiedEmail.toEmail)))
                }

                val eitherErrorOrNextPage: EitherT[Future, Result, Result] = for {
                  maybeVerifiedEmail <- EitherT(
                                          verifiedEmailAddressService
                                            .getVerifiedEmailAddress(user.eori)
                                        )
                                          .leftMap(logVerifiedEmailError.andThen(returnErrorPage))
                  verifiedEmail      <-
                    EitherT.fromOption[Future](maybeVerifiedEmail, Redirect(viewConfig.customsEmailFrontendUrl))
                  _                  <- EitherT(updateSession(sessionStore, request)(saveSession(verifiedEmail)))
                                          .leftMap(logSelectClaimsError.andThen(returnErrorPage))
                  result             <- EitherT.rightT[Future, Result](
                                          Redirect(
                                            claims.OverpaymentsRoutes.EnterMovementReferenceNumberController.enterJourneyMrn(
                                              typeOfClaimAnswer match {
                                                case TypeOfClaimAnswer.Individual => JourneyBindable.Single
                                                case TypeOfClaimAnswer.Multiple   => JourneyBindable.Multiple
                                                case TypeOfClaimAnswer.Scheduled  => JourneyBindable.Scheduled
                                              }
                                            )
                                          )
                                        )
                } yield result

                eitherErrorOrNextPage.merge
              }
            )
        }
        .getOrElse(Future.successful(Redirect(baseRoutes.StartController.start())))
    }
  }

  private def withSessionData(
    f: FillingOutClaim => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.using({ case fillingOutClaim @ FillingOutClaim(_, _, _: DraftClaim) =>
      f(fillingOutClaim)
    })
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
