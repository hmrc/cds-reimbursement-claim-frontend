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
import cats.implicits.catsSyntaxEq
import com.google.inject.{Inject, Singleton}
import julienrf.json.derived
import play.api.data.Forms.{mapping, nonEmptyText, number}
import play.api.data.{Form, Mapping}
import play.api.libs.json.OFormat
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController.DeclarantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantDetailsAsIndividualAnswer.IncompleteClaimantDetailsAsIndividualAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterClaimantDetailsAsIndividualController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  enterClaimantDetailAsIndividualPage: pages.enter_claimant_details_as_individual
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withClaimantDetailsAsIndividualAnswers(
    f: (
      SessionData,
      FillingOutClaim,
      ClaimantDetailsAsIndividualAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeClaimantDetailsAsIndividualAnswer = draftClaim.fold(
          _.claimantDetailsAsIndividualAnswers
        )
        maybeClaimantDetailsAsIndividualAnswer.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteClaimantDetailsAsIndividualAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def enterClaimantDetailsAsIndividual: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withClaimantDetailsAsIndividualAnswers { (_, _, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.claimantDetailsAsIndividual match {
              case Some(claimantDetailsAsIndividual) =>
                Ok(
                  enterClaimantDetailAsIndividualPage(
                    EnterClaimantDetailsAsIndividualController.claimantDetailsAsIndividualForm.fill(
                      claimantDetailsAsIndividual
                    )
                  )
                )
              case None                              =>
                Ok(
                  enterClaimantDetailAsIndividualPage(
                    EnterClaimantDetailsAsIndividualController.claimantDetailsAsIndividualForm
                  )
                )
            },
          ifComplete =>
            Ok(
              enterClaimantDetailAsIndividualPage(
                EnterClaimantDetailsAsIndividualController.claimantDetailsAsIndividualForm.fill(
                  ifComplete.claimantDetailsAsIndividual
                )
              )
            )
        )
      }
    }

  def enterClaimantDetailsAsIndividualSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withClaimantDetailsAsIndividualAnswers { (_, fillingOutClaim, answers) =>
        EnterClaimantDetailsAsIndividualController.claimantDetailsAsIndividualForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterClaimantDetailAsIndividualPage(
                  requestFormWithErrors
                )
              ),
            claimantDetailsAsIndividual => {
              val updatedAnswers = answers.fold(
                incomplete =>
                  incomplete.copy(
                    claimantDetailsAsIndividual = Some(claimantDetailsAsIndividual)
                  ),
                complete => complete.copy(claimantDetailsAsIndividual = claimantDetailsAsIndividual)
              )
              val newDraftClaim  = if (claimantDetailsAsIndividual.addCompanyDetails === YesNo.No) {
                fillingOutClaim.draftClaim.fold(
                  _.copy(
                    claimantDetailsAsIndividualAnswers = Some(updatedAnswers),
                    claimantDetailsAsImporterCompanyAnswers = None
                  )
                )
              } else {
                fillingOutClaim.draftClaim.fold(_.copy(claimantDetailsAsIndividualAnswers = Some(updatedAnswers)))
              }
              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not capture claimant as individual details", e)
                  errorHandler.errorResult()
                },
                _ =>
                  claimantDetailsAsIndividual.addCompanyDetails match {
                    case YesNo.No  =>
                      fillingOutClaim.draftClaim.declarantType match {
                        case Some(declarantType) =>
                          declarantType match {
                            case DeclarantType.Importer =>
                              Redirect(routes.SelectReasonForBasisAndClaimController.selectReasonForClaimAndBasis())
                            case _                      =>
                              Redirect(routes.SelectReasonForBasisAndClaimController.selectReasonForClaimAndBasis())

                          }
                        case None                => Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType())
                      }
                    case YesNo.Yes =>
                      Redirect(
                        routes.EnterClaimantDetailsAsImporterCompanyController.enterClaimantDetailsAsImporterCompany()
                      )
                  }
              )
            }
          )
      }
    }
}

object EnterClaimantDetailsAsIndividualController {

  val companyDetailsMapping: Mapping[YesNo] =
    mapping(
      "enter-claimant-details-individual.add-company-details" -> number
        .verifying("invalid", a => a === 0 || a === 1)
        .transform[YesNo](
          value => if (value === 0) YesNo.No else YesNo.Yes,
          {
            case YesNo.Yes => 1
            case YesNo.No  => 0
          }
        )
    )(identity)(Some(_))

  final case class ClaimantDetailsAsIndividual(
    fullName: String,
    emailAddress: Email,
    phoneNumber: String,
    contactAddress: NonUkAddress,
    addCompanyDetails: YesNo
  )

  object ClaimantDetailsAsIndividual {
    implicit val format: OFormat[ClaimantDetailsAsIndividual] = derived.oformat[ClaimantDetailsAsIndividual]()
  }

  val claimantDetailsAsIndividualForm: Form[ClaimantDetailsAsIndividual] = Form(
    mapping(
      "enter-claimant-details-individual.importer-full-name"    -> nonEmptyText,
      "enter-claimant-details-individual.importer-email"        -> Email.mapping,
      "enter-claimant-details-individual.importer-phone-number" -> nonEmptyText,
      ""                                                        -> Address.nonUkAddressFormMapping,
      ""                                                        -> companyDetailsMapping
    )(ClaimantDetailsAsIndividual.apply)(ClaimantDetailsAsIndividual.unapply)
  )
}
