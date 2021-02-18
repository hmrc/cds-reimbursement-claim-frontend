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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimantDetailsAsIndividualController.toClaimantDetailsAsIndividual
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController.DeclarantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantDetailsAsIndividualAnswer.{CompleteClaimantDetailsAsIndividualAnswer, IncompleteClaimantDetailsAsIndividualAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.{Address, Country}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.Declaration
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
      withClaimantDetailsAsIndividualAnswers { (_, fillingOutClaim, answers) =>
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
                fillingOutClaim.draftClaim.fold(_.maybeDeclaration) match {
                  case Some(declaration) =>
                    fillingOutClaim.draftClaim.fold(_.declarantTypeAnswer) match {
                      case Some(declarantTypeAnswer) =>
                        declarantTypeAnswer.declarantType match {
                          case Some(declarantType) =>
                            declarantType match {
                              case DeclarantType.Importer =>
                                Ok(
                                  enterClaimantDetailAsIndividualPage(
                                    EnterClaimantDetailsAsIndividualController.claimantDetailsAsIndividualForm
                                      .fill(
                                        toClaimantDetailsAsIndividual(declaration)
                                      )
                                  )
                                )
                              case _                      =>
                                Ok(
                                  enterClaimantDetailAsIndividualPage(
                                    EnterClaimantDetailsAsIndividualController.claimantDetailsAsIndividualForm
                                  )
                                )
                            }
                          case None                =>
                            Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType())
                        }
                      case None                      =>
                        Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType())
                    }
                  case None              =>
                    Ok(
                      enterClaimantDetailAsIndividualPage(
                        EnterClaimantDetailsAsIndividualController.claimantDetailsAsIndividualForm
                      )
                    )
                }
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
            requestFormWithErrors => {
              println(s"${requestFormWithErrors.toString}")
              BadRequest(
                enterClaimantDetailAsIndividualPage(
                  requestFormWithErrors
                )
              )
            },
            claimantDetailsAsIndividual => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteClaimantDetailsAsIndividualAnswer(
                    claimantDetailsAsIndividual
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
                      fillingOutClaim.draftClaim.fold(_.movementReferenceNumber) match {
                        case Some(referenceNumber) =>
                          referenceNumber match {
                            case Left(_)  =>
                              fillingOutClaim.draftClaim.declarantType match {
                                case Some(declarantType) =>
                                  declarantType match {
                                    case DeclarantType.Importer =>
                                      Redirect(
                                        routes.SelectReasonForBasisAndClaimController.selectReasonForClaimAndBasis()
                                      )
                                    case _                      => Redirect(routes.SelectReasonForClaimController.selectReasonForClaim())
                                  }
                                case None                => Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType())
                              }
                            case Right(_) => Redirect(routes.SelectReasonForClaimController.selectReasonForClaim())
                          }
                        case None                  => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
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

  def changeClaimantDetailsAsIndividual: Action[AnyContent] =
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
                    ),
                    isAmend = true
                  )
                )
              case None                              =>
                Ok(
                  enterClaimantDetailAsIndividualPage(
                    EnterClaimantDetailsAsIndividualController.claimantDetailsAsIndividualForm,
                    isAmend = true
                  )
                )
            },
          ifComplete =>
            Ok(
              enterClaimantDetailAsIndividualPage(
                EnterClaimantDetailsAsIndividualController.claimantDetailsAsIndividualForm.fill(
                  ifComplete.claimantDetailsAsIndividual
                ),
                isAmend = true
              )
            )
        )
      }
    }

  def changeClaimantDetailsAsIndividualSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withClaimantDetailsAsIndividualAnswers { (_, fillingOutClaim, answers) =>
        EnterClaimantDetailsAsIndividualController.claimantDetailsAsIndividualForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterClaimantDetailAsIndividualPage(
                  requestFormWithErrors,
                  isAmend = true
                )
              ),
            claimantDetailsAsIndividual => {
              val currentAnswer = answers.fold(
                ifIncomplete =>
                  ifIncomplete.claimantDetailsAsIndividual match {
                    case Some(value) => Some(value.addCompanyDetails)
                    case None        => None
                  },
                ifComplete => Some(ifComplete.claimantDetailsAsIndividual.addCompanyDetails)
              )

              (currentAnswer, Some(claimantDetailsAsIndividual.addCompanyDetails)) match {
                case (Some(o), Some(n)) =>
                  if (o === n) {
                    // just update this page and move back to the CYA
                    val updatedAnswers = answers.fold(
                      _ => CompleteClaimantDetailsAsIndividualAnswer(claimantDetailsAsIndividual),
                      complete => complete.copy(claimantDetailsAsIndividual = claimantDetailsAsIndividual)
                    )
                    val newDraftClaim  = fillingOutClaim.draftClaim
                      .fold(_.copy(claimantDetailsAsIndividualAnswers = Some(updatedAnswers)))

                    val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                    val result = EitherT
                      .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                      .leftMap((_: Unit) => Error("could not update session"))

                    result.fold(
                      e => {
                        logger.warn("could not capture claimant as individual details", e)
                        errorHandler.errorResult()
                      },
                      _ => Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
                    )
                  } else if (o === YesNo.Yes) {
                    // this means user doesn't want the importer company details so we trash that
                    // and update the session
                    // and send them back to the CYA page
                    val updatedAnswers = answers.fold(
                      _ => CompleteClaimantDetailsAsIndividualAnswer(claimantDetailsAsIndividual),
                      complete => complete.copy(claimantDetailsAsIndividual = claimantDetailsAsIndividual)
                    )
                    val newDraftClaim  = fillingOutClaim.draftClaim
                      .fold(
                        _.copy(
                          claimantDetailsAsIndividualAnswers = Some(updatedAnswers),
                          claimantDetailsAsImporterCompanyAnswers = None
                        )
                      )

                    val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                    val result = EitherT
                      .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                      .leftMap((_: Unit) => Error("could not update session"))

                    result.fold(
                      e => {
                        logger.warn("could not capture claimant as individual details", e)
                        errorHandler.errorResult()
                      },
                      _ => Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
                    )

                  } else {
                    // this means that they do want to add importer company details now so send them to that page with change state
                    val updatedAnswers = answers.fold(
                      _ => CompleteClaimantDetailsAsIndividualAnswer(claimantDetailsAsIndividual),
                      complete => complete.copy(claimantDetailsAsIndividual = claimantDetailsAsIndividual)
                    )
                    val newDraftClaim  = fillingOutClaim.draftClaim
                      .fold(
                        _.copy(claimantDetailsAsIndividualAnswers = Some(updatedAnswers))
                      )

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
                        Redirect(
                          routes.EnterClaimantDetailsAsImporterCompanyController
                            .changeClaimantDetailsAsImporterCompany()
                        )
                    )
                  }
                case _                  => Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswers())
              }
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

  def toClaimantDetailsAsIndividual(declaration: Declaration): ClaimantDetailsAsIndividual = {
    val d = Declaration.DeclarationOps(declaration)
    val a = declaration.consigneeDetails.flatMap(p => p.contactDetails)
    ClaimantDetailsAsIndividual(
      d.consigneeName.getOrElse(""),
      Email(d.consigneeEmail.getOrElse("")),
      d.consigneeTelephone.getOrElse(""),
      NonUkAddress(
        a.flatMap(s => s.addressLine1).getOrElse(""),
        a.flatMap(s => s.addressLine2).getOrElse(""),
        None,
        a.flatMap(s => s.addressLine3),
        a.flatMap(s => s.addressLine4).getOrElse(""),
        None,
        a.flatMap(s => s.postalCode),
        Country(a.flatMap(s => s.countryCode).getOrElse(""))
      ),
      YesNo.No
    )
  }
}
