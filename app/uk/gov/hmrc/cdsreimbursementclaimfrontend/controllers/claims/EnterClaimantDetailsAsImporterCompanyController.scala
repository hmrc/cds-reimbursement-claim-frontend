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
import com.google.inject.{Inject, Singleton}
import julienrf.json.derived
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimantDetailsAsImporterCompanyController.toClaimantDetailsAsImporter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController.DeclarantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimantDetailsAsImporterCompanyAnswer.{CompleteClaimantDetailsAsImporterCompanyAnswer, IncompleteClaimantDetailsAsImporterCompanyAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.{Address, Country}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.Declaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterClaimantDetailsAsImporterCompanyController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  enterClaimantDetailAsImporterCompanyPage: pages.enter_claimant_details_as_importer_company
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withClaimantDetailsAsImporterCompanyAnswers(
    f: (
      SessionData,
      FillingOutClaim,
      ClaimantDetailsAsImporterCompanyAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeClaimantDetailsAsImporterCompany = draftClaim.fold(
          _.claimantDetailsAsImporterCompanyAnswers
        )
        maybeClaimantDetailsAsImporterCompany.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteClaimantDetailsAsImporterCompanyAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  def enterClaimantDetailsAsImporterCompany: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withClaimantDetailsAsImporterCompanyAnswers { (_, fillingOutClaim, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.claimantDetailsAsImporterCompany match {
              case Some(claimantDetailsAsImporterCompany) =>
                Ok(
                  enterClaimantDetailAsImporterCompanyPage(
                    EnterClaimantDetailsAsImporterCompanyController.claimantDetailsAsImporterCompanyForm.fill(
                      claimantDetailsAsImporterCompany
                    )
                  )
                )
              case None                                   =>
                fillingOutClaim.draftClaim.fold(_.maybeDeclaration) match {
                  case Some(declaration) =>
                    fillingOutClaim.draftClaim.fold(_.declarantTypeAnswer) match {
                      case Some(declarantTypeAnswer) =>
                        declarantTypeAnswer.declarantType match {
                          case Some(declarantType) =>
                            declarantType match {
                              case DeclarantType.Importer =>
                                Ok(
                                  enterClaimantDetailAsImporterCompanyPage(
                                    EnterClaimantDetailsAsImporterCompanyController.claimantDetailsAsImporterCompanyForm
                                      .fill(
                                        toClaimantDetailsAsImporter(declaration)
                                      )
                                  )
                                )
                              case _                      =>
                                Ok(
                                  enterClaimantDetailAsImporterCompanyPage(
                                    EnterClaimantDetailsAsImporterCompanyController.claimantDetailsAsImporterCompanyForm
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
                      enterClaimantDetailAsImporterCompanyPage(
                        EnterClaimantDetailsAsImporterCompanyController.claimantDetailsAsImporterCompanyForm
                      )
                    )
                }
            },
          ifComplete =>
            Ok(
              enterClaimantDetailAsImporterCompanyPage(
                EnterClaimantDetailsAsImporterCompanyController.claimantDetailsAsImporterCompanyForm.fill(
                  ifComplete.claimantDetailsAsImporterCompany
                )
              )
            )
        )
      }
    }

  def enterClaimantDetailsAsImporterCompanySubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withClaimantDetailsAsImporterCompanyAnswers { (_, fillingOutClaim, answers) =>
        EnterClaimantDetailsAsImporterCompanyController.claimantDetailsAsImporterCompanyForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterClaimantDetailAsImporterCompanyPage(
                  requestFormWithErrors
                )
              ),
            claimantDetailsAsImporterCompany => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteClaimantDetailsAsImporterCompanyAnswer(
                    claimantDetailsAsImporterCompany
                  ),
                complete => complete.copy(claimantDetailsAsImporterCompany = claimantDetailsAsImporterCompany)
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(claimantDetailsAsImporterCompanyAnswers = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not capture importer company details", e)
                  errorHandler.errorResult()
                },
                _ =>
                  fillingOutClaim.draftClaim.fold(_.movementReferenceNumber) match {
                    case Some(referenceNumber) =>
                      referenceNumber match {
                        case Left(_)  =>
                          fillingOutClaim.draftClaim.declarantType match {
                            case Some(declarantType) =>
                              declarantType match {
                                case DeclarantType.Importer =>
                                  Redirect(routes.SelectReasonForBasisAndClaimController.selectReasonForClaimAndBasis())
                                case _                      => Redirect(routes.SelectReasonForClaimController.selectReasonForClaim())
                              }
                            case None                => Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType())
                          }
                        case Right(_) => Redirect(routes.SelectReasonForClaimController.selectReasonForClaim())
                      }
                    case None                  => Redirect(routes.EnterMovementReferenceNumberController.enterMrn())
                  }
              )
            }
          )
      }
    }

  def changeClaimantDetailsAsImporterCompany: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withClaimantDetailsAsImporterCompanyAnswers { (_, _, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.claimantDetailsAsImporterCompany match {
              case Some(claimantDetailsAsImporterCompany) =>
                Ok(
                  enterClaimantDetailAsImporterCompanyPage(
                    EnterClaimantDetailsAsImporterCompanyController.claimantDetailsAsImporterCompanyForm.fill(
                      claimantDetailsAsImporterCompany
                    ),
                    isAmend = true
                  )
                )
              case None                                   =>
                Ok(
                  enterClaimantDetailAsImporterCompanyPage(
                    EnterClaimantDetailsAsImporterCompanyController.claimantDetailsAsImporterCompanyForm,
                    isAmend = true
                  )
                )
            },
          ifComplete =>
            Ok(
              enterClaimantDetailAsImporterCompanyPage(
                EnterClaimantDetailsAsImporterCompanyController.claimantDetailsAsImporterCompanyForm.fill(
                  ifComplete.claimantDetailsAsImporterCompany
                ),
                isAmend = true
              )
            )
        )
      }
    }

  def changeClaimantDetailsAsImporterCompanySubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withClaimantDetailsAsImporterCompanyAnswers { (_, fillingOutClaim, answers) =>
        EnterClaimantDetailsAsImporterCompanyController.claimantDetailsAsImporterCompanyForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterClaimantDetailAsImporterCompanyPage(
                  requestFormWithErrors
                )
              ),
            claimantDetailsAsImporterCompany => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteClaimantDetailsAsImporterCompanyAnswer(
                    claimantDetailsAsImporterCompany
                  ),
                complete => complete.copy(claimantDetailsAsImporterCompany = claimantDetailsAsImporterCompany)
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(claimantDetailsAsImporterCompanyAnswers = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not capture importer company details", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswersSubmit())
              )
            }
          )
      }
    }
}

object EnterClaimantDetailsAsImporterCompanyController {

  final case class ClaimantDetailsAsImporterCompany(
    companyName: String,
    emailAddress: Email,
    phoneNumber: PhoneNumber,
    contactAddress: NonUkAddress
  )

  object ClaimantDetailsAsImporterCompany {
    implicit val format: OFormat[ClaimantDetailsAsImporterCompany] = Json.format[ClaimantDetailsAsImporterCompany]
  }

  object ClaimantDetailsAsIndividual {
    implicit val format: OFormat[ClaimantDetailsAsImporterCompany] = derived.oformat[ClaimantDetailsAsImporterCompany]()
  }

  val claimantDetailsAsImporterCompanyForm: Form[ClaimantDetailsAsImporterCompany] = Form(
    mapping(
      "enter-claimant-details-importer-company.importer-company-name" -> nonEmptyText,
      "enter-claimant-details-importer-company.importer-email"        -> Email.mapping,
      "enter-claimant-details-importer-company.importer-phone-number" -> PhoneNumber.mapping,
      ""                                                              -> Address.nonUkAddressFormMapping
    )(ClaimantDetailsAsImporterCompany.apply)(ClaimantDetailsAsImporterCompany.unapply)
  )

  def toClaimantDetailsAsImporter(declaration: Declaration): ClaimantDetailsAsImporterCompany = {
    val maybeAddress = declaration.consigneeDetails.map(p => p.establishmentAddress)
    ClaimantDetailsAsImporterCompany(
      "",
      Email(""),
      PhoneNumber(""),
      NonUkAddress(
        maybeAddress.map(s => s.addressLine1).getOrElse(""),
        maybeAddress.flatMap(s => s.addressLine2).getOrElse(""),
        None,
        maybeAddress.flatMap(s => s.addressLine3),
        "",
        None,
        maybeAddress.flatMap(s => s.postalCode),
        Country(maybeAddress.map(s => s.countryCode).getOrElse(""))
      )
    )
  }
}
