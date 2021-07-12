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
import cats.syntax.either._
import cats.syntax.option._
import com.google.inject.{Inject, Singleton}
import julienrf.json.derived
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText, of}
import play.api.libs.json.OFormat
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDetailsRegisteredWithCdsController.{DetailsRegisteredWithCdsFormData, consigneeToClaimantDetails, declarantToClaimantDetails}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController.DeclarantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DetailsRegisteredWithCdsAnswer.{CompleteDetailsRegisteredWithCdsAnswer, IncompleteDetailsRegisteredWithCdsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.{Address, Country}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.TemporaryJourneyExtractor._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterDetailsRegisteredWithCdsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  val featureSwitch: FeatureSwitchService,
  cc: MessagesControllerComponents,
  detailsRegisteredWithCdsPage: pages.enter_claimant_details_as_registered_with_cds
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withDetailsRegisteredWithCdsAnswers(
    f: (
      SessionData,
      FillingOutClaim,
      DetailsRegisteredWithCdsAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({ case (sessionData, fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)) =>
      val maybeDetailsRegisteredWithCds = draftClaim.fold(_.detailsRegisteredWithCdsAnswer)
      maybeDetailsRegisteredWithCds.fold[Future[Result]](
        f(
          sessionData,
          fillingOutClaim,
          IncompleteDetailsRegisteredWithCdsAnswer.empty
        )
      )(answer => f(sessionData, fillingOutClaim, answer))
    })

  def enterDetailsRegisteredWithCds: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDetailsRegisteredWithCdsAnswers { (_, fillingOutClaim, answers) =>
        val emptyForm = EnterDetailsRegisteredWithCdsController.detailsRegisteredWithCdsForm.asRight[Result]

        def fillFormFromAcc14Data(
          maybeDisplayDeclaration: Option[DisplayDeclaration]
        ): Either[Result, Form[DetailsRegisteredWithCdsFormData]] =
          maybeDisplayDeclaration match {
            case Some(declaration) =>
              fillingOutClaim.draftClaim
                .fold(_.declarantTypeAnswer)
                .flatMap(_.declarantType)
                .fold(
                  Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType())
                    .asLeft[Form[DetailsRegisteredWithCdsFormData]]
                ) { declarantType =>
                  val email    = fillingOutClaim.signedInUserDetails.verifiedEmail
                  val formData = declarantType match {
                    case DeclarantType.Importer | DeclarantType.AssociatedWithImporterCompany =>
                      consigneeToClaimantDetails(declaration, email)
                    case DeclarantType.AssociatedWithRepresentativeCompany                    =>
                      declarantToClaimantDetails(declaration, email)
                  }
                  emptyForm.map(_.fill(formData))
                }
            case None              => //Acc14 was never called, this is an Entry Number/Chief Number
              emptyForm
          }

        answers
          .fold(_.detailsRegisteredWithCds, _.detailsRegisteredWithCds.some)
          .fold(fillFormFromAcc14Data(fillingOutClaim.draftClaim.fold(_.displayDeclaration)))(completeAnswer =>
            emptyForm.map(_.fill(completeAnswer))
          )
          .map(a => Ok(detailsRegisteredWithCdsPage(a)))
          .merge

      }
    }

  def enterDetailsRegisteredWithCdsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDetailsRegisteredWithCdsAnswers { (_, fillingOutClaim, answers) =>
        EnterDetailsRegisteredWithCdsController.detailsRegisteredWithCdsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                detailsRegisteredWithCdsPage(
                  requestFormWithErrors
                )
              ),
            detailsRegisteredWithCds => {
              val updatedAnswers = answers.fold(
                _ => CompleteDetailsRegisteredWithCdsAnswer(detailsRegisteredWithCds),
                complete => complete.copy(detailsRegisteredWithCds = detailsRegisteredWithCds)
              )
              val newDraftClaim  = if (detailsRegisteredWithCds.addCompanyDetails) {
                fillingOutClaim.draftClaim.fold(_.copy(detailsRegisteredWithCdsAnswer = Some(updatedAnswers)))
              } else {
                fillingOutClaim.draftClaim.fold(
                  _.copy(
                    detailsRegisteredWithCdsAnswer = Some(updatedAnswers),
                    contactDetailsAnswer = None
                  )
                )
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
                  if (detailsRegisteredWithCds.addCompanyDetails) {
                    Redirect(routes.EnterYourContactDetailsController.enterContactDetails())
                  } else {
                    fillingOutClaim.draftClaim.fold(_.movementReferenceNumber) match {
                      case Some(referenceNumber) =>
                        referenceNumber match {
                          case MovementReferenceNumber(Left(_))  =>
                            fillingOutClaim.draftClaim.declarantType match {
                              case Some(declarantType) =>
                                declarantType match {
                                  case DeclarantType.Importer =>
                                    Redirect(
                                      routes.SelectReasonForBasisAndClaimController.selectReasonForClaimAndBasis()
                                    )
                                  case _                      =>
                                    Redirect(routes.SelectBasisForClaimController.selectBasisForClaim(extractJourney))
                                }
                              case None                => Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType())
                            }
                          case MovementReferenceNumber(Right(_)) =>
                            featureSwitch.NorthernIreland.isEnabled() match {
                              case true  =>
                                Redirect(
                                  routes.ClaimNorthernIrelandController.selectNorthernIrelandClaim(extractJourney)
                                )
                              case false =>
                                Redirect(routes.SelectBasisForClaimController.selectBasisForClaim(extractJourney))
                            }
                        }
                      case None                  =>
                        Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                    }
                  }
              )
            }
          )
      }
    }

  def changeDetailsRegisteredWithCds: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDetailsRegisteredWithCdsAnswers { (_, _, answers) =>
        val emptyForm  = EnterDetailsRegisteredWithCdsController.detailsRegisteredWithCdsForm
        val filledForm = answers
          .fold(_.detailsRegisteredWithCds, _.detailsRegisteredWithCds.some)
          .fold(emptyForm)(emptyForm.fill(_))
        Ok(detailsRegisteredWithCdsPage(filledForm, isAmend = true))
      }
    }

  def changeDetailsRegisteredWithCdsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDetailsRegisteredWithCdsAnswers { (_, fillingOutClaim, answers) =>
        EnterDetailsRegisteredWithCdsController.detailsRegisteredWithCdsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                detailsRegisteredWithCdsPage(
                  requestFormWithErrors,
                  isAmend = true
                )
              ),
            detailsRegisteredWithCds => {
              val currentAnswer = answers.fold(
                ifIncomplete =>
                  ifIncomplete.detailsRegisteredWithCds match {
                    case Some(value) => Some(value.addCompanyDetails)
                    case None        => None
                  },
                ifComplete => Some(ifComplete.detailsRegisteredWithCds.addCompanyDetails)
              )

              (currentAnswer, Some(detailsRegisteredWithCds.addCompanyDetails)) match {
                case (Some(o), Some(n)) =>
                  if (o === n) {
                    // just update this page and move back to the CYA
                    val updatedAnswers = answers.fold(
                      _ => CompleteDetailsRegisteredWithCdsAnswer(detailsRegisteredWithCds),
                      complete => complete.copy(detailsRegisteredWithCds = detailsRegisteredWithCds)
                    )
                    val newDraftClaim  = fillingOutClaim.draftClaim
                      .fold(_.copy(detailsRegisteredWithCdsAnswer = Some(updatedAnswers)))

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
                  } else if (o) {
                    // this means user doesn't want the importer company details so we trash that
                    // and update the session
                    // and send them back to the CYA page
                    val updatedAnswers = answers.fold(
                      _ => CompleteDetailsRegisteredWithCdsAnswer(detailsRegisteredWithCds),
                      complete => complete.copy(detailsRegisteredWithCds = detailsRegisteredWithCds)
                    )
                    val newDraftClaim  = fillingOutClaim.draftClaim
                      .fold(
                        _.copy(
                          detailsRegisteredWithCdsAnswer = Some(updatedAnswers),
                          contactDetailsAnswer = None
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
                      _ => CompleteDetailsRegisteredWithCdsAnswer(detailsRegisteredWithCds),
                      complete => complete.copy(detailsRegisteredWithCds = detailsRegisteredWithCds)
                    )
                    val newDraftClaim  = fillingOutClaim.draftClaim
                      .fold(
                        _.copy(detailsRegisteredWithCdsAnswer = Some(updatedAnswers))
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
                          routes.EnterYourContactDetailsController.changeContactDetails()
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

object EnterDetailsRegisteredWithCdsController {

  final case class DetailsRegisteredWithCdsFormData(
    fullName: String,
    emailAddress: Email,
    contactAddress: NonUkAddress,
    addCompanyDetails: Boolean
  )

  object DetailsRegisteredWithCdsFormData {
    implicit val format: OFormat[DetailsRegisteredWithCdsFormData] =
      derived.oformat[DetailsRegisteredWithCdsFormData]()

  }

  val detailsRegisteredWithCdsForm: Form[DetailsRegisteredWithCdsFormData] = Form(
    mapping(
      "enter-claimant-details-as-registered-with-cds.individual-full-name" -> nonEmptyText(maxLength = 512),
      "enter-claimant-details-as-registered-with-cds.individual-email"     -> Email.mappingMaxLength,
      ""                                                                   -> Address.nonUkAddressFormMapping,
      "enter-claimant-details-as-registered-with-cds.add-company-details"  -> of(BooleanFormatter.formatter)
    )(DetailsRegisteredWithCdsFormData.apply)(DetailsRegisteredWithCdsFormData.unapply)
  )

  def consigneeToClaimantDetails(
    displayDeclaration: DisplayDeclaration,
    verifiedEmail: Email
  ): DetailsRegisteredWithCdsFormData = {
    val declaration          = displayDeclaration.displayResponseDetail
    val establishmentAddress = declaration.consigneeDetails.map(p => p.establishmentAddress)
    DetailsRegisteredWithCdsFormData(
      declaration.consigneeDetails.map(_.legalName).getOrElse(""),
      verifiedEmail,
      NonUkAddress(
        establishmentAddress.map(_.addressLine1).getOrElse(""),
        establishmentAddress.flatMap(_.addressLine2),
        None,
        establishmentAddress.flatMap(_.addressLine3).getOrElse(""),
        establishmentAddress.flatMap(_.postalCode).getOrElse(""),
        establishmentAddress.map(cc => Country(cc.countryCode)).getOrElse(Country.uk)
      ),
      addCompanyDetails = false
    )
  }

  def declarantToClaimantDetails(
    displayDeclaration: DisplayDeclaration,
    verifiedEmail: Email
  ): DetailsRegisteredWithCdsFormData = {
    val declaration          = displayDeclaration.displayResponseDetail
    val establishmentAddress = declaration.declarantDetails.establishmentAddress
    DetailsRegisteredWithCdsFormData(
      declaration.declarantDetails.legalName,
      verifiedEmail,
      NonUkAddress(
        establishmentAddress.addressLine1,
        establishmentAddress.addressLine2,
        None,
        establishmentAddress.addressLine3.getOrElse(""),
        establishmentAddress.postalCode.getOrElse(""),
        Country(establishmentAddress.countryCode)
      ),
      addCompanyDetails = false
    )
  }

}
