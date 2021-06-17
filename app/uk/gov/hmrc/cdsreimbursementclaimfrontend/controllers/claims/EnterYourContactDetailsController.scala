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
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterYourContactDetailsController.toContactDetailsFormData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectWhoIsMakingTheClaimController.DeclarantType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ContactDetailsAnswer.{CompleteContactDetailsAnswer, IncompleteContactDetailsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.Address.NonUkAddress
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.address.{Address, Country}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.ContactDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.email.Email
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.phonenumber.PhoneNumber
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterYourContactDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  val featureSwitch: FeatureSwitchService,
  cc: MessagesControllerComponents,
  enterYourContactDetailsPage: pages.enter_your_contact_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withContactDetailsAnswers(
    f: (
      SessionData,
      FillingOutClaim,
      ContactDetailsAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({
      case (
            sessionData,
            fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
          ) =>
        val maybeContactDetails = draftClaim.fold(
          _.contactDetailsAnswer
        )
        maybeContactDetails.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteContactDetailsAnswer.empty)
        )(answer => f(sessionData, fillingOutClaim, addEmail(fillingOutClaim, answer)))
    })

  private def addEmail(
    fillingOutClaim: FillingOutClaim,
    claimantDetails: ContactDetailsAnswer
  ): ContactDetailsAnswer =
    claimantDetails.fold(
      ifIncomplete =>
        IncompleteContactDetailsAnswer(
          ifIncomplete.contactDetailsFormData.map(
            _.copy(emailAddress = fillingOutClaim.signedInUserDetails.verifiedEmail)
          )
        ),
      ifComplete =>
        CompleteContactDetailsAnswer(
          ifComplete.contactDetailsFormData.copy(emailAddress = fillingOutClaim.signedInUserDetails.verifiedEmail)
        )
    )

  def enterContactDetails: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withContactDetailsAnswers { (_, fillingOutClaim, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.contactDetailsFormData match {
              case Some(contactDetailsFormData) =>
                Ok(
                  enterYourContactDetailsPage(
                    EnterYourContactDetailsController.contactDetailsForm.fill(contactDetailsFormData)
                  )
                )
              case None                         =>
                fillingOutClaim.draftClaim.fold(_.displayDeclaration) match {
                  case Some(displayDeclaration) =>
                    fillingOutClaim.draftClaim.fold(_.declarantTypeAnswer) match {
                      case Some(declarantTypeAnswer) =>
                        declarantTypeAnswer.declarantType match {
                          case Some(declarantType) =>
                            val contactDetails = declarantType match {
                              case DeclarantType.Importer | DeclarantType.AssociatedWithImporterCompany =>
                                toContactDetailsFormData(
                                  displayDeclaration.displayResponseDetail.consigneeDetails.flatMap(_.contactDetails),
                                  fillingOutClaim.signedInUserDetails.verifiedEmail
                                )
                              case DeclarantType.AssociatedWithRepresentativeCompany                    =>
                                toContactDetailsFormData(
                                  displayDeclaration.displayResponseDetail.declarantDetails.contactDetails,
                                  fillingOutClaim.signedInUserDetails.verifiedEmail
                                )
                            }
                            Ok(
                              enterYourContactDetailsPage(
                                EnterYourContactDetailsController.contactDetailsForm
                                  .fill(contactDetails)
                              )
                            )
                          case None                =>
                            Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType())
                        }
                      case None                      =>
                        Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType())
                    }
                  case None                     =>
                    Ok(
                      enterYourContactDetailsPage(
                        EnterYourContactDetailsController.contactDetailsForm
                      )
                    )
                }
            },
          ifComplete =>
            Ok(
              enterYourContactDetailsPage(
                EnterYourContactDetailsController.contactDetailsForm.fill(
                  ifComplete.contactDetailsFormData
                )
              )
            )
        )
      }
    }

  def enterContactDetailsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withContactDetailsAnswers { (_, fillingOutClaim, answers) =>
        EnterYourContactDetailsController.contactDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterYourContactDetailsPage(
                  requestFormWithErrors
                )
              ),
            contactDetailsFormData => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteContactDetailsAnswer(
                    contactDetailsFormData
                  ),
                complete => complete.copy(contactDetailsFormData = contactDetailsFormData)
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(contactDetailsAnswer = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not capture contact details", e)
                  errorHandler.errorResult()
                },
                _ =>
                  fillingOutClaim.draftClaim.fold(_.movementReferenceNumber) match {
                    case Some(referenceNumber) =>
                      referenceNumber match {
                        case MovementReferenceNumber(Left(_))  =>
                          fillingOutClaim.draftClaim.declarantType match {
                            case Some(declarantType) =>
                              declarantType match {
                                case DeclarantType.Importer =>
                                  Redirect(routes.SelectReasonForBasisAndClaimController.selectReasonForClaimAndBasis())
                                case _                      => Redirect(routes.SelectBasisForClaimController.selectBasisForClaim())
                              }
                            case None                => Redirect(routes.SelectWhoIsMakingTheClaimController.selectDeclarantType())
                          }
                        case MovementReferenceNumber(Right(_)) =>
                          featureSwitch.NorthernIreland.isEnabled() match {
                            case true  => Redirect(routes.ClaimNorthernIrelandController.selectNorthernIrelandClaim())
                            case false => Redirect(routes.SelectBasisForClaimController.selectBasisForClaim())
                          }

                      }
                    case None                  =>
                      Redirect(routes.EnterMovementReferenceNumberController.enterJourneyMrn(JourneyBindable.Single))
                  }
              )
            }
          )
      }
    }

  def changeContactDetails: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withContactDetailsAnswers { (_, _, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.contactDetailsFormData match {
              case Some(contactDetailsFormData) =>
                Ok(
                  enterYourContactDetailsPage(
                    EnterYourContactDetailsController.contactDetailsForm.fill(
                      contactDetailsFormData
                    ),
                    isAmend = true
                  )
                )
              case None                         =>
                Ok(
                  enterYourContactDetailsPage(
                    EnterYourContactDetailsController.contactDetailsForm,
                    isAmend = true
                  )
                )
            },
          ifComplete =>
            Ok(
              enterYourContactDetailsPage(
                EnterYourContactDetailsController.contactDetailsForm.fill(
                  ifComplete.contactDetailsFormData
                ),
                isAmend = true
              )
            )
        )
      }
    }

  def changeContactDetailsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withContactDetailsAnswers { (_, fillingOutClaim, answers) =>
        EnterYourContactDetailsController.contactDetailsForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                enterYourContactDetailsPage(
                  requestFormWithErrors
                )
              ),
            contactDetailsFormData => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteContactDetailsAnswer(
                    contactDetailsFormData
                  ),
                complete => complete.copy(contactDetailsFormData = contactDetailsFormData)
              )
              val newDraftClaim  =
                fillingOutClaim.draftClaim.fold(_.copy(contactDetailsAnswer = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not capture contact details", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.CheckYourAnswersAndSubmitController.checkAllAnswersSubmit())
              )
            }
          )
      }
    }
}

object EnterYourContactDetailsController {

  final case class ContactDetailsFormData(
    companyName: String,
    emailAddress: Email,
    phoneNumber: PhoneNumber,
    contactAddress: NonUkAddress
  )

  object ContactDetailsFormData {
    implicit val format: OFormat[ContactDetailsFormData] = Json.format[ContactDetailsFormData]
  }

  val contactDetailsForm: Form[ContactDetailsFormData] = Form(
    mapping(
      "enter-your-contact-details.contact-name"         -> nonEmptyText(maxLength = 512),
      "enter-your-contact-details.contact-email"        -> Email.mappingMaxLength,
      "enter-your-contact-details.contact-phone-number" -> PhoneNumber.mapping,
      ""                                                -> Address.nonUkAddressFormMapping
    )(ContactDetailsFormData.apply)(ContactDetailsFormData.unapply)
  )

  def toContactDetailsFormData(
    contactDetails: Option[ContactDetails],
    verifiedEmail: Email
  ): ContactDetailsFormData =
    ContactDetailsFormData(
      contactDetails.flatMap(_.contactName).getOrElse(""),
      verifiedEmail,
      PhoneNumber(contactDetails.flatMap(_.telephone).getOrElse("")),
      NonUkAddress(
        contactDetails.flatMap(_.addressLine1).getOrElse(""),
        contactDetails.flatMap(_.addressLine2),
        None,
        contactDetails.flatMap(_.addressLine3).getOrElse(""),
        contactDetails.flatMap(_.postalCode).getOrElse(""),
        contactDetails.flatMap(_.countryCode).map(Country(_)).getOrElse(Country.uk)
      )
    )

}
