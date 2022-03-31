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
import cats.implicits.catsSyntaxEq
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.Configuration
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.extractRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyExtractor.withAnswersAndRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.ReimbursementRoutes.ReimbursementRoutes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.enterBankDetailsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.{routes => claimRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.fileupload.{routes => fileUploadRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyBindable
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.bankaccountreputation.response.ReputationResponse
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BankAccountType
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.ClaimService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging.LoggerOps
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.http.BadGatewayException
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class BankAccountController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration,
  val claimService: ClaimService,
  checkBankAccountDetailsPage: pages.check_bank_account_details,
  enterBankAccountDetailsPage: pages.enter_bank_account_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  implicit val dataExtractor: DraftClaim => Option[BankAccountDetails] = _.bankAccountDetailsAnswer

  def checkBankAccountDetails(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case fillingOutClaim: FillingOutClaim =>
        implicit val router: ReimbursementRoutes = extractRoutes(fillingOutClaim.draftClaim, journey)
        implicit val subKey: Option[String]      = router.subKey
        import router._

        fillingOutClaim.draftClaim.findNonEmptyBankAccountDetails
          .map { bankAccountDetails =>
            Future.successful(
              Ok(
                checkBankAccountDetailsPage(
                  bankAccountDetails,
                  CheckAnswers.when(fillingOutClaim.draftClaim.isComplete)(alternatively =
                    fileUploadRoutes.SupportingEvidenceController.uploadSupportingEvidence(journey)
                  ),
                  routes.SelectBankAccountTypeController.selectBankAccountType(journey)
                )
              )
            )
          }
          .getOrElse {
            Future.successful(Redirect(claimRoutes.SelectBankAccountTypeController.selectBankAccountType(journey)))
          }
      }
    }

  def enterBankAccountDetails(implicit journey: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[BankAccountDetails] { (_, answers, router) =>
        val bankDetailsForm =
          answers.toList.foldLeft(enterBankDetailsForm)((form, answer) => form.fill(answer))
        Ok(enterBankAccountDetailsPage(bankDetailsForm, router.submitUrlForEnterBankAccountDetails()))
      }
    }

  def enterBankAccountDetailsSubmit(implicit journeyBindable: JourneyBindable): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswersAndRoutes[BankAccountDetails] { (fillingOutClaim, _, router) =>
        fillingOutClaim.draftClaim.bankAccountTypeAnswer match {
          case None =>
            Redirect(claimRoutes.SelectBankAccountTypeController.selectBankAccountType(journeyBindable))

          case Some(bankAccount: BankAccountType) =>
            enterBankDetailsForm
              .bindFromRequest()
              .fold(
                requestFormWithErrors =>
                  BadRequest(
                    enterBankAccountDetailsPage(requestFormWithErrors, router.submitUrlForEnterBankAccountDetails())
                  ),
                bankAccountDetails => {
                  val updatedJourney =
                    FillingOutClaim.from(fillingOutClaim)(_.copy(bankAccountDetailsAnswer = Some(bankAccountDetails)))

                  (for {
                    _                  <- EitherT
                                            .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                                            .leftMap((_: Unit) => Error("could not update session"))
                    reputationResponse <- {
                      if (bankAccount === BankAccountType.Business) {
                        claimService.getBusinessAccountReputation(bankAccountDetails)
                      } else {
                        val postCode = fillingOutClaim.draftClaim
                          .extractEstablishmentAddress(fillingOutClaim.signedInUserDetails)
                          .flatMap(_.postalCode)
                        claimService.getPersonalAccountReputation(bankAccountDetails, postCode)
                      }
                    }
                  } yield reputationResponse).fold(
                    {
                      case e @ Error(_, Some(_: BadGatewayException), _) =>
                        logger warn ("could not contact bank account service: ", e)
                        Redirect(routes.ServiceUnavailableController.unavailable(journeyBindable))
                      case e                                             =>
                        logAndDisplayError("could not process bank account details: ")(errorHandler, request)(e)
                    },
                    reputationResponse =>
                      if (reputationResponse.otherError.isDefined) {
                        val errorKey = reputationResponse.otherError.map(_.code).getOrElse("account-does-not-exist")
                        val form     = enterBankDetailsForm
                          .fill(bankAccountDetails)
                          .withError("enter-bank-details", s"error.$errorKey")
                        BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
                      } else if (reputationResponse.accountNumberWithSortCodeIsValid === ReputationResponse.No) {
                        val form = enterBankDetailsForm
                          .withError("enter-bank-details", "error.moc-check-no")
                        BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
                      } else if (reputationResponse.accountNumberWithSortCodeIsValid =!= ReputationResponse.Yes) {
                        val form = enterBankDetailsForm
                          .withError("enter-bank-details", "error.moc-check-failed")
                        BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
                      } else if (reputationResponse.accountExists === Some(ReputationResponse.Error)) {
                        val form = enterBankDetailsForm
                          .fill(bankAccountDetails)
                          .withError("enter-bank-details", s"error.account-exists-error")
                        BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
                      } else if (reputationResponse.accountExists =!= Some(ReputationResponse.Yes)) {
                        val form = enterBankDetailsForm
                          .withError("enter-bank-details", s"error.account-does-not-exist")
                        BadRequest(enterBankAccountDetailsPage(form, router.submitUrlForEnterBankAccountDetails()))
                      } else {
                        Redirect(claimRoutes.BankAccountController.checkBankAccountDetails(journeyBindable))
                      }
                  )
                }
              )
        }
      }
    }
}
