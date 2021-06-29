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
import cats.syntax.all._
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterDuplicateMovementReferenceNumberController.{enterDuplicateEntryNumberKey, enterDuplicateMovementReferenceNumberKey, enterNoLegacyDuplicateMrnKey}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterMovementReferenceNumberController.movementReferenceNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionDataExtractor, SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.MrnJourney.MrnImporter
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.{ClaimService, FeatureSwitchService}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterDuplicateMovementReferenceNumberController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  claimService: ClaimService,
  featureSwitch: FeatureSwitchService,
  enterDuplicateMovementReferenceNumberPage: pages.enter_duplicate_movement_reference_number,
  enterNoLegacyDuplicateMrn: pages.enter_no_legacy_duplicate_mrn
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, cc: MessagesControllerComponents, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionDataExtractor
    with SessionUpdates
    with Logging {

  def enterDuplicateMrn(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDuplicateMovementReferenceNumberAnswer { (_, fillingOutClaim, maybeAnswers) =>
        val isMrnJourney = fillingOutClaim.draftClaim.isMrnFlow
        val messagesKey  = resolveMessagesKey(featureSwitch, isMrnJourney)
        val isErnEnabled = featureSwitch.EntryNumber.isEnabled()
        val form         = maybeAnswers.foldLeft(movementReferenceNumberForm(messagesKey, isErnEnabled))((mrnForm, mrn) =>
          mrnForm.fill(mrn)
        )

        Ok(resolveEnterDuplicateMrnPageFor(featureSwitch, isMrnJourney)(form))
      }
    }

  def enterDuplicateMrnSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withDuplicateMovementReferenceNumberAnswer { (_, fillingOutClaim, _) =>
        val isErnEnabled = featureSwitch.EntryNumber.isEnabled()
        val isMrnJourney = fillingOutClaim.draftClaim.isMrnFlow
        val messagesKey  = resolveMessagesKey(featureSwitch, isMrnJourney)
        def mrnForm      = movementReferenceNumberForm(messagesKey, isErnEnabled)

        mrnForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                resolveEnterDuplicateMrnPageFor(featureSwitch, isMrnJourney)(
                  requestFormWithErrors.copy(errors =
                    Seq(
                      EnterMovementReferenceNumberController
                        .processFormErrors(messagesKey, requestFormWithErrors.errors)
                    )
                  )
                )
              ),
            mrnOrEntryNumber => {

              def renderPageWithError(errorKey: String): Result =
                BadRequest(
                  resolveEnterDuplicateMrnPageFor(featureSwitch, isMrnJourney)(
                    mrnForm
                      .fill(mrnOrEntryNumber)
                      .withError(messagesKey, s"invalid.$errorKey")
                  )
                )

              mrnOrEntryNumber.value match {

                case Left(_) if !featureSwitch.EntryNumber.isEnabled() =>
                  renderPageWithError("mrn-not-entry-number")

                case Left(entryNumber) if featureSwitch.EntryNumber.isEnabled() =>
                  fillingOutClaim.draftClaim.movementReferenceNumber
                    .map(
                      _.bimap(
                        Option(_)
                          .filter(_.value === entryNumber.value)
                          .map(_ => "enter-different-entry-number"),
                        _ => "mrn-not-entry-number".some
                      ).merge
                    )
                    .map(maybeError =>
                      for {
                        updatedJourney <- EitherT
                                            .fromOption[Future](
                                              maybeError.map(renderPageWithError),
                                              FillingOutClaim.of(fillingOutClaim)(
                                                _.copy(
                                                  duplicateMovementReferenceNumberAnswer =
                                                    MovementReferenceNumber(entryNumber).some
                                                )
                                              )
                                            )
                                            .swap
                        result         <-
                          EitherT
                            .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = updatedJourney.some)))
                            .map { _ =>
                              Redirect(routes.EnterDeclarationDetailsController.enterDuplicateDeclarationDetails())
                            }
                            .leftMap { (_: Unit) =>
                              logger.warn("could not update session")
                              errorHandler.errorResult()
                            }
                      } yield result
                    )
                    .map(_.merge)
                    .getOrElse {
                      logger.warn("Invalid state: original Entry Number is missing")
                      errorHandler.errorResult()
                    }

                case Right(mrn) =>
                  fillingOutClaim.draftClaim.movementReferenceNumber
                    .map(
                      _.bimap(
                        _ => "entry-number-not-mrn".some,
                        Option(_)
                          .filter(_.value === mrn.value)
                          .map(_ => "enter-different-mrn")
                      ).merge
                    )
                    .map(maybeError =>
                      for {
                        updatedJourney   <- EitherT
                                              .fromOption[Future](
                                                maybeError.map(renderPageWithError),
                                                FillingOutClaim.of(fillingOutClaim)(
                                                  _.copy(
                                                    duplicateMovementReferenceNumberAnswer =
                                                      MovementReferenceNumber(mrn).some
                                                  )
                                                )
                                              )
                                              .swap
                        _                <- EitherT
                                              .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = updatedJourney.some)))
                                              .leftMap { (_: Unit) =>
                                                logger.warn("could not update session")
                                                Redirect(baseRoutes.IneligibleController.ineligible())
                                              }
                        maybeDeclaration <- claimService
                                              .getDisplayDeclaration(mrn)
                                              .leftMap { _ =>
                                                logger.warn("could not get duplicate declaration")
                                                Redirect(baseRoutes.IneligibleController.ineligible())
                                              }
                        mrnJourneyFlow   <- EitherT
                                              .fromEither[Future](
                                                EnterMovementReferenceNumberController.evaluateMrnJourneyFlow(
                                                  fillingOutClaim.signedInUserDetails,
                                                  maybeDeclaration
                                                )
                                              )
                                              .leftMap { _ =>
                                                logger.warn("could not evaluate MRN flow")
                                                Redirect(baseRoutes.IneligibleController.ineligible())
                                              }
                        declaration      <- EitherT.fromOption[Future](
                                              maybeDeclaration, {
                                                logger.warn("could not unbox display declaration")
                                                Redirect(baseRoutes.IneligibleController.ineligible())
                                              }
                                            )
                        _                <- EitherT
                                              .liftF(
                                                updateSession(sessionStore, request)(
                                                  _.copy(journeyStatus =
                                                    FillingOutClaim
                                                      .of(updatedJourney)(_.copy(duplicateDisplayDeclaration = declaration.some))
                                                      .some
                                                  )
                                                )
                                              )
                                              .leftMap { (_: Unit) =>
                                                logger.warn("could not unbox display declaration")
                                                Redirect(baseRoutes.IneligibleController.ineligible())
                                              }
                        page              = mrnJourneyFlow match {
                                              case _: MrnImporter =>
                                                Redirect(routes.CheckDeclarationDetailsController.checkDuplicateDetails())
                                              case _              => Redirect(routes.EnterImporterEoriNumberController.enterImporterEoriNumber())
                                            }
                      } yield page
                    )
                    .map(_.merge)
                    .getOrElse {
                      logger.warn("Invalid state: original MRN is missing")
                      errorHandler.errorResult()
                    }
              }
            }
          )
      }
    }

  private def withDuplicateMovementReferenceNumberAnswer(
    f: (
      SessionData,
      FillingOutClaim,
      Option[MovementReferenceNumber]
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.unapply({ case (sessionData, fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)) =>
      val maybeDuplicateMovementReferenceNumberAnswer = draftClaim.fold(_.duplicateMovementReferenceNumberAnswer)
      f(sessionData, fillingOutClaim, maybeDuplicateMovementReferenceNumberAnswer)
    })

  private def resolveEnterDuplicateMrnPageFor(
    feature: FeatureSwitchService,
    isMrnJourney: Boolean
  )(form: Form[MovementReferenceNumber])(implicit request: RequestWithSessionData[AnyContent]): HtmlFormat.Appendable =
    if (feature.EntryNumber.isEnabled()) enterDuplicateMovementReferenceNumberPage(form, isMrnJourney)
    else enterNoLegacyDuplicateMrn(form)

  private def resolveMessagesKey(
    feature: FeatureSwitchService,
    isMrnJourney: Boolean
  ): String =
    if (feature.EntryNumber.isEnabled()) {
      if (isMrnJourney) {
        enterDuplicateMovementReferenceNumberKey
      } else enterDuplicateEntryNumberKey
    } else enterNoLegacyDuplicateMrnKey

}

object EnterDuplicateMovementReferenceNumberController {

  val enterNoLegacyDuplicateMrnKey             = "enter-no-legacy-duplicate-mrn"
  val enterDuplicateMovementReferenceNumberKey = "enter-duplicate-movement-reference-number"
  val enterDuplicateEntryNumberKey             = "enter-duplicate-entry-number"
}
