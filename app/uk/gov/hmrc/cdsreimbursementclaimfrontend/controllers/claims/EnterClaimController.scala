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
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{bigDecimal, mapping}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimController.ClaimAmount
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimsAnswer.{CompleteClaimsAnswer, IncompleteClaimsAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.form.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{Claim, ClaimsAnswer, DraftClaim, DutiesSelectedAnswer, Error, SessionData, TaxCode, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EnterClaimController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val config: Configuration,
  uuidGenerator: UUIDGenerator,
  enterClaimPage: pages.enter_claim,
  checkClaimPage: pages.check_claim
)(implicit ec: ExecutionContext, viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def withClaimsAnswer(
    f: (
      SessionData,
      FillingOutClaim,
      ClaimsAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeClaimsAnswer = draftClaim.fold(
          _.claimsAnswer
        )
        maybeClaimsAnswer.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteClaimsAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  private def makeClaims(duties: List[Duty], ndrcDetails: List[NdrcDetails]): List[Claim] = {
    val taxCodesSelected                       = duties.map(s => s.taxCode.toString).toSet
    val completeDutyDetails: List[NdrcDetails] = ndrcDetails.filter(p => taxCodesSelected.contains(p.taxType))
    completeDutyDetails.map { details =>
      Claim(
        uuidGenerator.nextId(),
        details.paymentMethod,
        details.paymentReference,
        details.taxType,
        BigDecimal(details.amount),
        BigDecimal(0.0),
        isFilled = false
      )
    }
  }

  private def makeOneClaim(duty: Duty, ndrcDetails: List[NdrcDetails]): Claim = {
    val maybeNdrcDetails: Option[NdrcDetails] = ndrcDetails.find(p => p.taxType === duty.taxCode.toString)

    maybeNdrcDetails match {
      case Some(details) =>
        Claim(
          uuidGenerator.nextId(),
          details.paymentMethod,
          details.paymentReference,
          details.taxType,
          BigDecimal(details.amount),
          BigDecimal(0.0),
          isFilled = false
        )
      case None          => sys.error("Could not make single initial claim")
    }

  }

  //TODO: need to work: if # duties select now does not match the number of claims then work if the duties is greater than #claims we need to augment the claims with one more claims
  // if the # duties is now less than the number of claims, filter out the one that does not match the tax codes and remove that and update session data structures
  def startClaim(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withClaimsAnswer { (_, fillingOutClaim, answers) =>
        answers.fold(
          ifIncomplete =>
            if (ifIncomplete.claims.nonEmpty) {
              val maybeDutiesSelectedAnswer: Option[DutiesSelectedAnswer] =
                fillingOutClaim.draftClaim.fold(_.dutiesSelectedAnswer)
              val numberOfClaims                                          = ifIncomplete.claims.size
              maybeDutiesSelectedAnswer match {
                case Some(dutiesSelectedAnswer) =>
                  dutiesSelectedAnswer match {
                    case DutiesSelectedAnswer.IncompleteDutiesSelectedAnswer(maybeDutiesSelected) =>
                      maybeDutiesSelected match {
                        case Some(dutiesSelected) =>
                          if (dutiesSelected.duties.size === numberOfClaims) {
                            Redirect(routes.EnterClaimController.checkClaim())
                          } else if (dutiesSelected.duties.size <= numberOfClaims) {
                            // we need to remove that claim
                            val taxCodes       = dutiesSelected.duties.map(s => s.taxCode.toString)
                            val updatedClaims  = ifIncomplete.claims.filterNot(p => taxCodes.contains(p.taxCode))
                            //update session and work which is the next one to fill in
                            val updatedAnswers = IncompleteClaimsAnswer(updatedClaims)

                            val newDraftClaim =
                              fillingOutClaim.draftClaim.fold(_.copy(claimsAnswer = Some(updatedAnswers)))

                            val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                            val result = EitherT
                              .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                              .leftMap((_: Unit) => Error("could not update session"))

                            result.fold(
                              e => {
                                logger.warn("could not update claims", e)
                                errorHandler.errorResult()
                              },
                              _ => Redirect(routes.EnterClaimController.checkClaim())
                            )
                          } else {
                            // number of duties is greater than the number of claims
                            // augment claims structure, update session, and redirect to that one
                            val newTaxCodes             = dutiesSelected.duties.map(s => s.taxCode.toString).toSet
                            val currentTaxCodes         = ifIncomplete.claims.map(s => s.taxCode).toSet
                            val newTaxCode: Set[String] = newTaxCodes.diff(currentTaxCodes)
                            if (newTaxCode.isEmpty) {
                              logger.warn("Should have picked up the new tax code to claim against")
                              errorHandler.errorResult()
                            } else {
                              val maybeNdrcDetails: Option[List[NdrcDetails]] = fillingOutClaim.draftClaim
                                .fold(_.displayDeclaration)
                                .flatMap(declaration => declaration.displayResponseDetail.ndrcDetails)

                              val maybeNewTaxCode = newTaxCode.headOption.flatMap(s => TaxCode.fromString(s))

                              val newClaim = (maybeNewTaxCode, maybeNdrcDetails) match {
                                case (Some(taxCode), Some(ndrc)) => makeOneClaim(Duty(taxCode), ndrc)
                                case _                           => sys.error("could not create new claim")
                              }

                              val updatedClaims = ifIncomplete.claims :+ newClaim

                              val updatedAnswers = IncompleteClaimsAnswer(updatedClaims)

                              val newDraftClaim =
                                fillingOutClaim.draftClaim.fold(_.copy(claimsAnswer = Some(updatedAnswers)))

                              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                              val result = EitherT
                                .liftF(
                                  updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney)))
                                )
                                .leftMap((_: Unit) => Error("could not update session"))

                              result.fold(
                                e => {
                                  logger.warn("could not update claims", e)
                                  errorHandler.errorResult()
                                },
                                _ => Redirect(routes.EnterClaimController.enterClaim(newClaim.id))
                              )
                            }
                          }
                        case None                 => Redirect(routes.SelectDutiesController.selectDuties())
                      }
                    case DutiesSelectedAnswer.CompleteDutiesSelectedAnswer(dutiesSelected)        =>
                      if (dutiesSelected.duties.size === numberOfClaims) {
                        Redirect(routes.EnterClaimController.checkClaim())
                      } else if (dutiesSelected.duties.size <= numberOfClaims) {
                        // we need to remove that claim
                        val taxCodes       = dutiesSelected.duties.map(s => s.taxCode.toString)
                        val updatedClaims  = ifIncomplete.claims.filterNot(p => taxCodes.contains(p.taxCode))
                        //update session and work which is the next one to fill in
                        val updatedAnswers = IncompleteClaimsAnswer(updatedClaims)

                        val newDraftClaim =
                          fillingOutClaim.draftClaim.fold(_.copy(claimsAnswer = Some(updatedAnswers)))

                        val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                        val result = EitherT
                          .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                          .leftMap((_: Unit) => Error("could not update session"))

                        result.fold(
                          e => {
                            logger.warn("could not update claims", e)
                            errorHandler.errorResult()
                          },
                          _ => Redirect(routes.EnterClaimController.checkClaim())
                        )
                      } else {
                        // number of duties is greater than the number of claims
                        // augment claims structure, update session, and redirect to that one
                        val newTaxCodes             = dutiesSelected.duties.map(s => s.taxCode.toString).toSet
                        val currentTaxCodes         = ifIncomplete.claims.map(s => s.taxCode).toSet
                        val newTaxCode: Set[String] = newTaxCodes.diff(currentTaxCodes)
                        if (newTaxCode.isEmpty) {
                          logger.warn("Should have picked up the new tax code to claim against")
                          errorHandler.errorResult()
                        } else {
                          val maybeNdrcDetails: Option[List[NdrcDetails]] = fillingOutClaim.draftClaim
                            .fold(_.displayDeclaration)
                            .flatMap(declaration => declaration.displayResponseDetail.ndrcDetails)

                          val maybeNewTaxCode = newTaxCode.headOption.flatMap(s => TaxCode.fromString(s))

                          val newClaim = (maybeNewTaxCode, maybeNdrcDetails) match {
                            case (Some(taxCode), Some(ndrc)) => makeOneClaim(Duty(taxCode), ndrc)
                            case _                           => sys.error("could not create new claim")
                          }

                          val updatedClaims = ifIncomplete.claims :+ newClaim

                          val updatedAnswers = IncompleteClaimsAnswer(updatedClaims)

                          val newDraftClaim =
                            fillingOutClaim.draftClaim.fold(_.copy(claimsAnswer = Some(updatedAnswers)))

                          val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                          val result = EitherT
                            .liftF(
                              updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney)))
                            )
                            .leftMap((_: Unit) => Error("could not update session"))

                          result.fold(
                            e => {
                              logger.warn("could not update claims", e)
                              errorHandler.errorResult()
                            },
                            _ => Redirect(routes.EnterClaimController.enterClaim(newClaim.id))
                          )
                        }
                      }
                  }
                case None                       => Redirect(routes.SelectDutiesController.selectDuties())
              }
            } else {
              val maybeNdrcDetails: Option[List[NdrcDetails]] = fillingOutClaim.draftClaim
                .fold(_.displayDeclaration)
                .flatMap(declaration => declaration.displayResponseDetail.ndrcDetails)

              val claims: List[Claim] =
                (fillingOutClaim.draftClaim.fold(_.dutiesSelectedAnswer), maybeNdrcDetails) match {
                  case (Some(selectedDuties), Some(ndrcDetails)) =>
                    println(s"Select duties are: ${selectedDuties.duties.toString}")
                    println(s"NDRC are: ${ndrcDetails.toString()}")
                    makeClaims(selectedDuties.duties, ndrcDetails)
                  case _                                         => List.empty
                }

              if (claims.nonEmpty) {
                val updatedAnswers = answers.fold(
                  _ => IncompleteClaimsAnswer(claims),
                  complete => complete.copy(claims = claims)
                )

                val newDraftClaim =
                  fillingOutClaim.draftClaim.fold(_.copy(claimsAnswer = Some(updatedAnswers)))

                val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                val result = EitherT
                  .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                  .leftMap((_: Unit) => Error("could not update session"))

                result.fold(
                  e => {
                    logger.warn("could not start claims", e)
                    errorHandler.errorResult()
                  },
                  _ =>
                    claims.headOption.fold {
                      logger.warn("Could not determine whether there are any claims")
                      errorHandler.errorResult()
                    }(claim => Redirect(routes.EnterClaimController.enterClaim(claim.id)))
                )
              } else {
                logger.warn("Could not determine whether eligible for claims")
                Redirect(baseRoutes.IneligibleController.ineligible())
              }
            },
          _ => Redirect(routes.EnterClaimController.checkClaim())
        )
      }
    }

  def enterClaim(id: UUID): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withClaimsAnswer { (_, _, answers) =>
        answers.fold(
          ifIncomplete => {
            val maybeClaim: Option[Claim] = ifIncomplete.claims.find(p => p.id === id)
            maybeClaim.fold {
              logger.warn("Could not find claim")
              errorHandler.errorResult()
            }(claim =>
              if (claim.claimAmount === BigDecimal(0.0)) {
                Ok(enterClaimPage(id, EnterClaimController.claimAmountForm, claim))
              } else {
                Ok(enterClaimPage(id, EnterClaimController.claimAmountForm.fill(ClaimAmount(claim.claimAmount)), claim))
              }
            )
          },
          ifComplete => {
            val maybeClaim: Option[Claim] = ifComplete.claims.find(p => p.id === id)
            maybeClaim.fold {
              logger.warn("Could not find claim")
              errorHandler.errorResult()
            }(claim =>
              Ok(enterClaimPage(id, EnterClaimController.claimAmountForm.fill(ClaimAmount(claim.claimAmount)), claim))
            )
          }
        )
      }
    }

  def enterClaimSubmit(id: UUID): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withClaimsAnswer { (_, fillingOutClaim, answers) =>
        EnterClaimController.claimAmountForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors => {
              println(s"errors are :${requestFormWithErrors.toString}")
              val claim = answers
                .fold(ifComplete => ifComplete.claims, ifIncomplete => ifIncomplete.claims)
                .find(p => p.id === id)
              claim.fold {
                logger.warn("Lost claim!")
                errorHandler.errorResult()
              }(claim => BadRequest(enterClaimPage(id, requestFormWithErrors, claim)))
            },
            claimAmount => {

              def allProcessed(claims: List[Claim]): Boolean = claims.forall(p => p.isFilled)

              val claims = answers.fold(_.claims, _.claims)

              val maybeClaim: Option[Claim] = claims.find(p => p.id === id)

              val claim = maybeClaim match {
                case Some(value) => value
                case None        => sys.error("Could not find claim")
              }

              val updatedClaims =
                claims.filterNot(p => p.id === id) :+ claim.copy(isFilled = true, claimAmount = claimAmount.amount)

              if (allProcessed(updatedClaims)) {
                val updatedAnswers = answers.fold(
                  _ =>
                    IncompleteClaimsAnswer(
                      updatedClaims
                    ),
                  complete => complete.copy(claims = updatedClaims)
                )

                val newDraftClaim =
                  fillingOutClaim.draftClaim.fold(_.copy(claimsAnswer = Some(updatedAnswers)))

                val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                val result = EitherT
                  .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                  .leftMap((_: Unit) => Error("could not update session"))

                result.fold(
                  e => {
                    logger.warn("could not process all claims", e)
                    errorHandler.errorResult()
                  },
                  _ => Redirect(routes.EnterClaimController.checkClaim())
                )
              } else {
                println("all claims not processed")
                val updatedAnswers = answers.fold(
                  _ =>
                    IncompleteClaimsAnswer(
                      updatedClaims
                    ),
                  complete => complete.copy(claims = updatedClaims)
                )

                val newDraftClaim =
                  fillingOutClaim.draftClaim.fold(_.copy(claimsAnswer = Some(updatedAnswers)))

                val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                val result = EitherT
                  .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                  .leftMap((_: Unit) => Error("could not update session"))

                result.fold(
                  e => {
                    logger.warn("could not process claim", e)
                    errorHandler.errorResult()
                  },
                  _ => {
                    val notProcessClaims = updatedClaims.filter(p => p.isFilled === false)
                    println(s"claims not processed are :${notProcessClaims.toString()}")
                    notProcessClaims.headOption.fold {
                      logger.warn("Could not determine whether there are any claims 8")
                      errorHandler.errorResult()
                    }(claim => Redirect(routes.EnterClaimController.enterClaim(claim.id)))
                  }
                )
              }

            }
          )
      }
    }

  def checkClaim(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withClaimsAnswer { (_, _, answers) =>
        answers.fold(
          ifIncomplete => {
            val notProcessClaims = ifIncomplete.claims.filter(p => p.isFilled === false)
            if (notProcessClaims.nonEmpty) {
              notProcessClaims.headOption.fold {
                logger.warn("Could not determine whether there are any claims")
                errorHandler.errorResult()
              }(claim => Redirect(routes.EnterClaimController.enterClaim(claim.id)))
            } else {
              Ok(checkClaimPage(ifIncomplete.claims))
            }
          },
          ifComplete => Ok(checkClaimPage(ifComplete.claims))
        )
      }
    }

  def checkClaimSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withClaimsAnswer { (_, fillingOutClaim, answers) =>
        val completeClaimsAnswer = CompleteClaimsAnswer(answers.fold(_.claims, _.claims))

        val newDraftClaim =
          fillingOutClaim.draftClaim.fold(_.copy(claimsAnswer = Some(completeClaimsAnswer)))

        val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

        val result = EitherT
          .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
          .leftMap((_: Unit) => Error("could not update session"))

        result.fold(
          e => {
            logger.warn("could not complete claim state", e)
            errorHandler.errorResult()
          },
          _ =>
            fillingOutClaim.draftClaim
              .fold(_.movementReferenceNumber)
              .fold {
                logger.warn("could not find movement reference number")
                errorHandler.errorResult()
              }(referenceNumber =>
                referenceNumber.fold(
                  _ => Redirect(routes.BankAccountController.enterBankAccountDetails()),
                  _ => Redirect(routes.BankAccountController.checkBankAccountDetails())
                )
              )
        )

      }
    }

}

object EnterClaimController {

  final case class ClaimAmount(amount: BigDecimal)

  val claimAmountForm: Form[ClaimAmount] = Form(
    mapping(
      "enter-claim" -> bigDecimal
    )(ClaimAmount.apply)(ClaimAmount.unapply)
  )

}
