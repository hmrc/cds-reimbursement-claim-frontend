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
import cats.implicits.{catsSyntaxEq, _}
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Forms.{bigDecimal, mapping}
import play.api.data.{Form, FormError}
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.EnterClaimController.{AmountPair, ClaimAmount}
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
  enterEntryClaimPage: pages.enter_entry_claim,
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

  private def makeEntryClaims(taxCodes: List[String]): List[Claim] =
    taxCodes.map { details =>
      Claim(
        uuidGenerator.nextId(),
        "001",
        "Unknown",
        details,
        BigDecimal(0.0),
        BigDecimal(0.0),
        isFilled = false
      )
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

  private def makeNewSelectedClaims(duties: List[Duty], ndrcDetails: List[NdrcDetails]): List[Claim] =
    duties.map { duty =>
      Claim(
        uuidGenerator.nextId(),
        ndrcDetails.find(n => n.taxType === duty.taxCode.toString).map(s => s.paymentMethod).getOrElse(""),
        ndrcDetails.find(n => n.taxType === duty.toString).map(s => s.paymentReference).getOrElse(""),
        duty.taxCode.toString,
        ndrcDetails.find(n => n.taxType === duty.toString).map(s => BigDecimal(s.amount)).getOrElse(BigDecimal(0.0)),
        BigDecimal(0.0),
        isFilled = false
      )
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

                            val taxCodes       = dutiesSelected.duties.map(s => s.taxCode.toString)
                            val updatedClaims  = ifIncomplete.claims.filterNot(p => taxCodes.contains(p.taxCode))
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
                            val newTaxCodes: Set[String]     = dutiesSelected.duties.map(s => s.taxCode.toString).toSet
                            val currentTaxCodes: Set[String] = ifIncomplete.claims.map(s => s.taxCode).toSet
                            val newTaxCode: Set[String]      = newTaxCodes.diff(currentTaxCodes)
                            if (newTaxCode.isEmpty) {
                              logger.warn("Should have picked up the new tax code to claim against")
                              errorHandler.errorResult()
                            } else {

                              val newClaims: List[Claim] = fillingOutClaim.draftClaim.movementReferenceNumber match {
                                case Some(value) =>
                                  value match {
                                    case Left(_)  =>
                                      makeEntryClaims(newTaxCode.toList)
                                    case Right(_) =>
                                      val maybeNdrcDetails: Option[List[NdrcDetails]] = fillingOutClaim.draftClaim
                                        .fold(_.displayDeclaration)
                                        .flatMap(declaration => declaration.displayResponseDetail.ndrcDetails)

                                      val maybeNewDutiesToClaim =
                                        newTaxCode.map(s => TaxCode.fromString(s)).toList.sequence
                                      val newDutiesToClaim      = maybeNewDutiesToClaim match {
                                        case Some(value) => value.map(s => Duty(s))
                                        case None        => List.empty
                                      }

                                      maybeNdrcDetails match {
                                        case (Some(ndrc)) => makeNewSelectedClaims(newDutiesToClaim, ndrc)
                                        case _            => sys.error("could not create new claim")
                                      }
                                  }
                                case None        => List.empty ///TODO: change to error?
                              }

                              val updatedClaims = ifIncomplete.claims ::: newClaims

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
                                _ =>
                                  newClaims.headOption.fold {
                                    logger.warn("Could not add new claims")
                                    errorHandler.errorResult()
                                  }(newClaim => Redirect(routes.EnterClaimController.enterClaim(newClaim.id)))
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
                        val newTaxCodes: Set[String]     = dutiesSelected.duties.map(s => s.taxCode.toString).toSet
                        val currentTaxCodes: Set[String] = ifIncomplete.claims.map(s => s.taxCode).toSet
                        val newTaxCode: Set[String]      = newTaxCodes.diff(currentTaxCodes)
                        if (newTaxCode.isEmpty) {
                          logger.warn("Should have picked up the new tax code to claim against")
                          errorHandler.errorResult()
                        } else {

                          val newClaims: List[Claim] = fillingOutClaim.draftClaim.movementReferenceNumber match {
                            case Some(value) =>
                              value match {
                                case Left(_)  =>
                                  makeEntryClaims(newTaxCode.toList)
                                case Right(_) =>
                                  val maybeNdrcDetails: Option[List[NdrcDetails]] = fillingOutClaim.draftClaim
                                    .fold(_.displayDeclaration)
                                    .flatMap(declaration => declaration.displayResponseDetail.ndrcDetails)

                                  val maybeNewDutiesToClaim =
                                    newTaxCode.map(s => TaxCode.fromString(s)).toList.sequence
                                  val newDutiesToClaim      = maybeNewDutiesToClaim match {
                                    case Some(value) => value.map(s => Duty(s))
                                    case None        => List.empty
                                  }

                                  maybeNdrcDetails match {
                                    case (Some(ndrc)) => makeNewSelectedClaims(newDutiesToClaim, ndrc)
                                    case _            => sys.error("could not create new claim")
                                  }
                              }
                            case None        => List.empty ///TODO: change to error?
                          }

                          val updatedClaims = ifIncomplete.claims ::: newClaims

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
                            _ =>
                              newClaims.headOption.fold {
                                logger.warn("Could not add new claims")
                                errorHandler.errorResult()
                              }(newClaim => Redirect(routes.EnterClaimController.enterClaim(newClaim.id)))
                          )
                        }
                      }
                  }
                case None                       => Redirect(routes.SelectDutiesController.selectDuties())
              }
            } else {

              val claims = fillingOutClaim.draftClaim.movementReferenceNumber match {
                case Some(value) =>
                  value match {
                    case Left(_) =>
                      (fillingOutClaim.draftClaim.fold(_.dutiesSelectedAnswer)) match {
                        case Some(selectedDuties) =>
                          makeEntryClaims(selectedDuties.duties.map(s => s.taxCode.toString))
                        case _                    => List.empty
                      }

                    case Right(_) =>
                      val maybeNdrcDetails: Option[List[NdrcDetails]] = fillingOutClaim.draftClaim
                        .fold(_.displayDeclaration)
                        .flatMap(declaration => declaration.displayResponseDetail.ndrcDetails)

                      (fillingOutClaim.draftClaim.fold(_.dutiesSelectedAnswer), maybeNdrcDetails) match {
                        case (Some(selectedDuties), Some(ndrcDetails)) =>
                          makeClaims(selectedDuties.duties, ndrcDetails)
                        case _                                         => List.empty
                      }
                  }
                case None        => List.empty
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
      withClaimsAnswer { (_, fillingOutClaim, answers) =>
        answers.fold(
          ifIncomplete => {
            val maybeClaim: Option[Claim] = ifIncomplete.claims.find(p => p.id === id)
            maybeClaim.fold {
              logger.warn("Could not find claim")
              errorHandler.errorResult()
            }(claim =>
              if (claim.claimAmount === BigDecimal(0.0)) {
                fillingOutClaim.draftClaim.movementReferenceNumber match {
                  case Some(value) =>
                    value match {
                      case Left(_)  =>
                        Ok(enterEntryClaimPage(id, EnterClaimController.entryClaimAmountForm, claim))
                      case Right(_) =>
                        Ok(enterClaimPage(id, EnterClaimController.mrnClaimAmountForm(claim.paidAmount), claim))
                    }
                  case None        =>
                    logger.warn("Could not find movement or entry reference number")
                    errorHandler.errorResult()
                }
              } else {
                fillingOutClaim.draftClaim.movementReferenceNumber match {
                  case Some(value) =>
                    value match {
                      case Left(_)  =>
                        Ok(
                          enterEntryClaimPage(
                            id,
                            EnterClaimController.entryClaimAmountForm.fill(
                              AmountPair(claim.paidAmount, claim.claimAmount)
                            ),
                            claim
                          )
                        )
                      case Right(_) =>
                        Ok(
                          enterClaimPage(
                            id,
                            EnterClaimController
                              .mrnClaimAmountForm(claim.paidAmount)
                              .fill(ClaimAmount(claim.claimAmount)),
                            claim
                          )
                        )
                    }
                  case None        =>
                    logger.warn("Could not find movement or entry reference number")
                    errorHandler.errorResult()
                }
              }
            )
          },
          ifComplete => {
            val maybeClaim: Option[Claim] = ifComplete.claims.find(p => p.id === id)
            maybeClaim.fold {
              logger.warn("Could not find claim")
              errorHandler.errorResult()
            }(claim =>
              fillingOutClaim.draftClaim.movementReferenceNumber match {
                case Some(value) =>
                  value match {
                    case Left(_)  =>
                      Ok(
                        enterEntryClaimPage(
                          id,
                          EnterClaimController.entryClaimAmountForm.fill(
                            AmountPair(claim.paidAmount, claim.claimAmount)
                          ),
                          claim
                        )
                      )
                    case Right(_) =>
                      Ok(
                        enterClaimPage(
                          id,
                          EnterClaimController
                            .mrnClaimAmountForm(claim.paidAmount)
                            .fill(ClaimAmount(claim.claimAmount)),
                          claim
                        )
                      )
                  }
                case None        =>
                  logger.warn("Could not find movement or entry reference number")
                  errorHandler.errorResult()
              }
            )
          }
        )
      }
    }

  def enterClaimSubmit(id: UUID): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withClaimsAnswer { (_, fillingOutClaim, answers) =>
        fillingOutClaim.draftClaim.movementReferenceNumber match {
          case Some(value) =>
            value match {
              case Left(_)  =>
                EnterClaimController.entryClaimAmountForm
                  .bindFromRequest()
                  .fold(
                    requestFormWithErrors => {
                      val claim = answers
                        .fold(ifComplete => ifComplete.claims, ifIncomplete => ifIncomplete.claims)
                        .find(p => p.id === id)
                      claim.fold {
                        logger.warn("Lost claim!")
                        errorHandler.errorResult()
                      }(claim => BadRequest(enterEntryClaimPage(id, requestFormWithErrors, claim)))
                    },
                    amountPair => {

                      def allProcessed(claims: List[Claim]): Boolean = claims.forall(p => p.isFilled)

                      val claims = answers.fold(_.claims, _.claims)

                      val maybeClaim: Option[Claim] = claims.find(p => p.id === id)

                      val claim = maybeClaim match {
                        case Some(value) => value
                        case None        => sys.error("Could not find claim")
                      }

                      val updatedClaims =
                        claims.filterNot(p => p.id === id) :+ claim
                          .copy(
                            isFilled = true,
                            claimAmount = amountPair.claimAmount,
                            paidAmount = amountPair.paidAmount
                          )

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
                            notProcessClaims.headOption.fold {
                              logger.warn("Could not determine whether there are any claims 8")
                              errorHandler.errorResult()
                            }(claim => Redirect(routes.EnterClaimController.enterClaim(claim.id)))
                          }
                        )
                      }

                    }
                  )
              case Right(_) =>
                val claim = answers
                  .fold(ifComplete => ifComplete.claims, ifIncomplete => ifIncomplete.claims)
                  .find(p => p.id === id)
                claim.fold {
                  logger.warn("Lost claim!")
                  Future.successful(errorHandler.errorResult())
                } { claim =>
                  EnterClaimController
                    .mrnClaimAmountForm(claim.paidAmount)
                    .bindFromRequest()
                    .fold(
                      requestFormWithErrors => {
                        println(s"${requestFormWithErrors.toString}")
                        val updatedErrors: Seq[FormError] =
                          requestFormWithErrors.errors.map(d => d.copy(key = "enter-claim"))

                        val claim = answers
                          .fold(ifComplete => ifComplete.claims, ifIncomplete => ifIncomplete.claims)
                          .find(p => p.id === id)
                        claim.fold {
                          logger.warn("Lost claim!")
                          errorHandler.errorResult()
                        }(claim =>
                          BadRequest(enterClaimPage(id, requestFormWithErrors.copy(errors = updatedErrors), claim))
                        )
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
                          claims.filterNot(p => p.id === id) :+ claim
                            .copy(isFilled = true, claimAmount = claimAmount.amount)

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
          case None        =>
            logger.warn("Could not find movement or entry reference number")
            errorHandler.errorResult()
        }

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

  final case class AmountPair(
    paidAmount: BigDecimal,
    claimAmount: BigDecimal
  )

  final case class ClaimAmount(amount: BigDecimal)

  def mrnClaimAmountForm(paidAmount: BigDecimal): Form[ClaimAmount] =
    Form(
      mapping(
        "enter-claim" -> bigDecimal
      )(ClaimAmount.apply)(ClaimAmount.unapply)
        .verifying("invalid.claim", a => a.amount <= paidAmount)
    )

  val entryClaimAmountForm: Form[AmountPair] = Form(
    mapping(
      "enter-claim.paid-amount"  -> bigDecimal,
      "enter-claim.claim-amount" -> bigDecimal
    )(AmountPair.apply)(AmountPair.unapply)
      .verifying("enter-claim.invalid.claim", a => a.claimAmount <= a.paidAmount)
  )

}
