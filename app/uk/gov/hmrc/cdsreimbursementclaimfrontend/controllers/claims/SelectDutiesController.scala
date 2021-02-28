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
import play.api.data.Forms._
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectDutiesController.makeBlankForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{SessionUpdates, routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DutiesSelectedAnswer.{CompleteDutiesSelectedAnswer, IncompleteDutiesSelectedAnswer}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.NdrcDetails
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.form.{DutiesSelected, Duty}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{DraftClaim, DutiesSelectedAnswer, Error, SessionData, TaxCode, upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SelectDutiesController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val config: Configuration,
  selectDutiesPage: pages.select_duties
)(implicit ec: ExecutionContext, viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def withSelectedDutiesAnswer(
    f: (
      SessionData,
      FillingOutClaim,
      DutiesSelectedAnswer
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)
            )
          ) =>
        val maybeDutiesSelectedAnswer = draftClaim.fold(
          _.dutiesSelectedAnswer
        )
        maybeDutiesSelectedAnswer.fold[Future[Result]](
          f(sessionData, fillingOutClaim, IncompleteDutiesSelectedAnswer.empty)
        )(f(sessionData, fillingOutClaim, _))
      case _ => Redirect(baseRoutes.StartController.start())
    }

  private def duties(fillingOutClaim: FillingOutClaim): List[Duty] =
    fillingOutClaim.draftClaim.fold(_.displayDeclaration) match {
      case Some(displayDeclaration) =>
        displayDeclaration.displayResponseDetail.ndrcDetails match {
          case Some(ndrcDetails) => makeBlankForm(ndrcDetails).duties
          case None              => List.empty
        }
      case None                     => List.empty
    }

  def selectDuties(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSelectedDutiesAnswer { (_, fillingOutClaim, answers) =>
        answers.fold(
          ifIncomplete =>
            ifIncomplete.maybeDutiesSelected match {
              case Some(dutiesSelected) =>
                Ok(
                  selectDutiesPage(
                    SelectDutiesController
                      .selectDutiesForm(DutiesSelected(duties(fillingOutClaim)))
                      .fill(dutiesSelected),
                    duties(fillingOutClaim)
                  )
                )
              case None                 =>
                val ndrcDetails: List[NdrcDetails] = fillingOutClaim.draftClaim.fold(_.displayDeclaration) match {
                  case Some(displayDeclaration) =>
                    displayDeclaration.displayResponseDetail.ndrcDetails match {
                      case Some(ndrcDetails) => ndrcDetails
                      case None              => List.empty
                    }
                  case None                     => List.empty
                }

                if (ndrcDetails.nonEmpty) {
                  Ok(
                    selectDutiesPage(
                      SelectDutiesController
                        .selectDutiesForm(makeBlankForm(ndrcDetails))
                        .fill(DutiesSelected(List.empty)),
                      duties(fillingOutClaim)
                    )
                  )
                } else {
                  logger.warn(
                    s"ACC-14 response returned no NDRC details: Num of ndrc entries: ${ndrcDetails.size} | Ndrc tax codes: ${ndrcDetails
                      .map(s => s.taxType)
                      .mkString("|")}"
                  )
                  Redirect(baseRoutes.IneligibleController.ineligible())
                }
            },
          ifComplete => {
            println(s"I am here and the duties selected is :${ifComplete.dutiesSelected.duties.toString()}")
            Ok(
              selectDutiesPage(
                SelectDutiesController
                  .selectDutiesForm(DutiesSelected(duties(fillingOutClaim)))
                  .fill(ifComplete.dutiesSelected),
                duties(fillingOutClaim)
              )
            )
          }
        )
      }
    }

  //TODO: need to check if they changed the selection - if so trash what they renoved or update if they have added
  def selectDutiesSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSelectedDutiesAnswer { (_, fillingOutClaim, currentAnswers) =>
        SelectDutiesController
          .selectDutiesForm(DutiesSelected(duties(fillingOutClaim)))
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                selectDutiesPage(
                  requestFormWithErrors
                    .fill(
                      DutiesSelected(
                        currentAnswers.fold(
                          _.maybeDutiesSelected.getOrElse(DutiesSelected(List.empty)).duties,
                          _.dutiesSelected.duties
                        )
                      )
                    ),
                  duties(fillingOutClaim)
                )
              ),
            dutiesSelected => {

              println(s"duties selected are: ${dutiesSelected.duties.toString()}")
              val updatedAnswers = currentAnswers.fold(
                _ =>
                  CompleteDutiesSelectedAnswer(
                    dutiesSelected
                  ),
                complete => complete.copy(dutiesSelected = dutiesSelected)
              )

              val newDraftClaim =
                fillingOutClaim.draftClaim.fold(_.copy(dutiesSelectedAnswer = Some(updatedAnswers)))

              val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

              val result = EitherT
                .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                .leftMap((_: Unit) => Error("could not update session"))

              result.fold(
                e => {
                  logger.warn("could not get duties selected ", e)
                  errorHandler.errorResult()
                },
                _ => Redirect(routes.EnterClaimController.startClaim())
              )
            }
          )
      }
    }

}

object SelectDutiesController {

  private def makeBlankForm(ndrcDetails: List[NdrcDetails]): DutiesSelected = {
    val duties: List[Duty] = ndrcDetails.map { n =>
      TaxCode.fromString(n.taxType) match {
        case Some(taxCode) =>
          Duty(taxCode)
        case None          => sys.error(s"invalid data received from ACC-14: NDRC details tax type not recognised: ${n.taxType}")
      }
    }
    DutiesSelected(
      duties
    )
  }

  def selectDutiesForm(dutiesSelected: DutiesSelected): Form[DutiesSelected] = Form(
    mapping(
      "select-duties" -> list(
        mapping(
          "" -> number
            .verifying(
              "invalid tax code",
              code =>
                code === 0 ||
                  code === 1 ||
                  code === 2 ||
                  code === 3 ||
                  code === 4 ||
                  code === 5 ||
                  code === 6 ||
                  code === 7 ||
                  code === 8 ||
                  code === 9 ||
                  code === 10 ||
                  code === 11 ||
                  code === 12 ||
                  code === 13
            )
            .transform[TaxCode](
              (x: Int) => {
                println(s"index is $x")
                println(s"duties are: ${dutiesSelected.duties.toString()}")
                dutiesSelected.duties(x).taxCode
              },
              (t: TaxCode) => {
                println(s"tax codei is $t")
                dutiesSelected.duties.indexOf(Duty(t))
              }
            )
        )(Duty.apply)(Duty.unapply)
      ).verifying("error.required", _.nonEmpty)
    )(DutiesSelected.apply)(DutiesSelected.unapply)
  )

}
