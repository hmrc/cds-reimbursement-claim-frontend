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
import cats.implicits.{catsSyntaxEq, toBifunctorOps}
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Forms._
import play.api.data.format.Formatter
import play.api.data.{Form, FormError, Forms}
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.{FormUtils, Logging}
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
                    SelectDutiesController.selectDutiesForm.fill(dutiesSelected),
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
                      SelectDutiesController.selectDutiesForm.fill(makeBlankForm(ndrcDetails)),
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
          ifComplete =>
            Ok(
              selectDutiesPage(
                SelectDutiesController.selectDutiesForm.fill(ifComplete.dutiesSelected),
                duties(fillingOutClaim)
              )
            )
        )
      }
    }

  def selectDutiesSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSelectedDutiesAnswer { (_, fillingOutClaim, answers) =>
        SelectDutiesController.selectDutiesForm
          .bindFromRequest()
          .fold(
            requestFormWithErrors =>
              BadRequest(
                selectDutiesPage(
                  requestFormWithErrors
                    .fill(
                      DutiesSelected(
                        answers.fold(
                          _.maybeDutiesSelected.getOrElse(DutiesSelected(List.empty)).duties,
                          _.dutiesSelected.duties
                        )
                      )
                    ),
                  duties(fillingOutClaim)
                )
              ),
            dutiesSelected => {
              val updatedAnswers = answers.fold(
                _ =>
                  CompleteDutiesSelectedAnswer(
                    dutiesSelected
                  ),
                complete => complete.copy(dutiesSelected = dutiesSelected)
              )
              val newDraftClaim  =
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

  val assetTypeForNonUkResidentsForm: Form[List[TaxCode]] = {
    val checkBoxAssetTypeFormFormatter: Formatter[TaxCode] =
      new Formatter[TaxCode] {

        override def bind(
          key: String,
          data: Map[String, String]
        ): Either[Seq[FormError], TaxCode] =
          FormUtils
            .readValue(key, data, identity)
            .flatMap {
              case "0"  => Right(TaxCode.A00)
              case "1"  => Right(TaxCode.A20)
              case "2"  => Right(TaxCode.A30)
              case "3"  => Right(TaxCode.A35)
              case "4"  => Right(TaxCode.A40)
              case "5"  => Right(TaxCode.A45)
              case "6"  => Right(TaxCode.B00)
              case "7"  => Right(TaxCode.A50)
              case "8"  => Right(TaxCode.A70)
              case "9"  => Right(TaxCode.A80)
              case "10" => Right(TaxCode.A85)
              case "11" => Right(TaxCode.A90)
              case "12" => Right(TaxCode.A95)
              case "13" => Right(TaxCode.B05)
              case _    => Left(FormError(key, "error.invalid"))
            }
            .leftMap(Seq(_))

        override def unbind(
          key: String,
          value: TaxCode
        ): Map[String, String] =
          Map(key -> value.toString)
      }

    Form(
      mapping(
        "select-duties" -> Forms
          .list(of(checkBoxAssetTypeFormFormatter))
          .verifying("error.required", _.nonEmpty)
      )(identity)(Some(_))
    )
  }

  val selectDutiesForm: Form[DutiesSelected] = Form(
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
              {
                case 0  => TaxCode.A00
                case 1  => TaxCode.A20
                case 2  => TaxCode.A30
                case 3  => TaxCode.A35
                case 4  => TaxCode.A40
                case 5  => TaxCode.A45
                case 6  => TaxCode.B00
                case 7  => TaxCode.A50
                case 8  => TaxCode.A70
                case 9  => TaxCode.A80
                case 10 => TaxCode.A85
                case 11 => TaxCode.A90
                case 12 => TaxCode.A95
                case 13 => TaxCode.B05
              },
              {
                case TaxCode.A00 => 0
                case TaxCode.A20 => 1
                case TaxCode.A30 => 2
                case TaxCode.A35 => 3
                case TaxCode.A40 => 4
                case TaxCode.A45 => 5
                case TaxCode.B00 => 6
                case TaxCode.A50 => 7
                case TaxCode.A70 => 8
                case TaxCode.A80 => 9
                case TaxCode.A85 => 10
                case TaxCode.A90 => 11
                case TaxCode.A95 => 12
                case TaxCode.B05 => 13
              }
            )
        )(Duty.apply)(Duty.unapply)
      ).verifying("error.required", _.nonEmpty)
    )(DutiesSelected.apply)(DutiesSelected.unapply)
  )

}
