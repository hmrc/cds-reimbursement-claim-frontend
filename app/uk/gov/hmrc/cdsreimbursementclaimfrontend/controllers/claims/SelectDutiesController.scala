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
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.SelectDutiesController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.BasisOfClaimAnswer.IncorrectExciseValue
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DutiesSelectedAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class SelectDutiesController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration,
  selectDutiesPage: pages.select_duties
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  implicit val dataExtractor: DraftClaim => Option[DutiesSelectedAnswer] = _.dutiesSelectedAnswer

  def selectDuties(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[DutiesSelectedAnswer] { (fillingOutClaim, previousAnswer) =>
        val cmaEligibleDutiesMap: CmaEligibleAndDuties = getAvailableDuties(fillingOutClaim)

        cmaEligibleDutiesMap.dutiesSelectedAnswer.fold(
          error => {
            logger.warn("No Available duties: ", error)
            Redirect(baseRoutes.IneligibleController.ineligible())
          },
          dutiesAvailable => {
            val emptyForm  = selectDutiesForm(dutiesAvailable)
            val filledForm = previousAnswer.fold(emptyForm)(emptyForm.fill)
            Ok(selectDutiesPage(filledForm, dutiesAvailable, cmaEligibleDutiesMap.isCmaEligible))
          }
        )
      }
    }

  def selectDutiesSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withAnswers[DutiesSelectedAnswer] { (fillingOutClaim, _) =>
        val cmaEligibleDutiesMap: CmaEligibleAndDuties = getAvailableDuties(fillingOutClaim)

        cmaEligibleDutiesMap.dutiesSelectedAnswer.fold(
          error => {
            logger.warn("No Available duties: ", error)
            Redirect(baseRoutes.IneligibleController.ineligible())
          },
          dutiesAvailable =>
            selectDutiesForm(dutiesAvailable)
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(selectDutiesPage(formWithErrors, dutiesAvailable, cmaEligibleDutiesMap.isCmaEligible)),
                dutiesSelected => {
                  val newDraftClaim  =
                    fillingOutClaim.draftClaim.copy(dutiesSelectedAnswer = Some(dutiesSelected))
                  val updatedJourney = fillingOutClaim.copy(draftClaim = newDraftClaim)

                  EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                    .leftMap(_ => Error("could not update session"))
                    .fold(
                      logAndDisplayError("could not get duties selected "),
                      _ => Redirect(routes.EnterSingleClaimController.startClaim())
                    )
                }
              )
        )
      }
    }

}

object SelectDutiesController {

  final case class CmaEligibleAndDuties(
    isCmaEligible: Seq[Boolean],
    dutiesSelectedAnswer: Either[Error, DutiesSelectedAnswer]
  )

  def getAvailableDuties(fillingOutClaim: FillingOutClaim): CmaEligibleAndDuties = {
    val wasIncorrectExciseCodeSelected: Boolean =
      fillingOutClaim.draftClaim.basisOfClaimAnswer.exists(_ === IncorrectExciseValue)

    val ndrcDetails =
      fillingOutClaim.draftClaim.displayDeclaration.flatMap(_.displayResponseDetail.ndrcDetails)

    val acc14TaxCodes = ndrcDetails
      .map(_.map(n => TaxCodes.find(n.taxType)).flatten(Option.option2Iterable))
      .getOrElse(Nil)

    val isCmaEligible = ndrcDetails
      .getOrElse(Nil)
      .map(_.cmaEligible.getOrElse("0") === "1")

    if (wasIncorrectExciseCodeSelected) {
      val receivedExciseCodes = acc14TaxCodes.intersect(TaxCodes.excise).map(Duty(_))
      CmaEligibleAndDuties(
        isCmaEligible,
        DutiesSelectedAnswer(receivedExciseCodes).toRight(Error("No excise tax codes were received from Acc14"))
      )
    } else {
      CmaEligibleAndDuties(
        isCmaEligible,
        DutiesSelectedAnswer(acc14TaxCodes.map(Duty(_)))
          .toRight(Error("No UK or EU tax codes were received from Acc14"))
      )
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def selectDutiesForm(allAvailableDuties: DutiesSelectedAnswer): Form[DutiesSelectedAnswer] = Form(
    mapping(
      "select-duties" -> list(
        mapping(
          "" -> nonEmptyText
            .verifying(
              "invalid tax code",
              code => allAvailableDuties.map(_.taxCode.value).exists(_ === code)
            )
            .transform[TaxCode](
              (x: String) => TaxCodes.findUnsafe(x),
              (t: TaxCode) => t.value
            )
        )(Duty.apply)(Duty.unapply)
      ).verifying("error.required", _.nonEmpty)
    )(taxCodes => DutiesSelectedAnswer(taxCodes.head, taxCodes.tail: _*))(dsa => Some(dsa.toList))
  )

}
