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
import cats.data.NonEmptyList
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
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.claims.MultipleSelectDutiesController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfClaim.IncorrectExciseValue
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Duty

@Singleton
class MultipleSelectDutiesController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  cc: MessagesControllerComponents,
  val config: Configuration,
  selectDutiesPage: pages.multiple_select_duties,
  mrnDoesNotExistPage: pages.mrn_does_not_exist
)(implicit ec: ExecutionContext, viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  def selectDuties(index: AssociatedMrnIndex): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case journey: FillingOutClaim =>
        journey.draftClaim.associatedMRNsAnswer
          .get(index)
          .fold(BadRequest(mrnDoesNotExistPage())) { mrn =>
            val cmaEligibleDutiesMap: CmaEligibleAndDuties = getAvailableDuties(journey)

            cmaEligibleDutiesMap.dutiesSelectedAnswer.fold(
              error => {
                logger.warn("No Available duties: ", error)
                Redirect(baseRoutes.IneligibleController.ineligible())
              },
              dutiesAvailable => {
                val emptyForm  = selectDutiesForm(dutiesAvailable)
                val filledForm = journey.draftClaim.associatedMRNsAnswer.fold(emptyForm)(_ => emptyForm)
                Ok(selectDutiesPage(filledForm, dutiesAvailable, cmaEligibleDutiesMap.isCmaEligible, index, mrn))
              }
            )
          }
      }

    }

  def selectDutiesSubmit(index: AssociatedMrnIndex): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case journey: FillingOutClaim =>
        journey.draftClaim.associatedMRNsAnswer
          .get(index)
          .fold(Future.successful(BadRequest(mrnDoesNotExistPage()))) { mrn =>
            val cmaEligibleDutiesMap: CmaEligibleAndDuties = getAvailableDuties(journey)

            cmaEligibleDutiesMap.dutiesSelectedAnswer.fold(
              error => {
                logger.warn("No Available duties: ", error)
                Future.successful(Redirect(baseRoutes.IneligibleController.ineligible()))
              },
              dutiesAvailable =>
                selectDutiesForm(dutiesAvailable)
                  .bindFromRequest()
                  .fold(
                    formWithErrors =>
                      Future.successful(
                        BadRequest(
                          selectDutiesPage(
                            formWithErrors,
                            dutiesAvailable,
                            cmaEligibleDutiesMap.isCmaEligible,
                            index,
                            mrn
                          )
                        )
                      ),
                    dutiesSelected => {
                      val newDraftClaim =
                        journey.draftClaim.copy(multipleDutiesSelectedAnswer =
                          Some(
                            NonEmptyList(
                              (mrn, dutiesSelected),
                              journey.draftClaim.multipleDutiesSelectedAnswer
                                .map(_.toList.filterNot(_._1 == mrn))
                                .getOrElse(Nil)
                            )
                          )
                        )

                      val updatedJourney = journey.copy(draftClaim = newDraftClaim)

                      EitherT
                        .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                        .leftMap((_: Unit) => Error("could not update session"))
                        .fold(
                          logAndDisplayError("could not get duties selected "),
                          _ => Redirect(routes.EnterClaimController.startClaim())
                        )
                    }
                  )
            )
          }
      }
    }

}

object MultipleSelectDutiesController {

  final case class CmaEligibleAndDuties(
    isCmaEligible: Seq[Boolean],
    dutiesSelectedAnswer: Either[Error, DutiesSelectedAnswer]
  )

  def getAvailableDuties(fillingOutClaim: FillingOutClaim): CmaEligibleAndDuties = {
    val wasIncorrectExciseCodeSelected: Boolean = fillingOutClaim.draftClaim.basisOfClaimAnswer
      .exists(_ === IncorrectExciseValue)

    val ndrcDetails = fillingOutClaim.draftClaim.displayDeclaration
      .flatMap(_.displayResponseDetail.ndrcDetails)

    val acc14TaxCodes = ndrcDetails
      .map(_.map(n => TaxCode.fromString(n.taxType)).flatten(Option.option2Iterable))
      .getOrElse(Nil)

    val isCmaEligible = ndrcDetails
      .getOrElse(Nil)
      .map(_.cmaEligible.getOrElse("0") === "1")

    if (wasIncorrectExciseCodeSelected) {
      val receivedExciseCodes = acc14TaxCodes.intersect(TaxCode.listOfUKExciseCodes).map(Duty(_))
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
              (x: String) => TaxCode.allTaxCodesMap(x),
              (t: TaxCode) => t.value
            )
        )(Duty.apply)(Duty.unapply)
      ).verifying("error.required", _.nonEmpty)
    )(taxCodes => DutiesSelectedAnswer(taxCodes.head, taxCodes.tail: _*))(dsa => Some(dsa.toList))
  )

}
