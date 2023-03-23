/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple

import cats.data.EitherT
import cats.data.NonEmptyList
import cats.implicits.catsSyntaxEq
import cats.instances.future.catsStdInstancesForFuture
import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.cache.SessionCache
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ErrorHandler
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.overpaymentsmultiple.SelectMultipleDutiesController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionDataExtractor
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.BasisOfOverpaymentClaim.IncorrectExciseValue
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.declaration.DisplayDeclaration
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.AssociatedMrnIndex
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ClaimedReimbursement
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCode
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class SelectMultipleDutiesController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionCache,
  val controllerComponents: MessagesControllerComponents,
  selectMultipleDutiesPage: pages.select_multiple_duties,
  mrnDoesNotExistPage: pages.mrn_does_not_exist
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig, errorHandler: ErrorHandler)
    extends FrontendBaseController
    with WithAuthAndSessionDataAction
    with Logging
    with SessionDataExtractor
    with SessionUpdates {

  def selectDuties(mrnIndex: Int): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case journey: FillingOutClaim =>
        journey.draftClaim.MRNs
          .get(mrnIndex - 1)
          .fold(BadRequest(mrnDoesNotExistPage())) { mrn =>
            val dutiesAvailableMap: DutiesAvailable = getAvailableDuties(journey, mrnIndex)
            dutiesAvailableMap.dutiesSelectedAnswer.fold(
              error => {
                logger.warn("No Available duties: ", error)
                Redirect(baseRoutes.IneligibleController.ineligible())
              },
              dutiesAvailable => {
                val dutiesSelectedAnswer: Option[DutiesSelectedAnswer] = mrnIndex match {
                  case 1 =>
                    journey.draftClaim.dutiesSelectedAnswer

                  case i =>
                    journey.draftClaim.associatedMRNsDutiesSelectedAnswer.get(i - 2)
                }

                val emptyForm  = selectDutiesForm(dutiesAvailable)
                val filledForm = dutiesSelectedAnswer.map(emptyForm.fill).getOrElse(emptyForm)

                Ok(
                  selectMultipleDutiesPage(
                    filledForm,
                    dutiesAvailable,
                    mrn,
                    mrnIndex
                  )
                )
              }
            )
          }
      }

    }

  def selectDutiesSubmit(mrnIndex: Int): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.using { case journey: FillingOutClaim =>
        journey.draftClaim.MRNs
          .get(mrnIndex - 1)
          .fold(Future.successful(BadRequest(mrnDoesNotExistPage()))) { mrn =>
            val dutiesAvailableMap: DutiesAvailable = getAvailableDuties(journey, mrnIndex)
            dutiesAvailableMap.dutiesSelectedAnswer.fold(
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
                          selectMultipleDutiesPage(
                            formWithErrors,
                            dutiesAvailable,
                            mrn,
                            mrnIndex
                          )
                        )
                      ),
                    dutiesSelected => {
                      val newDraftClaim = mrnIndex match {
                        case 1 =>
                          journey.draftClaim.copy(
                            dutiesSelectedAnswer = Some(dutiesSelected),
                            claimedReimbursementsAnswer = journey.draftClaim.claimedReimbursementsAnswer
                              .map(claims =>
                                syncSelectedDutiesWithExistingClaims(mrnIndex, dutiesSelected, claims, journey)
                              )
                          )

                        case i =>
                          val associatedMRNsDutiesSelectedAnswer =
                            journey.draftClaim.associatedMRNsDutiesSelectedAnswer
                              .replaceOrAppend(i - 2, dutiesSelected)
                              .getOrElse(journey.draftClaim.associatedMRNsDutiesSelectedAnswer)

                          val associatedMRNsClaimsAnswer = for {
                            selectedDuties <- associatedMRNsDutiesSelectedAnswer
                            existingClaims <- journey.draftClaim.associatedMRNsClaimsAnswer
                          } yield selectedDuties
                            .zipWith(existingClaims)((d, c) =>
                              syncSelectedDutiesWithExistingClaims(mrnIndex, d, c, journey)
                            )

                          journey.draftClaim
                            .copy(
                              associatedMRNsDutiesSelectedAnswer = associatedMRNsDutiesSelectedAnswer,
                              associatedMRNsClaimsAnswer = associatedMRNsClaimsAnswer
                            )
                      }

                      val updatedJourney: FillingOutClaim = journey.copy(draftClaim = newDraftClaim)

                      EitherT
                        .liftF(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
                        .leftMap((_: Unit) => Error("could not update session"))
                        .fold(
                          logAndDisplayError("could not get duties selected "),
                          _ =>
                            Redirect(
                              SelectMultipleDutiesController
                                .selectNextPage(updatedJourney, mrnIndex)
                            )
                        )
                    }
                  )
            )
          }
      }
    }

}

object SelectMultipleDutiesController {

  def selectNextPage(fillingOutClaim: FillingOutClaim, mrnIndex: Int): Call =
    (for {
      duties <- fillingOutClaim.draftClaim.DutiesSelections.get(mrnIndex - 1)
      duty   <- duties.headOption
    } yield routes.EnterMultipleClaimsController.enterClaim(mrnIndex, duty.taxCode))
      .getOrElse(
        routes.SelectMultipleDutiesController.selectDuties(mrnIndex)
      )

  final case class DutiesAvailable(
    dutiesSelectedAnswer: Either[Error, DutiesSelectedAnswer]
  )

  def getAvailableDuties(fillingOutClaim: FillingOutClaim, mrnIndex: Int): DutiesAvailable = {

    val displayDeclaration: Option[DisplayDeclaration] = mrnIndex match {
      case 1          => fillingOutClaim.draftClaim.displayDeclaration
      case i if i > 1 =>
        fillingOutClaim.draftClaim.associatedMRNsDeclarationAnswer.get(AssociatedMrnIndex.fromUrlIndex(i))
      case _          => None
    }

    val wasIncorrectExciseCodeSelected: Boolean =
      fillingOutClaim.draftClaim.basisOfClaimAnswer.exists(_ === IncorrectExciseValue)

    val ndrcDetails =
      displayDeclaration.flatMap(_.displayResponseDetail.ndrcDetails)

    val acc14TaxCodes = ndrcDetails
      .map(_.map(n => TaxCodes.find(n.taxType)).flatten(Option.option2Iterable))
      .getOrElse(Nil)

    if (wasIncorrectExciseCodeSelected) {
      val receivedExciseCodes = acc14TaxCodes.intersect(TaxCodes.excise).map(Duty(_))
      DutiesAvailable(
        DutiesSelectedAnswer(receivedExciseCodes).toRight(Error("No excise tax codes were received from Acc14"))
      )
    } else {
      DutiesAvailable(
        DutiesSelectedAnswer(acc14TaxCodes.map(Duty(_)))
          .toRight(Error("No UK or EU tax codes were received from Acc14"))
      )
    }
  }

  def syncSelectedDutiesWithExistingClaims(
    mrnIndex: Int,
    selectedDuties: NonEmptyList[Duty],
    existingClaims: NonEmptyList[ClaimedReimbursement],
    journey: FillingOutClaim
  ): NonEmptyList[ClaimedReimbursement] =
    NonEmptyList.fromListUnsafe(
      EnterMultipleClaimsController.prepareClaims(mrnIndex - 1, selectedDuties.toList, existingClaims.toList, journey)
    )

  @SuppressWarnings(Array("org.wartremover.warts.IterableOps"))
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
