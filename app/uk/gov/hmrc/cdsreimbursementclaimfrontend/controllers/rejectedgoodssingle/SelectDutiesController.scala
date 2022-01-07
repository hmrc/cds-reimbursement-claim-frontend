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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle

import javax.inject.Inject
import javax.inject.Singleton
import cats.implicits.catsSyntaxEq
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.selectDutiesForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.{routes => baseRoutes}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.SelectDutiesController.CmaEligibleAndDuties
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodssingle.SelectDutiesController._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.journeys.RejectedGoodsSingleJourney
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Duty
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Error
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.TaxCodes
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.answers.DutiesSelectedAnswer
import uk.gov.hmrc.cdsreimbursementclaimfrontend.util.toFuture
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging._
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class SelectDutiesController @Inject() (
  val jcc: JourneyControllerComponents,
  selectDutiesPage: pages.select_duties
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController {

  def show(): Action[AnyContent] = actionReadJourney { implicit request => journey =>
    val cmaEligibleDutiesMap: CmaEligibleAndDuties = getAvailableDuties(journey)

    cmaEligibleDutiesMap.dutiesSelectedAnswer.fold(
      error => {
        logger.warn("No available duties", error)
        Redirect(baseRoutes.IneligibleController.ineligible())
      },
      dutiesAvailable =>
        Future.successful {
          val form = selectDutiesForm(dutiesAvailable)
          Ok(selectDutiesPage(form, dutiesAvailable, cmaEligibleDutiesMap.isCmaEligible))
        }
    )
  }

  def submit(): Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    val cmaEligibleDutiesMap: CmaEligibleAndDuties = getAvailableDuties(journey)

    cmaEligibleDutiesMap.dutiesSelectedAnswer.fold(
      error => {
        logger.warn("No Available duties: ", error)
        Future.successful((journey, Redirect(baseRoutes.IneligibleController.ineligible())))
      },
      dutiesAvailable =>
        selectDutiesForm(dutiesAvailable)
          .bindFromRequest()
          .fold(
            formWithErrors =>
              Future.successful(
                (
                  journey,
                  BadRequest(selectDutiesPage(formWithErrors, dutiesAvailable, cmaEligibleDutiesMap.isCmaEligible))
                )
              ),
            dutiesSelected =>
              Future.successful(
                (
                  journey
                    .selectAndReplaceTaxCodeSetForReimbursement(dutiesSelected.toList.map(_.taxCode))
                    .getOrElse(journey),
                  Redirect("enter-claim")
                )
              )
          )
    )

  }
}

object SelectDutiesController {

  final case class CmaEligibleAndDuties(
    isCmaEligible: Seq[Boolean],
    dutiesSelectedAnswer: Either[Error, DutiesSelectedAnswer]
  )

  def getAvailableDuties(journey: RejectedGoodsSingleJourney): CmaEligibleAndDuties = {

    val ndrcDetails = journey.getNdrcDetails

    val acc14TaxCodes = ndrcDetails
      .map(_.map(n => TaxCodes.find(n.taxType)).flatten(Option.option2Iterable))
      .getOrElse(Nil)

    val isCmaEligible = ndrcDetails
      .getOrElse(Nil)
      .map(_.cmaEligible.getOrElse("0") === "1")

    CmaEligibleAndDuties(
      isCmaEligible,
      DutiesSelectedAnswer(acc14TaxCodes.map(Duty(_)))
        .toRight(Error("No UK or EU tax codes were received from Acc14"))
    )

  }

}
