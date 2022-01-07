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

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.RejectedGoodsSingleClaimConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => claimPages}

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

@Singleton
class CheckYourAnswersController @Inject() (
  val jcc: JourneyControllerComponents,
  rejectedGoodsSingleClaimConnector: RejectedGoodsSingleClaimConnector,
  checkYourAnswersPage: pages.check_your_answers,
  confirmationOfSubmissionPage: claimPages.confirmation_of_submission,
  submitClaimFailedPage: claimPages.submit_claim_error
)(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
    extends RejectedGoodsSingleJourneyBaseController
    with Logging
    with RejectedGoodsSingleJourneyRouter {

  private val postAction: Call             = routes.CheckYourAnswersController.submit()
  private val showConfirmationAction: Call = routes.CheckYourAnswersController.showConfirmation()

  val show: Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      journey.toOutput
        .fold(
          errors => Redirect(routeForValidationErrors(errors)),
          output => Ok(checkYourAnswersPage(output, postAction))
        )
        .asFuture
    }

  val submit: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      if (journey.isFinalized)
        (journey, Redirect(showConfirmationAction)).asFuture
      else
        journey.toOutput
          .fold(
            errors => (journey, Redirect(routeForValidationErrors(errors))).asFuture,
            output =>
              rejectedGoodsSingleClaimConnector
                .submitClaim(RejectedGoodsSingleClaimConnector.Request(output))
                .map { response =>
                  (
                    journey.finalizeJourneyWith(response.caseNumber).getOrElse(journey),
                    Redirect(showConfirmationAction)
                  )
                }
                .recover { case NonFatal(_) =>
                  (journey, Ok(submitClaimFailedPage()))
                }
          )
    }

  val showConfirmation: Action[AnyContent] =
    jcc
      .authenticatedActionWithSessionData(requiredFeature)
      .async { implicit request =>
        request.sessionData
          .flatMap(getJourney)
          .map(journey =>
            (journey.caseNumber match {
              case Some(caseNumber) => Ok(confirmationOfSubmissionPage(journey.getTotalReimbursementAmount, caseNumber))
              case None             => Redirect(routes.CheckYourAnswersController.show())
            }).asFuture
          )
          .getOrElse(redirectToTheStartOfTheJourney)
      }
}
