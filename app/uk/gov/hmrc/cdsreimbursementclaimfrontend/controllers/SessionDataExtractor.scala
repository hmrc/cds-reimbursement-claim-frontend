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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import play.api.mvc.{Result, Results}
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.DraftClaim.DraftC285Claim
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.JourneyStatus.FillingOutClaim

import scala.concurrent.Future

trait SessionDataExtractor extends Results {
  def withAnswers[T](
    f: (FillingOutClaim, Option[T]) => Future[Result]
  )(implicit extractor: DraftC285Claim => Option[T], request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(fillingOutClaim @ FillingOutClaim(_, _, draftClaim: DraftClaim)) =>
        draftClaim
          .fold(extractor(_))
          .fold[Future[Result]](f(fillingOutClaim, None))(data => f(fillingOutClaim, Option(data)))
      case _                                                                     =>
        Future.successful(Redirect(routes.StartController.start()))
    }

}
