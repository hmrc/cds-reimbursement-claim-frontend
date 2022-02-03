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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsmultiple

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging

import scala.concurrent.ExecutionContext

@Singleton
class WorkInProgressController @Inject() (
  val jcc: JourneyControllerComponents
)(implicit val ec: ExecutionContext)
    extends RejectedGoodsMultipleJourneyBaseController
    with Logging {

  val show: Action[AnyContent] =
    simpleActionReadJourney { _ =>
      Ok("Work in progress ...")
    }

  val submit: Action[AnyContent] =
    actionReadWriteJourney { _ => journey =>
      (journey, Redirect(routes.WorkInProgressController.show)).asFuture
    }
}
