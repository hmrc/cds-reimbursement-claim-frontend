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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.rejectedgoodsingle

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.data._
import play.api.mvc._
import play.twirl.api.Html
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.{upscan => _}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

/** Dummy controller to showcase different patterns of implementing actions. */
@Singleton
class DummyExampleController @Inject() (
  val jcc: JourneyControllerComponents
)(implicit ec: ExecutionContext)
    extends RejectedGoodsSingleJourneyBaseController {

  // dummy API example
  def someApiCall(implicit hc: HeaderCarrier): Future[String] = ???
  def dummyPage(form: Form[String]): Html                     = ???

  val dummyForm: Form[String] = Form("text" -> Forms.text)

  /** A pattern of a simple GET action to show page based on the journey state */
  val showDummyPage: Action[AnyContent] =
    simpleActionReadJourney { journey =>
      journey.answers.detailsOfRejectedGoods match {
        case Some(s) => Ok(dummyPage(dummyForm.fill(s)))
        case None    => Ok(dummyPage(dummyForm))
      }
    }

  /** A pattern of a GET action calling an API and then showing page based on the response and journey state */
  val callApiAndShowDummyPage: Action[AnyContent] =
    actionReadJourney { implicit request => journey =>
      journey.answers.detailsOfRejectedGoods match {
        case Some(s) => someApiCall.map(_ => Ok(dummyPage(dummyForm.fill(s))))
        case None    => someApiCall.map(_ => Ok(dummyPage(dummyForm)))
      }
    }

  /** A pattern of a POST action processing payload and potentially modifying the journey. */
  val submitDummy: Action[AnyContent] =
    simpleActionReadWriteJourney { implicit request => journey =>
      dummyForm
        .bindFromRequest()
        .fold(
          formWithErrors => (journey, Ok(dummyPage(formWithErrors))),
          s => (journey.submitDetailsOfRejectedGoods(s), Ok)
        )
    }

  /** A pattern of a POST action processing payload then calling an external API and potentially modifying the journey. */
  val submitDummyAndCallApi: Action[AnyContent] =
    actionReadWriteJourney { implicit request => journey =>
      dummyForm
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful((journey, Ok(dummyPage(formWithErrors)))),
          s1 => someApiCall.map(s2 => (journey.submitDetailsOfRejectedGoods(s1 + s2), Ok))
        )
    }

}
