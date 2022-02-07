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
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.chooseHowManyMrnsForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{rejectedgoodssingle => pages}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@Singleton
class ChooseHowManyMrnsController @Inject() (
val jcc: JourneyControllerComponents,
chooseHowManyMrnsPage: pages.choose_how_many_mrns
  )(implicit val ec: ExecutionContext, viewConfig: ViewConfig)
  extends RejectedGoodsSingleJourneyBaseController {

  val formKey: String          = "rejected-goods.choose-how-many-mrns"
  private val postAction: Call = routes.ChooseHowManyMrnsController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
  Future.successful {
  val form = chooseHowManyMrnsForm

  Ok(chooseHowManyMrnsPage(form, postAction))
  }
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
  Future.successful(
  form
  .bindFromRequest()
  .fold(
  formWithErrors =>
  (
  journey,
  BadRequest(
    chooseHowManyMrnsPage(
  formWithErrors,
  postAction
  )
  )
  ),
    journeyType =>
  (
  journey.[SUBMIT METHOD](journeyType),
  Redirect(routes.EnterMovementReferenceNumberController.show()) //FIXME
  )
  )
  )
  }

  }