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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Call
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.Forms.eoriNumberForm
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.Eori
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.{claims => pages}

import scala.concurrent.ExecutionContext
import scala.language.postfixOps

@Singleton
class EnterDeclarantEoriNumberController @Inject() (
  val jcc: JourneyControllerComponents,
  enterDeclarantEoriNumberPage: pages.enter_declarant_eori_number
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends SecuritiesJourneyBaseController {

  val formKey: String  = "enter-declarant-eori-number"
  val postAction: Call = routes.EnterDeclarantEoriNumberController.submit()

  val show: Action[AnyContent] = actionReadJourney { implicit request => journey =>
    Ok(
      enterDeclarantEoriNumberPage(eoriNumberForm(formKey).withDefault(journey.answers.consigneeEoriNumber), postAction)
    ).asFuture
  }

  val submit: Action[AnyContent] = actionReadWriteJourney { implicit request => journey =>
    eoriNumberForm(formKey)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          journey -> BadRequest(enterDeclarantEoriNumberPage(formWithErrors.fill(Eori("")), postAction)) asFuture,
        eori =>
          journey
            .submitDeclarantEoriNumber(eori)
            .fold(
              e => {
                logger
                  .error(s"$eori] does not match EORI associated with MRN [${journey.getDeclarantEoriFromACC14}]: $e")
                journey -> Redirect(controllers.routes.IneligibleController.ineligible())
              },
              updatedJourney => updatedJourney -> Ok("Call TPI04 [DeclarantEoriNumber]]") //TODO: CDSR-1770
            )
            .asFuture
      )
  }

}
