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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.testonly.controllers

import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.connectors.DeclarationConnector
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.JourneyControllerComponents
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SecuritiesJourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ReasonForSecurity
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.ids.MRN

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

// This controller will be deleted at the end of CDSR-1781

@Singleton
class TestController @Inject() (
  connector: DeclarationConnector,
  val jcc: JourneyControllerComponents
)(implicit val ec: ExecutionContext, val viewConfig: ViewConfig)
    extends SecuritiesJourneyBaseController {

  def testIsDuplicate(mrn: String, reason: String): Action[AnyContent] = Action.async { implicit request =>
    connector
      .getIsDuplicate(MRN(mrn), ReasonForSecurity.find(reason).getOrElse(ReasonForSecurity.AccountSales))
      .fold(
        error => Ok(s"We got an error $error"),
        result => Ok(s"We got a result $result")
      )
  }
}
