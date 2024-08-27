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

package uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers

import com.google.inject.Inject
import com.google.inject.Singleton
import play.api.mvc.Action
import play.api.mvc.AnyContent
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.EnrolmentConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.config.ViewConfig
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.SessionDataActionWithRetrievedData
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.actions.WithAuthRetrievalsAndSessionDataAction
import uk.gov.hmrc.cdsreimbursementclaimfrontend.controllers.securities.SecuritiesJourneyBaseController
import uk.gov.hmrc.cdsreimbursementclaimfrontend.models.Feature
import uk.gov.hmrc.cdsreimbursementclaimfrontend.services.FeatureSwitchService
import uk.gov.hmrc.cdsreimbursementclaimfrontend.utils.Logging
import uk.gov.hmrc.cdsreimbursementclaimfrontend.views.html.landing_page_securities

import scala.concurrent.ExecutionContext

@Singleton
class LandingPageSecuritiesController @Inject() (
  val authenticatedActionWithRetrievedData: AuthenticatedActionWithRetrievedData,
  val sessionDataActionWithRetrievedData: SessionDataActionWithRetrievedData,
  val jcc: JourneyControllerComponents,
  val featureSwitchService: FeatureSwitchService,
  landingPageSecurities: landing_page_securities
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends SecuritiesJourneyBaseController
    with WithAuthRetrievalsAndSessionDataAction
    with SessionUpdates
    with Logging {

  private val securitiesAccessEoriSet =
    EnrolmentConfig.getLimitedAccessEoriSet(viewConfig.config)

  final val showLandingPageSecurities: Action[AnyContent] =
    authenticatedActionWithRetrievedDataAndSessionData { implicit request =>
      val userIsAuthorisedSecuritiesLimitedAccess =
        request.authenticatedRequest.journeyUserType.eoriOpt.exists(securitiesAccessEoriSet.contains)

      if (featureSwitchService.isEnabled(Feature.Securities)) {
        if (featureSwitchService.isEnabled(Feature.LimitedAccessSecurities)) {
          if (userIsAuthorisedSecuritiesLimitedAccess)
            Ok(landingPageSecurities())
          else Redirect(routes.UnauthorisedController.unauthorised())
        } else {
          Ok(landingPageSecurities())
        }
      } else Redirect(routes.UnauthorisedController.unauthorised())
    }
}
